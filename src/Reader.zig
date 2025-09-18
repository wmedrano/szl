//! Reader for parsing Scheme source code into values.
//!
//! The Reader module provides functionality to parse Scheme source code text
//! into the internal value representation used by the interpreter. It handles
//! tokenization and conversion of textual representations into typed values
//! like symbols, booleans, and eventually more complex structures like lists.

const std = @import("std");
const testing = std.testing;

const Symbol = @import("Symbol.zig");
const Tokenizer = @import("Tokenizer.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const Reader = @This();

/// The virtual machine instance used for value creation and symbol interning.
vm: *Vm,
/// The tokenizer used to break source text into tokens.
tokenizer: Tokenizer,

/// Reads exactly one value from the provided source text.
///
/// Args:
///   vm: Pointer to the virtual machine for value creation.
///   source: The source text to parse.
///
/// Returns:
///   The parsed value if successful.
///
/// Errors:
///   - NoValue: If the source contains no parseable values.
///   - TooManyValues: If the source contains more than one value.
///   - BadExpression: If the source contains malformed expressions.
pub fn readOne(vm: *Vm, source: []const u8) !Val {
    var reader = init(vm, source);
    const val = try (reader.next()) orelse return error.NoValue;
    if (try reader.next()) |_| return error.TooManyValues;
    return val;
}

/// Initializes a new Reader instance for parsing the given source text.
///
/// Creates a Reader that uses the provided virtual machine for value creation
/// and sets up a tokenizer to process the source text.
///
/// Args:
///   vm: Pointer to the virtual machine for value creation and symbol interning.
///   source: The source text to be parsed.
///
/// Returns:
///   A new Reader instance ready to parse the source text.
pub fn init(vm: *Vm, source: []const u8) Reader {
    return Reader{ .vm = vm, .tokenizer = Tokenizer.init(source) };
}

/// Reads and parses the next value from the source text.
///
/// This is the main parsing function that advances through the tokenizer,
/// identifies the type of the current token, and converts it to the appropriate
/// value representation. Handles symbols, booleans, and delegates to specialized
/// functions for expressions.
///
/// Args:
///   self: Pointer to the Reader instance.
///
/// Returns:
///   The next parsed value, or null if no more tokens are available.
///
/// Errors:
///   - BadExpression: If a closing parenthesis is encountered without a matching open.
///   - Other errors from parseToken or nextExpression functions.
pub fn next(self: *Reader) !?Val {
    const token = self.tokenizer.nextText() orelse return null;
    if (std.mem.eql(u8, token, "(")) return self.nextExpression();
    if (std.mem.eql(u8, token, ")")) return error.BadExpression;
    return try self.parseToken(token);
}

/// Parses an expression starting with an opening parenthesis.
///
/// This function is called when an opening parenthesis "(" is encountered.
/// Currently returns a BadExpression error as expression parsing is not
/// yet implemented. This function serves as a placeholder for future
/// implementation of list and expression parsing.
///
/// Args:
///   self: Pointer to the Reader instance.
///
/// Returns:
///   A parsed expression value (when implemented).
///
/// Errors:
///   - BadExpression: Currently always returned as expressions are not implemented.
fn nextExpression(self: *Reader) !?Val {
    var expressions = std.ArrayList(Val){};
    defer expressions.deinit(self.vm.allocator);
    while (self.tokenizer.nextText()) |token| {
        if (std.mem.eql(u8, token, "(")) {
            const expr = (try self.nextExpression()) orelse return error.BadExpression;
            try expressions.append(self.vm.allocator, expr);
        } else if (std.mem.eql(u8, token, ")")) {
            return try self.vm.toVal(expressions.items);
        } else {
            const expr = try self.parseToken(token);
            try expressions.append(self.vm.allocator, expr);
        }
    }
    return error.BadExpression;
}

/// Parses a single token into its corresponding value.
///
/// Converts textual token representations into their appropriate value types.
/// Recognizes boolean literals (#t and #f) and treats all other tokens as symbols.
/// This function handles the core atomic value parsing for the Scheme reader.
///
/// Args:
///   self: Pointer to the Reader instance (used for VM access).
///   token: The token text to parse.
///
/// Returns:
///   The parsed value corresponding to the token.
///
/// Errors:
///   - May return errors from the VM's builder when creating symbol values.
fn parseToken(self: *Reader, token: []const u8) !Val {
    if (std.mem.eql(u8, token, "#t")) return Val.init(true);
    if (std.mem.eql(u8, token, "#f")) return Val.init(false);
    return self.vm.toVal(Symbol.init(token));
}

test "readOne with empty input returns NoValue error" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.NoValue,
        readOne(&vm, ""),
    );
}

test "readOne with single boolean true returns boolean value" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#t");
    try testing.expectEqual(
        Val.init(true),
        result,
    );
}

test "readOne with single boolean false returns boolean value" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#f");
    try testing.expectEqual(
        Val.init(false),
        result,
    );
}

test "readOne with single symbol returns symbol value" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "hello");
    try testing.expectEqual(
        try vm.toVal(Symbol.init("hello")),
        result,
    );
}

test "readOne with multiple values returns TooManyValues error" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.TooManyValues,
        readOne(&vm, "hello world"),
    );
}

test "readOne with whitespace around single value succeeds" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "   #t   ");
    try testing.expectEqual(
        Val.init(true),
        result,
    );
}

test "readOne with empty list returns empty list" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "()");
    try testing.expectFmt(
        "()",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with single element list returns proper list" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(hello)");
    try testing.expectFmt(
        "(hello)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with multiple element list returns proper list" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(a b c)");
    try testing.expectFmt(
        "(a b c)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with boolean elements in list returns proper list" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(#t #f)");
    try testing.expectFmt(
        "(#t #f)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with simple nested list returns proper nested structure" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "((a))");
    try testing.expectFmt(
        "((a))",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with multi-level nested list returns proper structure" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(((hello)))");
    try testing.expectFmt(
        "(((hello)))",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with mixed nested content returns proper structure" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(a (b c) d)");
    try testing.expectFmt(
        "(a (b c) d)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with nested empty lists returns proper structure" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(())");
    try testing.expectFmt(
        "(())",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with mixed symbols and booleans returns proper list" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(hello #t world #f)");
    try testing.expectFmt(
        "(hello #t world #f)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with complex nested mixed content returns proper structure" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(func (arg1 #t) (arg2 #f))");
    try testing.expectFmt(
        "(func (arg1 #t) (arg2 #f))",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with nested booleans and symbols returns proper structure" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "((#t hello) (#f world))");
    try testing.expectFmt(
        "((#t hello) (#f world))",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with unclosed expression returns BadExpression error" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "(hello world"),
    );
}

test "readOne with extra closing parenthesis returns BadExpression error" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "hello)"),
    );
}

test "readOne with nested unclosed expression returns BadExpression error" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "(hello (world"),
    );
}

test "readOne with mismatched parentheses returns BadExpression error" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "((hello)"),
    );
}

test "readOne with whitespace in expressions returns proper structure" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(  hello   world  )");
    try testing.expectFmt(
        "(hello world)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with nested expressions and whitespace returns proper structure" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "( a  ( b   c ) d )");
    try testing.expectFmt(
        "(a (b c) d)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with multiple expressions returns TooManyValues error" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.TooManyValues,
        readOne(&vm, "(a) (b)"),
    );
}

test "readOne with empty nested lists with whitespace returns proper structure" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "( ( ) )");
    try testing.expectFmt(
        "(())",
        "{f}",
        .{vm.pretty(result)},
    );
}
