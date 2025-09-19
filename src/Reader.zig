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
/// Handles nested expressions and builds proper list structures.
///
/// Args:
///   self: Pointer to the Reader instance.
///
/// Returns:
///   A parsed expression value representing the list.
///
/// Errors:
///   - BadExpression: If the expression is malformed or unclosed.
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
/// Recognizes boolean literals (#t and #f), integers, floating point numbers,
/// and treats all other tokens as symbols.
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
    if (std.mem.eql(u8, token, "#t") or std.mem.eql(u8, token, "#true"))
        return Val.init(true);
    if (std.mem.eql(u8, token, "#f") or std.mem.eql(u8, token, "#false"))
        return Val.init(false);
    if (std.fmt.parseInt(i64, token, 10) catch null) |x|
        return Val.init(x);
    if (std.fmt.parseFloat(f64, token) catch null) |x|
        return Val.init(x);
    return self.vm.toVal(Symbol.init(token));
}

test "readOne with empty input returns NoValue error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.NoValue,
        readOne(&vm, ""),
    );
}

test "readOne with single boolean true returns boolean value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#t");
    try testing.expectEqual(
        Val.init(true),
        result,
    );
}

test "readOne with single boolean false returns boolean value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#f");
    try testing.expectEqual(
        Val.init(false),
        result,
    );
}

test "readOne with long form boolean true returns boolean value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#true");
    try testing.expectEqual(
        Val.init(true),
        result,
    );
}

test "readOne with long form boolean false returns boolean value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#false");
    try testing.expectEqual(
        Val.init(false),
        result,
    );
}

test "readOne with single symbol returns symbol value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "hello");
    try testing.expectEqual(
        try vm.toVal(Symbol.init("hello")),
        result,
    );
}

test "readOne with multiple values returns TooManyValues error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.TooManyValues,
        readOne(&vm, "hello world"),
    );
}

test "readOne with whitespace around single value succeeds" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "   #t   ");
    try testing.expectEqual(
        Val.init(true),
        result,
    );
}

test "readOne with positive integer returns integer value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(42),
        try readOne(&vm, "42"),
    );
}

test "readOne with negative integer returns integer value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(-123),
        try readOne(&vm, "-123"),
    );
}

test "readOne with zero returns integer value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(0),
        try readOne(&vm, "0"),
    );
}

test "readOne with large integer returns integer value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(9223372036854775807),
        try readOne(&vm, "9223372036854775807"),
    );
}

test "readOne with empty list returns empty list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "()");
    try testing.expectFmt(
        "()",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with single element list returns proper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(hello)");
    try testing.expectFmt(
        "(hello)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with multiple element list returns proper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(a b c)");
    try testing.expectFmt(
        "(a b c)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with boolean elements in list returns proper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(#t #f)");
    try testing.expectFmt(
        "(#t #f)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with mixed boolean formats in list returns proper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(#t #false #true #f)");
    try testing.expectFmt(
        "(#t #f #t #f)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with simple nested list returns proper nested structure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "((a))");
    try testing.expectFmt(
        "((a))",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with multi-level nested list returns proper structure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(((hello)))");
    try testing.expectFmt(
        "(((hello)))",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with mixed nested content returns proper structure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(a (b c) d)");
    try testing.expectFmt(
        "(a (b c) d)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with nested empty lists returns proper structure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(())");
    try testing.expectFmt(
        "(())",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with mixed symbols and booleans returns proper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(hello #t world #f)");
    try testing.expectFmt(
        "(hello #t world #f)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with integers in list returns proper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(1 2 3)");
    try testing.expectFmt(
        "(1 2 3)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with mixed integers symbols and booleans returns proper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(hello 42 #t -17 world #f 0)");
    try testing.expectFmt(
        "(hello 42 #t -17 world #f 0)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with invalid integer falls back to symbol" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        try vm.toVal(Symbol.init("123abc")),
        try readOne(&vm, "123abc"),
    );
}

test "readOne with complex nested mixed content returns proper structure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(func (arg1 #t) (arg2 #f))");
    try testing.expectFmt(
        "(func (arg1 #t) (arg2 #f))",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with nested booleans and symbols returns proper structure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "((#t hello) (#f world))");
    try testing.expectFmt(
        "((#t hello) (#f world))",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with unclosed expression returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "(hello world"),
    );
}

test "readOne with extra closing parenthesis returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "hello)"),
    );
}

test "readOne with nested unclosed expression returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "(hello (world"),
    );
}

test "readOne with mismatched parentheses returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "((hello)"),
    );
}

test "readOne with whitespace in expressions returns proper structure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(  hello   world  )");
    try testing.expectFmt(
        "(hello world)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with nested expressions and whitespace returns proper structure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "( a  ( b   c ) d )");
    try testing.expectFmt(
        "(a (b c) d)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with positive float returns float value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(3.14),
        try readOne(&vm, "3.14"),
    );
}

test "readOne with negative float returns float value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(-2.718),
        try readOne(&vm, "-2.718"),
    );
}

test "readOne with zero float returns float value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(0.0),
        try readOne(&vm, "0.0"),
    );
}

test "readOne with scientific notation returns float value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(1.23e-4),
        try readOne(&vm, "1.23e-4"),
    );
}

test "readOne with floats in list returns proper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(3.14 -2.718 0.0)");
    try testing.expectFmt(
        "(3.14 -2.718 0)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with mixed integers and floats in list returns proper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(42 3.14 -17 -2.718)");
    try testing.expectFmt(
        "(42 3.14 -17 -2.718)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with multiple expressions returns TooManyValues error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.TooManyValues,
        readOne(&vm, "(a) (b)"),
    );
}

test "readOne with empty nested lists with whitespace returns proper structure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "( ( ) )");
    try testing.expectFmt(
        "(())",
        "{f}",
        .{vm.pretty(result)},
    );
}
