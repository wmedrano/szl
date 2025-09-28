//! Reader for parsing Scheme source code into values.
//!
//! The Reader module provides functionality to parse Scheme source code text
//! into the internal value representation used by the interpreter. It handles
//! tokenization and conversion of textual representations into typed values
//! like symbols, booleans, and eventually more complex structures like lists.

const std = @import("std");
const testing = std.testing;

const Char = @import("../types/Char.zig");
const Pair = @import("../types/Pair.zig");
const String = @import("../types/String.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vector = @import("../types/Vector.zig");
const Vm = @import("../Vm.zig");
const Tokenizer = @import("Tokenizer.zig");

const Reader = @This();

/// The virtual machine instance used for value creation and symbol interning.
vm: *Vm,
/// The tokenizer used to break source text into tokens.
tokenizer: Tokenizer,

const Error = error{ BadExpression, OutOfMemory };

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
pub fn next(self: *Reader) Error!?Val {
    while (true) {
        const token = self.tokenizer.nextText() orelse return null;
        const result = try self.parseTokenValue(token);
        switch (result) {
            .value => |val| return val,
            .end_expression => return error.BadExpression, // Unexpected ) at top level
            .comment => continue, // Skip comments and try next token
        }
    }
}

const ParseResult = union(enum) {
    value: Val,
    end_expression: void,
    comment: void,
};

/// Parses a token into a value or signals end of expression.
///
/// Args:
///   self: Pointer to the Reader instance.
///   token: The token to parse.
///
/// Returns:
///   ParseResult indicating either a parsed value or end of expression.
///
/// Errors:
///   - BadExpression: If malformed expressions are encountered.
///   - Other errors from parseToken, nextExpression, or nextQuoted functions.
fn parseTokenValue(self: *Reader, token: []const u8) Error!ParseResult {
    if (std.mem.eql(u8, token, "(")) {
        const expr = try self.nextExpression() orelse return error.BadExpression;
        return ParseResult{ .value = expr };
    }
    if (std.mem.eql(u8, token, ")")) return ParseResult{ .end_expression = {} };
    if (std.mem.eql(u8, token, "'")) {
        const quoted = try self.nextQuoted();
        return ParseResult{ .value = quoted };
    }
    if (std.mem.eql(u8, token, "#(")) {
        const vector = try self.nextVector();
        return ParseResult{ .value = vector };
    }
    // Skip comments - they start with semicolon
    if (token.len > 0 and token[0] == ';') {
        // Return a special result that indicates we should skip this token
        // We need to add a new variant to ParseResult for this
        return ParseResult{ .comment = {} };
    }
    const val = try self.parseToken(token);
    return ParseResult{ .value = val };
}

/// Parses a quoted expression (quote syntax sugar).
///
/// Handles the single quote character which is syntactic sugar for the quote special form.
/// Transforms 'expr into (quote expr) by building the appropriate list structure.
///
/// Args:
///   self: Pointer to the Reader instance.
///
/// Returns:
///   A Val representing the quoted expression as (quote expr).
///
/// Errors:
///   - BadExpression: If no expression follows the quote character.
///   - Other errors from the VM's builder when creating the quote structure.
fn nextQuoted(self: *Reader) Error!Val {
    const expr = (try self.next()) orelse return error.BadExpression;
    const builder = self.vm.builder();
    return try builder.build(Pair{
        .car = try builder.internStaticVal(Symbol.init("quote")),
        .cdr = try builder.build(Pair{
            .car = expr,
            .cdr = Val.init({}),
        }),
    });
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
        switch (try self.parseTokenValue(token)) {
            .value => |val| try expressions.append(self.vm.allocator, val),
            .end_expression => return try self.vm.builder().buildList(expressions.items),
            .comment => continue, // Skip comments in expressions
        }
    }
    return error.BadExpression;
}

/// Parses a vector literal starting with "#(" and ending with ")".
///
/// Reads elements from the tokenizer until a closing parenthesis is encountered,
/// then creates a vector containing all the parsed elements.
///
/// Args:
///   self: Pointer to the Reader instance.
///
/// Returns:
///   A parsed vector value containing all elements.
///
/// Errors:
///   - BadExpression: If the vector is malformed or unclosed.
fn nextVector(self: *Reader) !Val {
    var elements = std.ArrayList(Val){};
    defer elements.deinit(self.vm.allocator);
    while (self.tokenizer.nextText()) |token| {
        switch (try self.parseTokenValue(token)) {
            .value => |val| try elements.append(self.vm.allocator, val),
            .end_expression => {
                const vector = try Vector.initFromSlice(self.vm.allocator, elements.items);
                return try self.vm.toVal(vector);
            },
            .comment => continue, // Skip comments in vectors
        }
    }
    return error.BadExpression;
}

/// Parses a character literal token into a character value.
///
/// Handles character literals in the forms:
/// - #\<character> for single characters (e.g., #\a, #\A, #\()
/// - #\<character name> for named characters (e.g., #\space, #\newline)
/// - #\x<hex> for hexadecimal character codes (e.g., #\x41 for 'A')
///
/// Args:
///   token: The token text to parse as a character literal.
///
/// Returns:
///   A Char value if the token is a valid character literal, null otherwise.
///
/// Errors:
///   - BadExpression: If the character literal is malformed.
fn parseChar(token: []const u8) !?Char {
    if (token.len < 3 or !std.mem.startsWith(u8, token, "#\\")) {
        return null;
    }

    const char_part = token[2..];

    // Handle single character literals
    // TODO: Sanitize this more.
    if (char_part.len == 1) {
        return Char.init(char_part[0]);
    }

    // Handle hex notation #\x<hex>
    if (std.mem.startsWith(u8, char_part, "x")) {
        if (char_part.len < 2) return error.BadExpression;
        const hex_part = char_part[1..];
        const value = std.fmt.parseInt(u8, hex_part, 16) catch return error.BadExpression;
        return Char.init(value);
    }

    // Handle named characters
    if (std.mem.eql(u8, char_part, "alarm")) return Char.init(0x07);
    if (std.mem.eql(u8, char_part, "backspace")) return Char.init(0x08);
    if (std.mem.eql(u8, char_part, "delete")) return Char.init(0x7f);
    if (std.mem.eql(u8, char_part, "escape")) return Char.init(0x1b);
    if (std.mem.eql(u8, char_part, "newline")) return Char.init(0x0a);
    if (std.mem.eql(u8, char_part, "null")) return Char.init(0x00);
    if (std.mem.eql(u8, char_part, "return")) return Char.init(0x0d);
    if (std.mem.eql(u8, char_part, "space")) return Char.init(0x20);
    if (std.mem.eql(u8, char_part, "tab")) return Char.init(0x09);

    // If we get here, it's not a valid character literal
    return null;
}

/// Parses a string literal token into a string value.
///
/// Handles string literals in the form "string content" with escape sequences.
/// Supports common escape sequences like \", \\, \n, \r, \t, \b, \f, \a, \v, \0
/// and hexadecimal escape sequences \xHH.
///
/// Args:
///   self: The Reader instance.
///   token: The token text to parse as a string literal.
///
/// Returns:
///   A String value if the token is a valid string literal, null otherwise.
///
/// Errors:
///   - BadExpression: If the string literal is malformed.
///   - OutOfMemory: If string allocation fails.
fn parseString(self: *Reader, token: []const u8) !?String {
    if (token.len < 2 or token[0] != '"') {
        return null;
    }

    // Check if string is properly closed
    if (token[token.len - 1] != '"') {
        return error.BadExpression;
    }

    var string = String.initStatic("");
    errdefer string.deinit(self.vm.allocator);
    const content = token[1 .. token.len - 1]; // Remove surrounding quotes
    var i: usize = 0;

    while (i < content.len) {
        const ch = content[i];
        if (ch != '\\') {
            try string.appendByte(self.vm.allocator, content[i]);
            i += 1;
        } else {
            if (i + 1 >= content.len)
                return error.BadExpression;
            switch (content[i + 1]) {
                '"' => try string.appendByte(self.vm.allocator, '"'),
                '\\' => try string.appendByte(self.vm.allocator, '\\'),
                'n' => try string.appendByte(self.vm.allocator, '\n'),
                'r' => try string.appendByte(self.vm.allocator, '\r'),
                't' => try string.appendByte(self.vm.allocator, '\t'),
                'b' => try string.appendByte(self.vm.allocator, 0x08), // backspace
                'f' => try string.appendByte(self.vm.allocator, 0x0C), // form feed
                'a' => try string.appendByte(self.vm.allocator, 0x07), // bell/alarm
                'v' => try string.appendByte(self.vm.allocator, 0x0B), // vertical tab
                '0' => try string.appendByte(self.vm.allocator, 0x00), // null
                'x' => {
                    // Hexadecimal escape sequence \xHH
                    if (i + 3 >= content.len) return error.BadExpression;
                    const hex_str = content[i + 2 .. i + 4];
                    const value = std.fmt.parseInt(u8, hex_str, 16) catch {
                        return error.BadExpression;
                    };
                    try string.appendByte(self.vm.allocator, value);
                    i += 2; // Skip extra characters for hex
                },
                else => return error.BadExpression,
            }
            i += 2; // Skip escape sequence
        }
    }

    return string;
}

/// Parses a single token into its corresponding value.
///
/// Converts textual token representations into their appropriate value types.
/// Recognizes boolean literals (#t and #f), character literals (#\<char>),
/// string literals ("string"), integers, floating point numbers, and treats
/// all other tokens as symbols.
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
///   - BadExpression: If a token starting with #\ fails to parse as a character.
///   - May return errors from the VM's builder when creating symbol values.
fn parseToken(self: *Reader, token: []const u8) !Val {
    if (std.mem.eql(u8, token, "#t") or std.mem.eql(u8, token, "#true"))
        return Val.init(true);
    if (std.mem.eql(u8, token, "#f") or std.mem.eql(u8, token, "#false"))
        return Val.init(false);

    // Check if this looks like a character literal
    if (token.len >= 2 and std.mem.startsWith(u8, token, "#\\")) {
        if (try parseChar(token)) |ch| {
            return Val.init(ch);
        } else {
            return error.BadExpression;
        }
    }

    // Check if this looks like a string literal
    if (token.len >= 2 and token[0] == '"') {
        if (try self.parseString(token)) |string| {
            return self.vm.toVal(string);
        } else {
            return error.BadExpression;
        }
    }

    if (std.fmt.parseInt(i64, token, 10) catch null) |x|
        return Val.init(x);
    if (std.fmt.parseFloat(f64, token) catch null) |x|
        return Val.init(x);
    return self.vm.toVal(Symbol.init(token));
}

////////////////////////////////////////////////////////////////////////////////
// Basic readOne tests
////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////
// Integer parsing tests
////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////
// List parsing tests
////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////
// Float parsing tests
////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////
// Quote parsing tests
////////////////////////////////////////////////////////////////////////////////

test "readOne with quoted symbol returns quote expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "'hello");
    try testing.expectFmt(
        "(quote hello)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with quoted boolean returns quote expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "'#t");
    try testing.expectFmt(
        "(quote #t)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with quoted integer returns quote expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "'42");
    try testing.expectFmt(
        "(quote 42)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with quoted float returns quote expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "'3.14");
    try testing.expectFmt(
        "(quote 3.14)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with quoted list returns quote expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "'(a b c)");
    try testing.expectFmt(
        "(quote (a b c))",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with quoted empty list returns quote expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "'()");
    try testing.expectFmt(
        "(quote ())",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with quoted nested list returns quote expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "'(a (b c) d)");
    try testing.expectFmt(
        "(quote (a (b c) d))",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with quote followed by nothing returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "'"),
    );
}

test "readOne with quote followed by whitespace only returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "'   "),
    );
}

test "readOne with nested quotes returns nested quote expressions" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "''hello");
    try testing.expectFmt(
        "(quote (quote hello))",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with mixed integers and quoted list returns proper structure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(1 2 '(3 4))");
    try testing.expectFmt(
        "(1 2 (quote (3 4)))",
        "{f}",
        .{vm.pretty(result)},
    );
}

////////////////////////////////////////////////////////////////////////////////
// Character tests
////////////////////////////////////////////////////////////////////////////////

test "readOne with single character literal returns character value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectReadOne(.char, "#\\a", "#\\a");
}

test "readOne with uppercase character literal returns character value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectReadOne(.char, "#\\A", "#\\A");
}

test "readOne with uppercase character literal x returns #\\x" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\x");
    try testing.expectEqual(
        Val.init(Char.init('x')),
        result,
    );
}

test "readOne with parenthesis character literal returns character value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\(");
    try testing.expectEqual(
        Val.init(Char.init('(')),
        result,
    );
}

test "readOne with space character name returns character value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectReadOne(.char, "#\\space", "#\\space");
}

test "readOne with newline character name returns character value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\newline");
    try testing.expectEqual(
        Val.init(Char.init('\n')),
        result,
    );
}

test "readOne with tab character name returns character value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\tab");
    try testing.expectEqual(
        Val.init(Char.init('\t')),
        result,
    );
}

test "readOne with alarm character name returns character value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\alarm");
    try testing.expectEqual(
        Val.init(Char.init(0x07)),
        result,
    );
}

test "readOne with backspace character name returns character value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\backspace");
    try testing.expectEqual(
        Val.init(Char.init(0x08)),
        result,
    );
}

test "readOne with delete character name returns character value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\delete");
    try testing.expectEqual(
        Val.init(Char.init(0x7f)),
        result,
    );
}

test "readOne with escape character name returns character value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\escape");
    try testing.expectEqual(
        Val.init(Char.init(0x1b)),
        result,
    );
}

test "readOne with null character name returns character value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\null");
    try testing.expectEqual(
        Val.init(Char.init(0x00)),
        result,
    );
}

test "readOne with return character name returns character value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\return");
    try testing.expectEqual(
        Val.init(Char.init('\r')),
        result,
    );
}

test "readOne with hex character literal returns character value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\x41");
    try testing.expectEqual(
        Val.init(Char.init(0x41)),
        result,
    );
}

test "readOne with lowercase hex character literal returns character value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\x7f");
    try testing.expectEqual(
        Val.init(Char.init(0x7f)),
        result,
    );
}

test "readOne with uppercase hex character literal returns character value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\xA0");
    try testing.expectEqual(
        Val.init(Char.init(0xA0)),
        result,
    );
}

test "readOne with characters in list returns proper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(#\\a #\\space #\\newline)");
    try testing.expectFmt(
        "(#\\a #\\space #\\newline)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with malformed character literal missing char returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "#\\"),
    );
}

test "readOne with incomplete character literal returns symbol" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#");
    try testing.expectEqual(
        try vm.toVal(Symbol.init("#")),
        result,
    );
}

test "readOne with malformed character literal with invalid name returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "#\\invalidname"),
    );
}

test "readOne with empty character name returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "#\\"),
    );
}

// Invalid hex values and formats tests
test "readOne with invalid hex character literal returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "#\\xGG"),
    );
}

test "readOne with incomplete hex character literal returns character x" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\x");
    try testing.expectEqual(
        Val.init(Char.init('x')),
        result,
    );
}

test "readOne with hex character literal with invalid characters returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "#\\x1Z"),
    );
}

test "readOne with hex character literal with too many digits returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "#\\x123"),
    );
}

// Boundary conditions for hex parsing
test "readOne with hex character literal 00 returns null character" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\x00");
    try testing.expectEqual(
        Val.init(Char.init(0x00)),
        result,
    );
}

test "readOne with hex character literal FF returns max character" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\xFF");
    try testing.expectEqual(
        Val.init(Char.init(0xFF)),
        result,
    );
}

test "readOne with hex character literal with single digit returns character" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\x5");
    try testing.expectEqual(
        Val.init(Char.init(0x05)),
        result,
    );
}

test "readOne with hex character literal with lowercase returns character" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\xab");
    try testing.expectEqual(
        Val.init(Char.init(0xab)),
        result,
    );
}

// Non-printable character handling tests
test "readOne with control character returns character value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#\\x01");
    try testing.expectEqual(
        Val.init(Char.init(0x01)),
        result,
    );
}

test "readOne with non-printable character in list returns proper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(#\\x00 #\\x1F #\\x7F)");
    try testing.expectFmt(
        "(#\\null #\\x1F #\\delete)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with all named control characters returns proper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(#\\alarm #\\backspace #\\tab #\\newline #\\return #\\escape)");
    try testing.expectFmt(
        "(#\\alarm #\\backspace #\\tab #\\newline #\\return #\\escape)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with mixed printable and non-printable characters returns proper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(#\\a #\\x00 #\\space #\\x1f #\\z)");
    try testing.expectFmt(
        "(#\\a #\\null #\\space #\\x1F #\\z)",
        "{f}",
        .{vm.pretty(result)},
    );
}

////////////////////////////////////////////////////////////////////////////////
// String tests
////////////////////////////////////////////////////////////////////////////////

test "readOne with simple string literal returns string value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectReadOne(.string, "\"hello world\"", "\"hello world\"");
}

test "readOne with empty string literal returns empty string value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectReadOne(.string, "\"\"", "\"\"");
}

test "readOne with string containing escape sequences returns string value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectReadOne(.string, "\"hello\\nworld\\t\"", "\"hello\\nworld\\t\"");
}

test "readOne with string containing escaped quotes returns string value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectReadOne(.string, "\"say \\\"hello\\\"\"", "\"say \\\"hello\\\"\"");
}

test "readOne with string containing hex escape sequences returns string value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectReadOne(.string, "\"ABC\"", "\"\\x41\\x42\\x43\"");
}

test "readOne with string containing all escape sequences returns string value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // The pretty printer will format these back as escape sequences
    try vm.expectReadOne(.string, "\"\\\"\\\\\\n\\r\\t\\b\\f\\a\\v\\0\"", "\"\\\"\\\\\\n\\r\\t\\b\\f\\a\\v\\0\"");
}

test "readOne with strings in list returns proper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(\"hello\" \"world\" \"test\")");
    try testing.expectFmt(
        "(\"hello\" \"world\" \"test\")",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with unclosed string literal returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "\"unclosed string"),
    );
}

test "readOne with invalid escape sequence returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "\"invalid\\q\""),
    );
}

test "readOne with incomplete hex escape sequence returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "\"\\x4\""),
    );
}

test "readOne with invalid hex escape sequence returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "\"\\xGG\""),
    );
}

test "readOne with string containing unicode characters returns string value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectReadOne(.string, "\"héllo 🤖\"", "\"héllo 🤖\"");
}

////////////////////////////////////////////////////////////////////////////////
// Vector tests
////////////////////////////////////////////////////////////////////////////////

test "readOne with empty vector returns vector value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectReadOne(.vector, "#()", "#()");
}

test "readOne with simple vector returns vector value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectReadOne(.vector, "#(1 2 3)", "#(1 2 3)");
}

test "readOne with mixed type vector returns vector value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectReadOne(.vector, "#(42 #t () \"hello\")", "#(42 #t () \"hello\")");
}

test "readOne with nested vectors returns vector value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectReadOne(.vector, "#(#(1 2) #(3 4))", "#(#(1 2) #(3 4))");
}

test "readOne with vector containing lists returns vector value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectReadOne(.vector, "#((a b) (c d))", "#((a b) (c d))");
}

test "readOne with single element vector returns vector value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectReadOne(.vector, "#(42)", "#(42)");
}

test "readOne with unclosed vector returns BadExpression error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.BadExpression,
        readOne(&vm, "#(1 2 3"),
    );
}

test "readOne evaluates vector elements correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#(1 #t \"hello\")");
    try testing.expectEqual(.vector, std.meta.activeTag(result.repr));

    const vector = try vm.inspector().to(Vector, result);
    try testing.expectEqual(3, vector.len());

    try testing.expectEqual(Val.init(1), try vector.get(0));
    try testing.expectEqual(Val.init(true), try vector.get(1));
    const string_val = try vector.get(2);
    try testing.expectEqual(.string, std.meta.activeTag(string_val.repr));
}

////////////////////////////////////////////////////////////////////////////////
// Comment Tests
////////////////////////////////////////////////////////////////////////////////

test "readOne with comment followed by expression skips comment" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "; this is a comment\nhello");
    try testing.expectEqual(
        try vm.toVal(Symbol.init("hello")),
        result,
    );
}

test "readOne with expression followed by comment returns only expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "42 ; this is a comment");
    try testing.expectEqual(
        Val.init(42),
        result,
    );
}

test "readOne with comment only returns NoValue error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.NoValue,
        readOne(&vm, "; just a comment"),
    );
}

test "readOne with multiple comments before expression skips all comments" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "; first comment\n; second comment\n#t");
    try testing.expectEqual(
        Val.init(true),
        result,
    );
}

test "readOne with list containing comments skips comments within list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(+ ; addition operator\n 1 ; first number\n 2) ; second number");
    try testing.expectFmt(
        "(+ 1 2)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with nested list containing comments returns proper structure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "(define ; define a variable\n x ; variable name\n (+ 1 2)) ; variable value");
    try testing.expectFmt(
        "(define x (+ 1 2))",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with vector containing comments skips comments within vector" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#(1 ; first element\n 2 ; second element\n 3) ; third element");
    try testing.expectFmt(
        "#(1 2 3)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with comment at end of file returns NoValue error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.NoValue,
        readOne(&vm, "; comment at end of file"),
    );
}

test "readOne with expression and multiple trailing comments returns expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#f ; first comment\n; second comment");
    try testing.expectEqual(
        Val.init(false),
        result,
    );
}

test "readOne with complex nested structure and comments returns proper structure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm,
        \\(let ; let binding
        \\  ((x 10) ; x equals 10
        \\   (y 20)) ; y equals 20
        \\  (+ x y)) ; add x and y
    );
    try testing.expectFmt(
        "(let ((x 10) (y 20)) (+ x y))",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with quote followed by comment and expression returns quoted expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "' ; quote symbol\nhello");
    try testing.expectFmt(
        "(quote hello)",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with empty vector containing only comments returns empty vector" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "#( ; just comments\n ; in vector\n )");
    try testing.expectFmt(
        "#()",
        "{f}",
        .{vm.pretty(result)},
    );
}

test "readOne with empty list containing only comments returns empty list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try readOne(&vm, "( ; just comments\n ; in list\n )");
    try testing.expectFmt(
        "()",
        "{f}",
        .{vm.pretty(result)},
    );
}
