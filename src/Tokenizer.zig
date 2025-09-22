//! Tokenizer for parsing Scheme source code into token spans.
//!
//! This module provides a simple tokenizer that breaks source text into tokens.
//! Parentheses are treated as individual tokens while other non-whitespace
//! characters are grouped together into symbols.

const std = @import("std");
const testing = std.testing;

/// A simple tokenizer for parsing text into spans.
/// Handles parentheses as individual tokens and groups other characters into symbols.
const Tokenizer = @This();

/// The source text being tokenized.
source: []const u8,
/// Current position in the source text.
idx: usize,

/// Represents a span of text with start and end positions.
pub const Span = struct {
    /// Starting position in the source text.
    start: usize,
    /// Ending position in the source text (exclusive).
    end: usize,
};

/// Creates a new tokenizer for the given source text.
pub fn init(source: []const u8) Tokenizer {
    return Tokenizer{ .source = source, .idx = 0 };
}

/// Returns the next token span, or null if no more tokens are available.
/// Parentheses are treated as individual tokens, while other non-whitespace
/// characters are grouped into symbols. Special handling for character literals.
pub fn next(self: *Tokenizer) ?Span {
    self.eatWhitespace();
    const next_char = self.currentChar() orelse return null;

    if (isParen(next_char)) return self.eat();
    if (next_char == '\'') return self.eat();
    if (std.mem.startsWith(u8, self.rest(), "#\\")) return self.eatCharacterLiteral();
    if (std.mem.startsWith(u8, self.rest(), "#(")) return self.eatVectorStart();
    if (next_char == '"') return self.eatStringLiteral();
    return self.eatSymbol();
}

/// Returns the text content of the next token, or null if no more tokens are available.
pub fn nextText(self: *Tokenizer) ?[]const u8 {
    const span = self.next() orelse return null;
    return self.text(span);
}

/// Returns the text content for a given span.
pub fn text(self: Tokenizer, span: Span) []const u8 {
    return self.source[span.start..span.end];
}

/// Returns the current character at the tokenizer's position, or null if at end.
fn currentChar(self: Tokenizer) ?u8 {
    if (self.idx < self.source.len)
        return self.source[self.idx];
    return null;
}

/// Returns the remaining unprocessed source text from the current position.
fn rest(self: Tokenizer) []const u8 {
    return self.source[self.idx..];
}

/// Consumes a single character and returns its span.
inline fn eat(self: *Tokenizer) Span {
    const span = Span{ .start = self.idx, .end = self.idx + 1 };
    self.idx += 1;
    return span;
}

/// Returns true if the character is a parenthesis.
fn isParen(ch: u8) bool {
    return ch == '(' or ch == ')';
}

/// Returns true if the character is whitespace (space, tab, newline, carriage return).
fn isWhitespace(ch: u8) bool {
    switch (ch) {
        ' ', '\t', '\n', '\r' => return true,
        else => return false,
    }
}

/// Skips over whitespace characters in the source text.
fn eatWhitespace(self: *Tokenizer) void {
    while (self.currentChar()) |ch| {
        if (!isWhitespace(ch)) return;
        _ = self.eat();
    }
}

/// Consumes characters until whitespace is encountered, returning the span of the symbol.
fn eatSymbol(self: *Tokenizer) Span {
    const start = self.idx;
    while (self.currentChar()) |ch| {
        if (isWhitespace(ch) or isParen(ch)) break;
        _ = self.eat();
    }
    return Span{ .start = start, .end = self.idx };
}

/// Consumes a character literal starting with #\ and returns its span.
/// After #\, consume everything until whitespace or parenthesis.
fn eatCharacterLiteral(self: *Tokenizer) Span {
    const start = self.idx;

    // Consume the '#' and '\'
    _ = self.eat();
    _ = self.eat();

    // Consume at least one character, then stop at whitespace or parenthesis
    if (self.currentChar()) |_| {
        _ = self.eat();
    }

    while (self.currentChar()) |ch| {
        if (isWhitespace(ch) or isParen(ch)) break;
        _ = self.eat();
    }

    return Span{ .start = start, .end = self.idx };
}

/// Consumes a string literal starting with " and returns its span.
/// Handles escape sequences properly within the string.
fn eatStringLiteral(self: *Tokenizer) Span {
    const start = self.idx;

    // Consume the opening quote
    _ = self.eat();

    while (self.currentChar()) |ch| {
        _ = self.eat();
        if (ch == '"') break;
        if (ch == '\\') {
            // Escape sequence, consume the character after the backslash.
            if (self.currentChar()) |_| _ = self.eat();
        }
    }

    return Span{ .start = start, .end = self.idx };
}

/// Consumes a vector start token "#(" and returns its span.
fn eatVectorStart(self: *Tokenizer) Span {
    const start = self.idx;
    // Consume "#("
    _ = self.eat(); // consume '#'
    _ = self.eat(); // consume '('
    return Span{ .start = start, .end = self.idx };
}

////////////////////////////////////////////////////////////////////////////////
// Vector Tokenization Tests
////////////////////////////////////////////////////////////////////////////////

test "vector start token #(" {
    var tokenizer = init("#(");
    try testing.expectEqualStrings("#(", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "vector with elements" {
    var tokenizer = init("#(1 2 3)");
    try testing.expectEqualStrings("#(", tokenizer.nextText().?);
    try testing.expectEqualStrings("1", tokenizer.nextText().?);
    try testing.expectEqualStrings("2", tokenizer.nextText().?);
    try testing.expectEqualStrings("3", tokenizer.nextText().?);
    try testing.expectEqualStrings(")", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "empty vector" {
    var tokenizer = init("#()");
    try testing.expectEqualStrings("#(", tokenizer.nextText().?);
    try testing.expectEqualStrings(")", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

////////////////////////////////////////////////////////////////////////////////
// Basic Tokenization Tests
////////////////////////////////////////////////////////////////////////////////

test "empty input then returns null" {
    var tokenizer = init("");
    try testing.expectEqual(null, tokenizer.nextText());
}

////////////////////////////////////////////////////////////////////////////////
// Character Literal Tests
////////////////////////////////////////////////////////////////////////////////

test "character literal with parenthesis" {
    var tokenizer = init("#\\(");
    try testing.expectEqualStrings("#\\(", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "character literal with letter" {
    var tokenizer = init("#\\a");
    try testing.expectEqualStrings("#\\a", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "character literal with space name" {
    var tokenizer = init("#\\space");
    try testing.expectEqualStrings("#\\space", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "character literal with invalid hex" {
    var tokenizer = init("#\\x123xyz");
    try testing.expectEqualStrings("#\\x123xyz", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

////////////////////////////////////////////////////////////////////////////////
// Symbol and Parenthesis Tests
////////////////////////////////////////////////////////////////////////////////

test "single symbol then returns symbol" {
    var tokenizer = init("hello");
    try testing.expectEqualStrings("hello", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "single opening parenthesis then returns parenthesis" {
    var tokenizer = init("(");
    try testing.expectEqualStrings("(", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "single closing parenthesis then returns parenthesis" {
    var tokenizer = init(")");
    try testing.expectEqualStrings(")", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "multiple symbols separated by spaces then returns each symbol" {
    var tokenizer = init("hello world foo");
    try testing.expectEqualStrings("hello", tokenizer.nextText().?);
    try testing.expectEqualStrings("world", tokenizer.nextText().?);
    try testing.expectEqualStrings("foo", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "mixed symbols and parentheses then returns tokens in order" {
    var tokenizer = init("(hello world)");
    try testing.expectEqualStrings("(", tokenizer.nextText().?);
    try testing.expectEqualStrings("hello", tokenizer.nextText().?);
    try testing.expectEqualStrings("world", tokenizer.nextText().?);
    try testing.expectEqualStrings(")", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "leading and trailing whitespace then ignores whitespace" {
    var tokenizer = init("   hello   world   ");
    try testing.expectEqualStrings("hello", tokenizer.nextText().?);
    try testing.expectEqualStrings("world", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "multiple whitespace types then treats all as whitespace" {
    var tokenizer = init("hello\t\n\r world");
    try testing.expectEqualStrings("hello", tokenizer.nextText().?);
    try testing.expectEqualStrings("world", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "consecutive parentheses then returns each parenthesis separately" {
    var tokenizer = init("())(");
    try testing.expectEqualStrings("(", tokenizer.nextText().?);
    try testing.expectEqualStrings(")", tokenizer.nextText().?);
    try testing.expectEqualStrings(")", tokenizer.nextText().?);
    try testing.expectEqualStrings("(", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "symbol immediately followed by parenthesis then returns separate tokens" {
    var tokenizer = init("hello(world)foo");
    try testing.expectEqualStrings("hello", tokenizer.nextText().?);
    try testing.expectEqualStrings("(", tokenizer.nextText().?);
    try testing.expectEqualStrings("world", tokenizer.nextText().?);
    try testing.expectEqualStrings(")", tokenizer.nextText().?);
    try testing.expectEqualStrings("foo", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

////////////////////////////////////////////////////////////////////////////////
// Span and Position Tests
////////////////////////////////////////////////////////////////////////////////

test "next returns correct span indexes then span matches expected positions" {
    var tokenizer = init("hello world");
    const span1 = tokenizer.next().?;
    try testing.expectEqual(0, span1.start);
    try testing.expectEqual(5, span1.end);

    const span2 = tokenizer.next().?;
    try testing.expectEqual(6, span2.start);
    try testing.expectEqual(11, span2.end);

    try testing.expectEqual(null, tokenizer.next());
}

////////////////////////////////////////////////////////////////////////////////
// Quote Tests
////////////////////////////////////////////////////////////////////////////////

test "single quote then returns quote as individual token" {
    var tokenizer = init("'");
    try testing.expectEqualStrings("'", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "quote followed by symbol then returns separate tokens" {
    var tokenizer = init("'hello");
    try testing.expectEqualStrings("'", tokenizer.nextText().?);
    try testing.expectEqualStrings("hello", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "quote with whitespace then ignores whitespace" {
    var tokenizer = init("'   hello");
    try testing.expectEqualStrings("'", tokenizer.nextText().?);
    try testing.expectEqualStrings("hello", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "quote in expression then returns as separate token" {
    var tokenizer = init("('quoted symbol)");
    try testing.expectEqualStrings("(", tokenizer.nextText().?);
    try testing.expectEqualStrings("'", tokenizer.nextText().?);
    try testing.expectEqualStrings("quoted", tokenizer.nextText().?);
    try testing.expectEqualStrings("symbol", tokenizer.nextText().?);
    try testing.expectEqualStrings(")", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "multiple quotes then returns each quote separately" {
    var tokenizer = init("''hello");
    try testing.expectEqualStrings("'", tokenizer.nextText().?);
    try testing.expectEqualStrings("'", tokenizer.nextText().?);
    try testing.expectEqualStrings("hello", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "quote within symbol then returns as single token" {
    var tokenizer = init("hello'world");
    try testing.expectEqualStrings("hello'world", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "quote span positions then returns correct indices" {
    var tokenizer = init("hello 'world");
    _ = tokenizer.next(); // skip hello
    const quote_span = tokenizer.next().?;
    try testing.expectEqual(6, quote_span.start);
    try testing.expectEqual(7, quote_span.end);
    try testing.expectEqualStrings("'", tokenizer.text(quote_span));
}

////////////////////////////////////////////////////////////////////////////////
// Strings
////////////////////////////////////////////////////////////////////////////////

test "string literal basic tokenization" {
    var tokenizer = init("\"hello world\"");
    try testing.expectEqualStrings("\"hello world\"", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "string literal with escape sequences" {
    var tokenizer = init("\"hello\\nworld\\t\"");
    try testing.expectEqualStrings("\"hello\\nworld\\t\"", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "string literal with escaped quotes" {
    var tokenizer = init("\"say \\\"hello\\\"\"");
    try testing.expectEqualStrings("\"say \\\"hello\\\"\"", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "empty string literal" {
    var tokenizer = init("\"\"");
    try testing.expectEqualStrings("\"\"", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "string literal with multiple tokens" {
    var tokenizer = init("hello \"world\" test");
    try testing.expectEqualStrings("hello", tokenizer.nextText().?);
    try testing.expectEqualStrings("\"world\"", tokenizer.nextText().?);
    try testing.expectEqualStrings("test", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}

test "unclosed string literal" {
    var tokenizer = init("\"unclosed");
    try testing.expectEqualStrings("\"unclosed", tokenizer.nextText().?);
    try testing.expectEqual(null, tokenizer.nextText());
}
