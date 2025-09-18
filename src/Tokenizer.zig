const std = @import("std");

/// A simple tokenizer for parsing text into spans.
/// Handles parentheses as individual tokens and groups other characters into symbols.
const Tokenizer = @This();

/// The source text being tokenized
source: []const u8,
/// Current position in the source text
idx: usize,

/// Represents a span of text with start and end positions
pub const Span = struct {
    /// Starting position in the source text
    start: usize,
    /// Ending position in the source text (exclusive)
    end: usize,
};

/// Creates a new tokenizer for the given source text
pub fn init(source: []const u8) Tokenizer {
    return Tokenizer{ .source = source, .idx = 0 };
}

/// Returns the next token span, or null if no more tokens are available.
/// Parentheses are treated as individual tokens, while other non-whitespace
/// characters are grouped into symbols.
pub fn next(self: *Tokenizer) ?Span {
    self.eatWhitespace();
    const next_char = self.currentChar() orelse return null;
    if (isParen(next_char)) return self.eat();
    return self.eatSymbol();
}

/// Returns the text content of the next token, or null if no more tokens are
/// available.
pub fn nextText(self: *Tokenizer) ?[]const u8 {
    const span = self.next() orelse return null;
    return self.text(span);
}

/// Returns the text content for a given span
pub fn text(self: Tokenizer, span: Span) []const u8 {
    return self.source[span.start..span.end];
}

/// Returns the current character at the tokenizer's position, or null if at end
fn currentChar(self: Tokenizer) ?u8 {
    if (self.idx < self.source.len)
        return self.source[self.idx];
    return null;
}

/// Consumes a single character and returns its span
inline fn eat(self: *Tokenizer) Span {
    const span = Span{ .start = self.idx, .end = self.idx + 1 };
    self.idx += 1;
    return span;
}

/// Returns true if the character is a parenthesis.
fn isParen(ch: u8) bool {
    return ch == '(' or ch == ')';
}

/// Returns true if the character is whitespace (space, tab, newline, carriage return)
fn isWhitespace(ch: u8) bool {
    switch (ch) {
        ' ', '\t', '\n', '\r' => return true,
        else => return false,
    }
}

/// Skips over whitespace characters in the source text
fn eatWhitespace(self: *Tokenizer) void {
    while (self.currentChar()) |ch| {
        if (!isWhitespace(ch)) return;
        _ = self.eat();
    }
}

/// Consumes characters until whitespace is encountered, returning the span of the symbol
fn eatSymbol(self: *Tokenizer) Span {
    const start = self.idx;
    while (self.currentChar()) |ch| {
        if (isWhitespace(ch) or isParen(ch)) break;
        _ = self.eat();
    }
    return Span{ .start = start, .end = self.idx };
}

test "empty input then returns null" {
    var tokenizer = init("");
    try std.testing.expectEqual(null, tokenizer.nextText());
}

test "single symbol then returns symbol" {
    var tokenizer = init("hello");
    try std.testing.expectEqualStrings("hello", tokenizer.nextText().?);
    try std.testing.expectEqual(null, tokenizer.nextText());
}

test "single opening parenthesis then returns parenthesis" {
    var tokenizer = init("(");
    try std.testing.expectEqualStrings("(", tokenizer.nextText().?);
    try std.testing.expectEqual(null, tokenizer.nextText());
}

test "single closing parenthesis then returns parenthesis" {
    var tokenizer = init(")");
    try std.testing.expectEqualStrings(")", tokenizer.nextText().?);
    try std.testing.expectEqual(null, tokenizer.nextText());
}

test "multiple symbols separated by spaces then returns each symbol" {
    var tokenizer = init("hello world foo");
    try std.testing.expectEqualStrings("hello", tokenizer.nextText().?);
    try std.testing.expectEqualStrings("world", tokenizer.nextText().?);
    try std.testing.expectEqualStrings("foo", tokenizer.nextText().?);
    try std.testing.expectEqual(null, tokenizer.nextText());
}

test "mixed symbols and parentheses then returns tokens in order" {
    var tokenizer = init("(hello world)");
    try std.testing.expectEqualStrings("(", tokenizer.nextText().?);
    try std.testing.expectEqualStrings("hello", tokenizer.nextText().?);
    try std.testing.expectEqualStrings("world", tokenizer.nextText().?);
    try std.testing.expectEqualStrings(")", tokenizer.nextText().?);
    try std.testing.expectEqual(null, tokenizer.nextText());
}

test "leading and trailing whitespace then ignores whitespace" {
    var tokenizer = init("   hello   world   ");
    try std.testing.expectEqualStrings("hello", tokenizer.nextText().?);
    try std.testing.expectEqualStrings("world", tokenizer.nextText().?);
    try std.testing.expectEqual(null, tokenizer.nextText());
}

test "multiple whitespace types then treats all as whitespace" {
    var tokenizer = init("hello\t\n\r world");
    try std.testing.expectEqualStrings("hello", tokenizer.nextText().?);
    try std.testing.expectEqualStrings("world", tokenizer.nextText().?);
    try std.testing.expectEqual(null, tokenizer.nextText());
}

test "consecutive parentheses then returns each parenthesis separately" {
    var tokenizer = init("())(");
    try std.testing.expectEqualStrings("(", tokenizer.nextText().?);
    try std.testing.expectEqualStrings(")", tokenizer.nextText().?);
    try std.testing.expectEqualStrings(")", tokenizer.nextText().?);
    try std.testing.expectEqualStrings("(", tokenizer.nextText().?);
    try std.testing.expectEqual(null, tokenizer.nextText());
}

test "symbol immediately followed by parenthesis then returns separate tokens" {
    var tokenizer = init("hello(world)foo");
    try std.testing.expectEqualStrings("hello", tokenizer.nextText().?);
    try std.testing.expectEqualStrings("(", tokenizer.nextText().?);
    try std.testing.expectEqualStrings("world", tokenizer.nextText().?);
    try std.testing.expectEqualStrings(")", tokenizer.nextText().?);
    try std.testing.expectEqualStrings("foo", tokenizer.nextText().?);
    try std.testing.expectEqual(null, tokenizer.nextText());
}

test "next returns correct span indexes then span matches expected positions" {
    var tokenizer = init("hello world");
    const span1 = tokenizer.next().?;
    try std.testing.expectEqual(0, span1.start);
    try std.testing.expectEqual(5, span1.end);

    const span2 = tokenizer.next().?;
    try std.testing.expectEqual(6, span2.start);
    try std.testing.expectEqual(11, span2.end);

    try std.testing.expectEqual(null, tokenizer.next());
}
