const std = @import("std");
const testing = std.testing;

const Tokenizer = @This();

pub const TokenType = enum {
    // Delimiters
    left_paren,
    right_paren,
    dot,
    // Literals
    identifier,
    string,
    sharpsign_expr_start,
    // Special
    quote,
    quasiquote,
    unquote,
    unquote_splicing,
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: u32,
    column: u32,
};

pub const CompletionStatus = enum {
    complete,
    missing_close_paren,
    malformed,
};

source: []const u8,
start: u32 = 0,
current: u32 = 0,
line: u32 = 1,
column: u32 = 1,

pub fn init(source: []const u8) Tokenizer {
    return .{ .source = source };
}

pub fn nextToken(self: *Tokenizer) ?Token {
    self.skipWhitespaceAndComments();

    if (self.isAtEnd()) {
        return null;
    }

    self.start = self.current;
    const start_column = self.column;

    const c = self.advance();

    return switch (c) {
        '(' => self.makeTokenAt(.left_paren, start_column),
        ')' => self.makeTokenAt(.right_paren, start_column),
        '.' => if (self.isAtEnd() or isDelimiter(self.peek()))
            self.makeTokenAt(.dot, start_column)
        else
            self.scanIdentifier(start_column),
        '\'' => self.makeTokenAt(.quote, start_column),
        '`' => self.makeTokenAt(.quasiquote, start_column),
        ',' => if (self.match('@'))
            self.makeTokenAt(.unquote_splicing, start_column)
        else
            self.makeTokenAt(.unquote, start_column),
        '"' => self.scanString(start_column),
        '#' => self.scanHashPrefix(start_column),
        else => self.scanIdentifier(start_column),
    };
}

fn skipWhitespaceAndComments(self: *Tokenizer) void {
    while (!self.isAtEnd()) {
        const c = self.peek();
        switch (c) {
            ' ', '\r', '\t' => {
                _ = self.advance();
            },
            '\n' => {
                self.line += 1;
                self.column = 0;
                _ = self.advance();
            },
            ';' => {
                // Skip comment until end of line
                while (!self.isAtEnd() and self.peek() != '\n') {
                    _ = self.advance();
                }
            },
            else => return,
        }
    }
}

fn scanString(self: *Tokenizer, start_column: u32) Token {
    while (!self.isAtEnd() and self.peek() != '"') {
        if (self.peek() == '\n') {
            self.line += 1;
            self.column = 0;
        }
        if (self.peek() == '\\') {
            _ = self.advance();
            if (!self.isAtEnd()) {
                _ = self.advance();
            }
        } else {
            _ = self.advance();
        }
    }

    if (self.isAtEnd()) {
        return self.makeTokenAt(.string, start_column);
    }

    // Closing "
    _ = self.advance();
    return self.makeTokenAt(.string, start_column);
}

fn scanHashPrefix(self: *Tokenizer, start_column: u32) Token {
    // Could be boolean (#t, #f, #true, #false), character (#\a, #\newline, #\x3BB), vector (#(), or bytevector (#u8()
    if (self.isAtEnd()) {
        return self.makeTokenAt(.identifier, start_column); // Just '#'
    }

    if (self.peek() == '(') {
        // Vector literal: #(
        _ = self.advance(); // consume '('
        return self.makeTokenAt(.sharpsign_expr_start, start_column);
    } else if (self.peek() == 'u') {
        // Could be bytevector (#u8() or other identifier
        const next_pos = self.current + 1;
        if (next_pos < self.source.len and self.source[next_pos] == '8') {
            const next_next_pos = next_pos + 1;
            if (next_next_pos < self.source.len and self.source[next_next_pos] == '(') {
                _ = self.advance(); // consume 'u'
                _ = self.advance(); // consume '8'
                _ = self.advance(); // consume '('
                return self.makeTokenAt(.sharpsign_expr_start, start_column);
            }
        }
        // Not a bytevector, treat as identifier
        return self.scanIdentifier(start_column);
    } else if (self.peek() == '\\') {
        // Character literal - scan the rest
        _ = self.advance(); // consume '\'

        if (self.isAtEnd()) {
            return self.makeTokenAt(.identifier, start_column);
        }

        // Check for hex escape
        if (self.peek() == 'x') {
            _ = self.advance(); // consume 'x'
            // Read hex digits
            while (!self.isAtEnd() and std.ascii.isHex(self.peek())) {
                _ = self.advance();
            }
            return self.makeTokenAt(.identifier, start_column);
        }

        // First, consume at least one character
        _ = self.advance();

        // Check if this is a named character (more chars follow that aren't delimiters)
        while (!self.isAtEnd() and !isDelimiter(self.peek())) {
            _ = self.advance();
        }

        return self.makeTokenAt(.identifier, start_column);
    } else {
        // Boolean or other identifier starting with #
        return self.scanIdentifier(start_column);
    }
}

fn scanIdentifier(self: *Tokenizer, start_column: u32) Token {
    while (!self.isAtEnd() and !isDelimiter(self.peek())) {
        _ = self.advance();
    }
    return self.makeTokenAt(.identifier, start_column);
}

fn isDelimiter(c: u8) bool {
    return switch (c) {
        '(', ')', '"', ';', ' ', '\t', '\n', '\r' => true,
        else => false,
    };
}

fn advance(self: *Tokenizer) u8 {
    const c = self.source[self.current];
    self.current += 1;
    self.column += 1;
    return c;
}

fn peek(self: *Tokenizer) u8 {
    if (self.isAtEnd()) return 0;
    return self.source[self.current];
}

fn match(self: *Tokenizer, expected: u8) bool {
    if (self.isAtEnd()) return false;
    if (self.source[self.current] != expected) return false;
    _ = self.advance();
    return true;
}

fn isAtEnd(self: *Tokenizer) bool {
    return self.current >= self.source.len;
}

fn makeToken(self: *Tokenizer, token_type: TokenType) Token {
    return self.makeTokenAt(token_type, self.column - (self.current - self.start));
}

fn makeTokenAt(self: *Tokenizer, token_type: TokenType, start_column: u32) Token {
    return .{
        .type = token_type,
        .lexeme = self.source[self.start..self.current],
        .line = self.line,
        .column = start_column,
    };
}

/// Checks if the source contains a complete expression with balanced parentheses.
/// This properly handles comments and strings, as it uses the tokenizer.
pub fn isComplete(source: []const u8) CompletionStatus {
    var tokenizer = Tokenizer.init(source);
    var depth: i32 = 0;

    while (tokenizer.nextToken()) |token| {
        switch (token.type) {
            .left_paren, .sharpsign_expr_start => depth += 1,
            .right_paren => {
                depth -= 1;
                if (depth < 0) {
                    return .malformed;
                }
            },
            else => {},
        }
    }

    if (depth > 0) {
        return .missing_close_paren;
    }

    return .complete;
}

test "tokenize empty string" {
    var tokenizer = Tokenizer.init("");
    const token = tokenizer.nextToken();
    try testing.expectEqualDeep(null, token);
}

test "tokenize parentheses" {
    var tokenizer = Tokenizer.init("()");
    try testing.expectEqualDeep(
        Token{ .type = TokenType.left_paren, .lexeme = "(", .line = 1, .column = 1 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.right_paren, .lexeme = ")", .line = 1, .column = 2 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(null, tokenizer.nextToken());
}

test "tokenize identifiers" {
    var tokenizer = Tokenizer.init("+ define a-lambda 123 45.67");
    // Operators
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "+", .line = 1, .column = 1 },
        tokenizer.nextToken(),
    );
    // Multi-character symbols
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "define", .line = 1, .column = 3 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "a-lambda", .line = 1, .column = 10 },
        tokenizer.nextToken(),
    );
    // Numbers
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "123", .line = 1, .column = 19 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "45.67", .line = 1, .column = 23 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(null, tokenizer.nextToken());
}

test "tokenize strings" {
    var tokenizer = Tokenizer.init("\"hello world\"");
    try testing.expectEqualDeep(
        Token{ .type = TokenType.string, .lexeme = "\"hello world\"", .line = 1, .column = 1 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(null, tokenizer.nextToken());
}

test "tokenize hash-prefixed identifiers" {
    var tokenizer = Tokenizer.init("#t #f #true #false #badboolean");
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "#t", .line = 1, .column = 1 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "#f", .line = 1, .column = 4 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "#true", .line = 1, .column = 7 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "#false", .line = 1, .column = 13 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "#badboolean", .line = 1, .column = 20 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(null, tokenizer.nextToken());
}

test "tokenize characters" {
    var tokenizer = Tokenizer.init("#\\a #\\Z #\\( #\\space #\\newline #\\x3BB");
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "#\\a", .line = 1, .column = 1 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "#\\Z", .line = 1, .column = 5 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "#\\(", .line = 1, .column = 9 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "#\\space", .line = 1, .column = 13 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "#\\newline", .line = 1, .column = 21 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "#\\x3BB", .line = 1, .column = 31 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(null, tokenizer.nextToken());
}

test "tokenize quote forms" {
    var tokenizer = Tokenizer.init("'x `x ,x ,@x");
    try testing.expectEqualDeep(
        Token{ .type = TokenType.quote, .lexeme = "'", .line = 1, .column = 1 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "x", .line = 1, .column = 2 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.quasiquote, .lexeme = "`", .line = 1, .column = 4 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "x", .line = 1, .column = 5 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.unquote, .lexeme = ",", .line = 1, .column = 7 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "x", .line = 1, .column = 8 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.unquote_splicing, .lexeme = ",@", .line = 1, .column = 10 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "x", .line = 1, .column = 12 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(null, tokenizer.nextToken());
}

test "tokenize with comments" {
    var tokenizer = Tokenizer.init("; comment\n(+ 1 2)");
    try testing.expectEqualDeep(
        Token{ .type = TokenType.left_paren, .lexeme = "(", .line = 2, .column = 1 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "+", .line = 2, .column = 2 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "1", .line = 2, .column = 4 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "2", .line = 2, .column = 6 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.right_paren, .lexeme = ")", .line = 2, .column = 7 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(null, tokenizer.nextToken());
}

test "isComplete: empty string is complete" {
    try testing.expectEqual(CompletionStatus.complete, isComplete(""));
}

test "isComplete: complete expression" {
    try testing.expectEqual(CompletionStatus.complete, isComplete("(+ 1 2)"));
}

test "isComplete: nested complete expression" {
    try testing.expectEqual(CompletionStatus.complete, isComplete("(+ (- 3 1) 2)"));
}

test "isComplete: multiple complete expressions" {
    try testing.expectEqual(CompletionStatus.complete, isComplete("(+ 1 2) (- 3 1)"));
}

test "isComplete: complete with comment" {
    try testing.expectEqual(CompletionStatus.complete, isComplete("; comment\n(+ 1 2)"));
}

test "isComplete: complete with string containing parens" {
    try testing.expectEqual(CompletionStatus.complete, isComplete("(print \"(hello)\")"));
}

test "isComplete: missing close paren" {
    try testing.expectEqual(CompletionStatus.missing_close_paren, isComplete("(+ 1 2"));
}

test "isComplete: nested missing close paren" {
    try testing.expectEqual(CompletionStatus.missing_close_paren, isComplete("(+ (- 3 1) 2"));
}

test "isComplete: multiple missing close parens" {
    try testing.expectEqual(CompletionStatus.missing_close_paren, isComplete("((+ 1 2"));
}

test "isComplete: malformed with extra close paren" {
    try testing.expectEqual(CompletionStatus.malformed, isComplete("())"));
}

test "isComplete: malformed with only close paren" {
    try testing.expectEqual(CompletionStatus.malformed, isComplete(")"));
}

test "isComplete: malformed in middle of expression" {
    try testing.expectEqual(CompletionStatus.malformed, isComplete("(+ 1 2)) (- 3 1)"));
}

test "isComplete: just symbols is complete" {
    try testing.expectEqual(CompletionStatus.complete, isComplete("define x 42"));
}

test "tokenize vectors" {
    var tokenizer = Tokenizer.init("#() #(1 2 3)");
    try testing.expectEqualDeep(
        Token{ .type = TokenType.sharpsign_expr_start, .lexeme = "#(", .line = 1, .column = 1 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.right_paren, .lexeme = ")", .line = 1, .column = 3 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.sharpsign_expr_start, .lexeme = "#(", .line = 1, .column = 5 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "1", .line = 1, .column = 7 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "2", .line = 1, .column = 9 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "3", .line = 1, .column = 11 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.right_paren, .lexeme = ")", .line = 1, .column = 12 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(null, tokenizer.nextToken());
}

test "isComplete: empty vector is complete" {
    try testing.expectEqual(CompletionStatus.complete, isComplete("#()"));
}

test "isComplete: complete vector" {
    try testing.expectEqual(CompletionStatus.complete, isComplete("#(1 2 3)"));
}

test "isComplete: nested vector is complete" {
    try testing.expectEqual(CompletionStatus.complete, isComplete("#(1 #(2 3) 4)"));
}

test "isComplete: missing close paren in vector" {
    try testing.expectEqual(CompletionStatus.missing_close_paren, isComplete("#(1 2 3"));
}

test "isComplete: vector with list is complete" {
    try testing.expectEqual(CompletionStatus.complete, isComplete("#((1 2) 3)"));
}

test "tokenizer with number-like items parses as identifiers" {
    var tokenizer = Tokenizer.init("13-4 2x 99red-balloons");
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "13-4", .line = 1, .column = 1 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "2x", .line = 1, .column = 6 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.identifier, .lexeme = "99red-balloons", .line = 1, .column = 9 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(null, tokenizer.nextToken());
}
