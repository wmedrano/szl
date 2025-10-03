const std = @import("std");
const testing = std.testing;

const Tokenizer = @This();

pub const TokenType = enum {
    // Delimiters
    left_paren,
    right_paren,
    // Literals
    number,
    string,
    symbol,
    boolean,
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
        '\'' => self.makeTokenAt(.quote, start_column),
        '`' => self.makeTokenAt(.quasiquote, start_column),
        ',' => if (self.match('@'))
            self.makeTokenAt(.unquote_splicing, start_column)
        else
            self.makeTokenAt(.unquote, start_column),
        '"' => self.scanString(start_column),
        '#' => self.scanBoolean(start_column),
        '-', '+' => if (self.isAtEnd() or isDelimiter(self.peek()))
            self.makeTokenAt(.symbol, start_column)
        else if (std.ascii.isDigit(self.peek()))
            self.scanNumber(start_column)
        else
            self.scanSymbol(start_column),
        '0'...'9' => self.scanNumber(start_column),
        else => self.scanSymbol(start_column),
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

fn scanNumber(self: *Tokenizer, start_column: u32) Token {
    while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
        _ = self.advance();
    }

    // Look for decimal part
    if (!self.isAtEnd() and self.peek() == '.') {
        const next = if (self.current + 1 < self.source.len)
            self.source[self.current + 1]
        else
            0;
        if (std.ascii.isDigit(next)) {
            _ = self.advance(); // consume '.'
            while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
        }
    }

    return self.makeTokenAt(.number, start_column);
}

fn scanBoolean(self: *Tokenizer, start_column: u32) Token {
    // #t, #f, #true, or #false
    while (!self.isAtEnd() and !isDelimiter(self.peek())) {
        _ = self.advance();
    }
    return self.makeTokenAt(.boolean, start_column);
}

fn scanSymbol(self: *Tokenizer, start_column: u32) Token {
    while (!self.isAtEnd() and !isDelimiter(self.peek())) {
        _ = self.advance();
    }
    return self.makeTokenAt(.symbol, start_column);
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
    return .{
        .type = token_type,
        .lexeme = self.source[self.start..self.current],
        .line = self.line,
        .column = self.column - (self.current - self.start),
    };
}

fn makeTokenAt(self: *Tokenizer, token_type: TokenType, start_column: u32) Token {
    return .{
        .type = token_type,
        .lexeme = self.source[self.start..self.current],
        .line = self.line,
        .column = start_column,
    };
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

test "tokenize numbers" {
    var tokenizer = Tokenizer.init("123 45.67");
    try testing.expectEqualDeep(
        Token{ .type = TokenType.number, .lexeme = "123", .line = 1, .column = 1 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.number, .lexeme = "45.67", .line = 1, .column = 5 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(null, tokenizer.nextToken());
}

test "tokenize symbols" {
    var tokenizer = Tokenizer.init("+ define a-lambda");
    try testing.expectEqualDeep(
        Token{ .type = TokenType.symbol, .lexeme = "+", .line = 1, .column = 1 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.symbol, .lexeme = "define", .line = 1, .column = 3 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.symbol, .lexeme = "a-lambda", .line = 1, .column = 10 },
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

test "tokenize booleans" {
    var tokenizer = Tokenizer.init("#t #f #true #false #badboolean");
    try testing.expectEqualDeep(
        Token{ .type = TokenType.boolean, .lexeme = "#t", .line = 1, .column = 1 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.boolean, .lexeme = "#f", .line = 1, .column = 4 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.boolean, .lexeme = "#true", .line = 1, .column = 7 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.boolean, .lexeme = "#false", .line = 1, .column = 13 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.boolean, .lexeme = "#badboolean", .line = 1, .column = 20 },
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
        Token{ .type = TokenType.symbol, .lexeme = "x", .line = 1, .column = 2 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.quasiquote, .lexeme = "`", .line = 1, .column = 4 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.symbol, .lexeme = "x", .line = 1, .column = 5 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.unquote, .lexeme = ",", .line = 1, .column = 7 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.symbol, .lexeme = "x", .line = 1, .column = 8 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.unquote_splicing, .lexeme = ",@", .line = 1, .column = 10 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.symbol, .lexeme = "x", .line = 1, .column = 12 },
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
        Token{ .type = TokenType.symbol, .lexeme = "+", .line = 2, .column = 2 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.number, .lexeme = "1", .line = 2, .column = 4 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.number, .lexeme = "2", .line = 2, .column = 6 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(
        Token{ .type = TokenType.right_paren, .lexeme = ")", .line = 2, .column = 7 },
        tokenizer.nextToken(),
    );
    try testing.expectEqualDeep(null, tokenizer.nextToken());
}
