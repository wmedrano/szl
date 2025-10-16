const std = @import("std");
const testing = std.testing;

const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");
const Tokenizer = @import("Tokenizer.zig");

const Reader = @This();

tokenizer: Tokenizer,
vm: *Vm,
last_token: ?Tokenizer.Token = null,

/// Diagnostic information for reader errors
pub const Diagnostic = struct {
    /// The type of error that occurred
    kind: Kind = .unexpected_token,
    /// Line number where the error occurred (1-indexed)
    line: u32 = 0,
    /// Column number where the error occurred (1-indexed)
    column: u32 = 0,
    /// Optional context information about the error
    message: []const u8 = "",
    /// Optional lexeme that caused the error
    lexeme: ?[]const u8 = null,

    pub const Kind = enum {
        unexpected_eof,
        unexpected_token,
        invalid_number,
        invalid_string,
        invalid_character,
        invalid_bytevector,
        unexpected_dot,
        unexpected_close_paren,
        empty_dot_list,
        multiple_values,
        no_values,
    };

    pub fn format(
        self: Diagnostic,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        if (self.lexeme) |lex| {
            try writer.print("{}:{}: error: {s}: {s}: '{s}'", .{
                self.line,
                self.column,
                @tagName(self.kind),
                self.message,
                lex,
            });
        } else {
            try writer.print("{}:{}: error: {s}: {s}", .{
                self.line,
                self.column,
                @tagName(self.kind),
                self.message,
            });
        }
    }
};

pub const Error = error{
    ReadError,
    NotImplemented,
    OutOfMemory,
};

pub const ReadOneError = Vm.Error || error{
    NoValue,
    TooManyValues,
};

pub fn init(vm: *Vm, source: []const u8) Reader {
    return Reader{
        .vm = vm,
        .tokenizer = Tokenizer.init(source),
        .last_token = null,
    };
}

pub fn readNext(self: *Reader, diagnostic: ?*Diagnostic) Error!?Val {
    const result = try self.readNextImpl(diagnostic);
    switch (result) {
        .atom => |v| return v,
        .end_expr => {
            if (diagnostic) |diag| {
                const token = self.last_token orelse Tokenizer.Token{ .type = .right_paren, .lexeme = ")", .line = 1, .column = 1 };
                diag.* = .{
                    .kind = .unexpected_close_paren,
                    .line = token.line,
                    .column = token.column,
                    .message = "unexpected closing parenthesis",
                };
            }
            return Error.ReadError;
        },
        .dot => {
            if (diagnostic) |diag| {
                const token = self.last_token orelse Tokenizer.Token{ .type = .dot, .lexeme = ".", .line = 1, .column = 1 };
                diag.* = .{
                    .kind = .unexpected_dot,
                    .line = token.line,
                    .column = token.column,
                    .message = "unexpected dot notation",
                };
            }
            return Error.ReadError;
        },
        .end => return null,
    }
}

pub fn readOne(vm: *Vm, source: []const u8, diagnostic: ?*Diagnostic) ReadOneError!Val {
    var reader = Reader.init(vm, source);
    const first = try reader.readNext(diagnostic) orelse {
        if (diagnostic) |diag| {
            diag.* = .{
                .kind = .no_values,
                .line = 1,
                .column = 1,
                .message = "expected at least one value",
            };
        }
        return ReadOneError.NoValue;
    };
    if (try reader.readNext(diagnostic)) |_| {
        if (diagnostic) |diag| {
            const token = reader.last_token orelse Tokenizer.Token{ .type = .left_paren, .lexeme = "", .line = 1, .column = 1 };
            diag.* = .{
                .kind = .multiple_values,
                .line = token.line,
                .column = token.column,
                .message = "expected only one value, found multiple",
            };
        }
        return ReadOneError.TooManyValues;
    }
    return first;
}

const ReadResult = union(enum) {
    atom: Val,
    end_expr,
    dot,
    end,
};

fn readNextImpl(self: *Reader, diagnostic: ?*Diagnostic) Error!ReadResult {
    const next_token = self.tokenizer.nextToken() orelse
        return ReadResult{ .end = {} };
    self.last_token = next_token;
    switch (next_token.type) {
        .left_paren => return try self.readList(next_token, diagnostic),
        .right_paren => return ReadResult{ .end_expr = {} },
        .dot => return ReadResult{ .dot = {} },
        .bytevector_start => return try self.readBytevector(next_token, diagnostic),
        .vector_start => return try self.readVector(next_token, diagnostic),
        .identifier => {
            // Classify the identifier based on its lexeme
            const lexeme = next_token.lexeme;

            // Check if it starts with '#' (sharpsign literal)
            if (lexeme.len > 0 and lexeme[0] == '#') {
                return try self.parseSharpsign(next_token, diagnostic);
            }

            // Check if it's a number (starts with digit, +digit, -digit, or just + or -)
            if (lexeme.len > 0) {
                const first = lexeme[0];
                if (std.ascii.isDigit(first)) {
                    return try self.parseNumber(next_token, diagnostic);
                } else if ((first == '+' or first == '-')) {
                    if (lexeme.len == 1) {
                        // Just + or - by itself is a symbol
                        return try self.parseSymbol(next_token);
                    } else if (std.ascii.isDigit(lexeme[1])) {
                        // +/- followed by digit is a number
                        return try self.parseNumber(next_token, diagnostic);
                    }
                }
            }

            // Otherwise, it's a symbol
            return try self.parseSymbol(next_token);
        },
        .string => return try self.parseString(next_token, diagnostic),
        .quote => return try self.readQuote(next_token, diagnostic),
        .quasiquote => {
            if (diagnostic) |diag| {
                diag.* = .{
                    .kind = .unexpected_token,
                    .line = next_token.line,
                    .column = next_token.column,
                    .message = "quasiquote syntax is not yet implemented",
                    .lexeme = next_token.lexeme,
                };
            }
            return Error.NotImplemented;
        },
        .unquote => {
            if (diagnostic) |diag| {
                diag.* = .{
                    .kind = .unexpected_token,
                    .line = next_token.line,
                    .column = next_token.column,
                    .message = "unquote syntax is not yet implemented",
                    .lexeme = next_token.lexeme,
                };
            }
            return Error.NotImplemented;
        },
        .unquote_splicing => {
            if (diagnostic) |diag| {
                diag.* = .{
                    .kind = .unexpected_token,
                    .line = next_token.line,
                    .column = next_token.column,
                    .message = "unquote-splicing syntax is not yet implemented",
                    .lexeme = next_token.lexeme,
                };
            }
            return Error.NotImplemented;
        },
    }
}

fn readList(self: *Reader, open_paren_token: Tokenizer.Token, diagnostic: ?*Diagnostic) Error!ReadResult {
    var elements = std.ArrayList(Val){};
    defer elements.deinit(self.vm.allocator());
    var dot_idx: ?usize = null;
    const builder = self.vm.builder();

    while (true) {
        switch (try self.readNextImpl(diagnostic)) {
            .atom => |v| try elements.append(self.vm.allocator(), v),
            .end_expr => {
                const res = if (dot_idx == elements.items.len)
                    try builder.makePairs(elements.items)
                else
                    try builder.makeList(elements.items);
                return ReadResult{ .atom = res };
            },
            .end => {
                if (diagnostic) |diag| {
                    diag.* = .{
                        .kind = .unexpected_eof,
                        .line = open_paren_token.line,
                        .column = open_paren_token.column,
                        .message = "missing closing parenthesis for list",
                    };
                }
                return Error.ReadError;
            },
            .dot => {
                if (elements.items.len == 0) {
                    if (diagnostic) |diag| {
                        const token = self.last_token orelse open_paren_token;
                        diag.* = .{
                            .kind = .empty_dot_list,
                            .line = token.line,
                            .column = token.column,
                            .message = "dot notation requires at least one element before the dot",
                        };
                    }
                    return Error.ReadError;
                }
                dot_idx = elements.items.len + 1;
            },
        }
    }
}

fn readBytevector(self: *Reader, open_token: Tokenizer.Token, diagnostic: ?*Diagnostic) Error!ReadResult {
    var bytes = std.ArrayList(u8){};
    defer bytes.deinit(self.vm.allocator());
    const builder = self.vm.builder();

    while (true) {
        switch (try self.readNextImpl(diagnostic)) {
            .atom => |v| {
                // Parse value as u8 (0-255)
                const int_val = v.asInt() orelse {
                    if (diagnostic) |diag| {
                        const token = self.last_token orelse open_token;
                        diag.* = .{
                            .kind = .invalid_bytevector,
                            .line = token.line,
                            .column = token.column,
                            .message = "bytevector elements must be integers",
                        };
                    }
                    return Error.ReadError;
                };
                if (int_val < 0 or int_val > 255) {
                    if (diagnostic) |diag| {
                        const token = self.last_token orelse open_token;
                        diag.* = .{
                            .kind = .invalid_bytevector,
                            .line = token.line,
                            .column = token.column,
                            .message = "bytevector elements must be in range 0-255",
                        };
                    }
                    return Error.ReadError;
                }
                try bytes.append(self.vm.allocator(), @intCast(int_val));
            },
            .end_expr => {
                const bv = try builder.makeBytevector(bytes.items);
                return ReadResult{ .atom = bv };
            },
            .end => {
                if (diagnostic) |diag| {
                    diag.* = .{
                        .kind = .unexpected_eof,
                        .line = open_token.line,
                        .column = open_token.column,
                        .message = "missing closing parenthesis for bytevector",
                    };
                }
                return Error.ReadError;
            },
            .dot => {
                if (diagnostic) |diag| {
                    const token = self.last_token orelse open_token;
                    diag.* = .{
                        .kind = .unexpected_dot,
                        .line = token.line,
                        .column = token.column,
                        .message = "bytevectors do not support dot notation",
                    };
                }
                return Error.ReadError;
            },
        }
    }
}

fn readVector(self: *Reader, open_token: Tokenizer.Token, diagnostic: ?*Diagnostic) Error!ReadResult {
    var elements = std.ArrayList(Val){};
    defer elements.deinit(self.vm.allocator());
    const builder = self.vm.builder();

    while (true) {
        switch (try self.readNextImpl(diagnostic)) {
            .atom => |v| try elements.append(self.vm.allocator(), v),
            .end_expr => {
                const vec = try builder.makeVector(elements.items);
                return ReadResult{ .atom = vec };
            },
            .end => {
                if (diagnostic) |diag| {
                    diag.* = .{
                        .kind = .unexpected_eof,
                        .line = open_token.line,
                        .column = open_token.column,
                        .message = "missing closing parenthesis for vector",
                    };
                }
                return Error.ReadError;
            },
            .dot => {
                if (diagnostic) |diag| {
                    const token = self.last_token orelse open_token;
                    diag.* = .{
                        .kind = .unexpected_dot,
                        .line = token.line,
                        .column = token.column,
                        .message = "vectors do not support dot notation",
                    };
                }
                return Error.ReadError;
            },
        }
    }
}

fn readQuote(self: *Reader, quote_token: Tokenizer.Token, diagnostic: ?*Diagnostic) Error!ReadResult {
    const builder = self.vm.builder();
    const next = try self.readNextImpl(diagnostic);

    switch (next) {
        .atom => |v| {
            const quote_symbol = try builder.makeStaticSymbol("quote");
            const list_items = [_]Val{ quote_symbol, v };
            const quoted = try builder.makeList(&list_items);
            return ReadResult{ .atom = quoted };
        },
        .end => {
            if (diagnostic) |diag| {
                diag.* = .{
                    .kind = .unexpected_eof,
                    .line = quote_token.line,
                    .column = quote_token.column,
                    .message = "expected value after quote",
                };
            }
            return Error.ReadError;
        },
        .end_expr => {
            if (diagnostic) |diag| {
                const token = self.last_token orelse quote_token;
                diag.* = .{
                    .kind = .unexpected_close_paren,
                    .line = token.line,
                    .column = token.column,
                    .message = "expected value after quote, found closing parenthesis",
                };
            }
            return Error.ReadError;
        },
        .dot => {
            if (diagnostic) |diag| {
                const token = self.last_token orelse quote_token;
                diag.* = .{
                    .kind = .unexpected_dot,
                    .line = token.line,
                    .column = token.column,
                    .message = "expected value after quote, found dot",
                };
            }
            return Error.ReadError;
        },
    }
}

fn parseSharpsign(self: Reader, token: Tokenizer.Token, diagnostic: ?*Diagnostic) Error!ReadResult {
    const lexeme = token.lexeme;

    // Lexeme must start with '#'
    if (lexeme.len == 0 or lexeme[0] != '#') {
        if (diagnostic) |diag| {
            diag.* = .{
                .kind = .unexpected_token,
                .line = token.line,
                .column = token.column,
                .message = "invalid sharpsign literal",
                .lexeme = lexeme,
            };
        }
        return Error.ReadError;
    }

    // Check if it's a character literal (#\...)
    if (lexeme.len >= 2 and lexeme[1] == '\\') {
        return self.parseCharacter(token, diagnostic);
    }

    // Check if it's a boolean (#t, #f, #true, #false)
    if (std.mem.eql(u8, lexeme, "#t") or std.mem.eql(u8, lexeme, "#true")) {
        return ReadResult{ .atom = Val.initBool(true) };
    }
    if (std.mem.eql(u8, lexeme, "#f") or std.mem.eql(u8, lexeme, "#false")) {
        return ReadResult{ .atom = Val.initBool(false) };
    }

    // If we get here, it's an invalid sharpsign literal
    if (diagnostic) |diag| {
        diag.* = .{
            .kind = .unexpected_token,
            .line = token.line,
            .column = token.column,
            .message = "invalid sharpsign literal",
            .lexeme = lexeme,
        };
    }
    return Error.ReadError;
}

fn parseNumber(_: Reader, token: Tokenizer.Token, diagnostic: ?*Diagnostic) Error!ReadResult {
    const lexeme = token.lexeme;
    if (lexeme.len == 0) {
        if (diagnostic) |diag| {
            diag.* = .{
                .kind = .invalid_number,
                .line = token.line,
                .column = token.column,
                .message = "empty number literal",
            };
        }
        return Error.ReadError;
    }

    var is_negative = false;
    var start_idx: usize = 0;

    // Handle leading sign
    if (lexeme[0] == '-') {
        is_negative = true;
        start_idx = 1;
    } else if (lexeme[0] == '+') {
        start_idx = 1;
    }

    if (start_idx >= lexeme.len) {
        if (diagnostic) |diag| {
            diag.* = .{
                .kind = .invalid_number,
                .line = token.line,
                .column = token.column,
                .message = "number literal contains only sign character",
                .lexeme = lexeme,
            };
        }
        return Error.ReadError;
    }

    // Check if this is a float (contains a decimal point)
    var has_decimal = false;
    for (lexeme[start_idx..]) |c| {
        if (c == '.') {
            has_decimal = true;
            break;
        }
    }

    if (has_decimal) {
        // Parse as float
        const f = std.fmt.parseFloat(f64, lexeme) catch {
            if (diagnostic) |diag| {
                diag.* = .{
                    .kind = .invalid_number,
                    .line = token.line,
                    .column = token.column,
                    .message = "invalid floating-point number",
                    .lexeme = lexeme,
                };
            }
            return Error.ReadError;
        };
        const val = Val.initFloat(f);
        return ReadResult{ .atom = val };
    } // Parse as integer
    var n: i64 = 0;
    for (lexeme[start_idx..]) |digit| {
        if (digit < '0' or digit > '9') {
            if (diagnostic) |diag| {
                diag.* = .{
                    .kind = .invalid_number,
                    .line = token.line,
                    .column = token.column,
                    .message = "invalid digit in integer literal",
                    .lexeme = lexeme,
                };
            }
            return Error.ReadError;
        }
        n *= 10;
        n += digit - '0';
    }

    if (is_negative) n = -n;

    const val = Val.initInt(n);
    return ReadResult{ .atom = val };
}

fn parseSymbol(self: Reader, token: Tokenizer.Token) Error!ReadResult {
    const symbol = try self.vm.builder().makeSymbol(token.lexeme);
    return ReadResult{ .atom = symbol };
}

fn parseString(self: Reader, token: Tokenizer.Token, diagnostic: ?*Diagnostic) Error!ReadResult {
    const lexeme = token.lexeme;
    // Token includes the quotes, so we need to strip them
    if (lexeme.len < 2 or lexeme[0] != '"' or lexeme[lexeme.len - 1] != '"') {
        if (diagnostic) |diag| {
            diag.* = .{
                .kind = .invalid_string,
                .line = token.line,
                .column = token.column,
                .message = "malformed string literal",
            };
        }
        return Error.ReadError;
    }

    const content = lexeme[1 .. lexeme.len - 1];

    // Process escape sequences
    var unescaped = std.ArrayList(u8){};
    defer unescaped.deinit(self.vm.allocator());

    var i: usize = 0;
    while (i < content.len) : (i += 1) {
        if (content[i] == '\\' and i + 1 < content.len) {
            // Handle escape sequences
            i += 1;
            const escaped_char = switch (content[i]) {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '\\' => '\\',
                '"' => '"',
                else => content[i], // Unknown escape, just use the character
            };
            try unescaped.append(self.vm.allocator(), escaped_char);
        } else {
            try unescaped.append(self.vm.allocator(), content[i]);
        }
    }

    const string_val = try self.vm.builder().makeString(unescaped.items);
    return ReadResult{ .atom = string_val };
}

fn parseCharacter(_: Reader, token: Tokenizer.Token, diagnostic: ?*Diagnostic) Error!ReadResult {
    const lexeme = token.lexeme;
    // Token format: #\<character>
    // Examples: #\a, #\space, #\newline, #\x3BB
    if (lexeme.len < 3 or lexeme[0] != '#' or lexeme[1] != '\\') {
        if (diagnostic) |diag| {
            diag.* = .{
                .kind = .invalid_character,
                .line = token.line,
                .column = token.column,
                .message = "malformed character literal",
            };
        }
        return Error.ReadError;
    }

    const char_part = lexeme[2..];

    // Handle single char
    if (char_part.len == 1)
        return ReadResult{ .atom = Val.initChar(char_part[0]) };

    // Handle named characters
    const named_characters = std.StaticStringMap(u21).initComptime(.{
        .{ "alarm", 0x0007 },
        .{ "backspace", 0x0008 },
        .{ "delete", 0x007F },
        .{ "escape", 0x001B },
        .{ "newline", 0x000A },
        .{ "null", 0x0000 },
        .{ "return", 0x000D },
        .{ "space", ' ' },
        .{ "tab", 0x0009 },
    });
    if (named_characters.get(char_part)) |value|
        return ReadResult{ .atom = Val.initChar(value) };

    // Handle hex
    if (char_part.len > 1 and char_part[0] == 'x') {
        // Hex escape: #\x3BB
        const hex_str = char_part[1..];
        if (hex_str.len == 0) {
            if (diagnostic) |diag| {
                diag.* = .{
                    .kind = .invalid_character,
                    .line = token.line,
                    .column = token.column,
                    .message = "hex character escape requires at least one digit",
                };
            }
            return Error.ReadError;
        }
        const int_ch = std.fmt.parseInt(u21, hex_str, 16) catch {
            if (diagnostic) |diag| {
                diag.* = .{
                    .kind = .invalid_character,
                    .line = token.line,
                    .column = token.column,
                    .message = "invalid hex character escape",
                };
            }
            return Error.ReadError;
        };
        return ReadResult{ .atom = Val.initChar(int_ch) };
    }

    // Unknown named character or invalid format
    if (diagnostic) |diag| {
        diag.* = .{
            .kind = .invalid_character,
            .line = token.line,
            .column = token.column,
            .message = "unknown named character",
            .lexeme = lexeme,
        };
    }
    return Error.ReadError;
}

fn expectReadNext(self: *Reader, expect: ?[]const u8, vm: *const Vm) !void {
    const end_of_read = "end_of_read";
    const expect_normalized = expect orelse end_of_read;
    if (try self.readNext(null)) |next| {
        const pretty = vm.pretty(next);
        try testing.expectFmt(
            expect_normalized,
            "{f}",
            .{pretty},
        );
    } else {
        try testing.expectEqualStrings(expect_normalized, end_of_read);
    }
}

test "read int" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "1 009");
    try reader.expectReadNext("1", &vm);
    try reader.expectReadNext("9", &vm);
    try reader.expectReadNext(null, &vm);
}

test "read list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "() (1 2 3) (1 . 2 3)");
    try reader.expectReadNext("()", &vm);
    try reader.expectReadNext("(1 2 3)", &vm);
    try reader.expectReadNext("(1 2 3)", &vm);
    try reader.expectReadNext(null, &vm);
}

test "read pair" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var reader = Reader.init(&vm, "(1 . 2) (1 2 . 3)");

    try reader.expectReadNext("(1 . 2)", &vm);
    try reader.expectReadNext("(1 2 . 3)", &vm);
    try reader.expectReadNext(null, &vm);
}

test "read symbol" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "foo bar-baz");
    try reader.expectReadNext("foo", &vm);
    try reader.expectReadNext("bar-baz", &vm);
    try reader.expectReadNext(null, &vm);
}

test "read boolean" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "#t #f #true #false");
    try reader.expectReadNext("#t", &vm);
    try reader.expectReadNext("#f", &vm);
    try reader.expectReadNext("#t", &vm);
    try reader.expectReadNext("#f", &vm);
    try reader.expectReadNext(null, &vm);
}

test "read character" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "#\\a #\\Z #\\space #\\newline #\\x41");
    try reader.expectReadNext("#\\a", &vm);
    try reader.expectReadNext("#\\Z", &vm);
    try reader.expectReadNext("#\\space", &vm);
    try reader.expectReadNext("#\\newline", &vm);
    try reader.expectReadNext("#\\A", &vm); // #\x41 is 'A'
    try reader.expectReadNext(null, &vm);
}

test "read vectors" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "#() #(1 2 3)");
    try reader.expectReadNext("#()", &vm);
    try reader.expectReadNext("#(1 2 3)", &vm);
    try reader.expectReadNext(null, &vm);
}

test "read vector with mixed types" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "#(1 \"hello\" foo #t)");
    try reader.expectReadNext("#(1 \"hello\" foo #t)", &vm);
    try reader.expectReadNext(null, &vm);
}

test "read nested vectors" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "#(1 #(2 3) 4)");
    try reader.expectReadNext("#(1 #(2 3) 4)", &vm);
    try reader.expectReadNext(null, &vm);
}

test "read string" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "\"hello\" \"\" \"world\"");
    try reader.expectReadNext("\"hello\"", &vm);
    try reader.expectReadNext("\"\"", &vm);
    try reader.expectReadNext("\"world\"", &vm);
    try reader.expectReadNext(null, &vm);
}

test "read string with escapes" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Pretty printer outputs the actual escaped characters, not the escape sequences
    var reader = Reader.init(&vm, "\"line1\\nline2\" \"tab\\there\" \"quote\\\"inside\"");
    const val1 = (try reader.readNext(null)).?;
    const val2 = (try reader.readNext(null)).?;
    const val3 = (try reader.readNext(null)).?;
    try reader.expectReadNext(null, &vm);

    // Verify the strings contain the actual escaped characters
    const str1 = vm.objects.strings.get(val1.data.string).?.asSlice();
    const str2 = vm.objects.strings.get(val2.data.string).?.asSlice();
    const str3 = vm.objects.strings.get(val3.data.string).?.asSlice();

    try testing.expectEqualStrings("line1\nline2", str1);
    try testing.expectEqualStrings("tab\there", str2);
    try testing.expectEqualStrings("quote\"inside", str3);
}

test "read float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "3.14 -2.5 +0.5");
    try reader.expectReadNext("3.14", &vm);
    try reader.expectReadNext("-2.5", &vm);
    try reader.expectReadNext("0.5", &vm);
    try reader.expectReadNext(null, &vm);
}

test "read negative int" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "-42 +100");
    try reader.expectReadNext("-42", &vm);
    try reader.expectReadNext("100", &vm);
    try reader.expectReadNext(null, &vm);
}

test "read quote" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "'x '(1 2) ''nested");
    try reader.expectReadNext("(quote x)", &vm);
    try reader.expectReadNext("(quote (1 2))", &vm);
    try reader.expectReadNext("(quote (quote nested))", &vm);
    try reader.expectReadNext(null, &vm);
}

test "read bytevector" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "#u8() #u8(0 1 255)");
    try reader.expectReadNext("#u8()", &vm);
    try reader.expectReadNext("#u8(0 1 255)", &vm);
    try reader.expectReadNext(null, &vm);
}

test "read special symbols" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "+ - +foo foo? foo! ->string");
    try reader.expectReadNext("+", &vm);
    try reader.expectReadNext("-", &vm);
    try reader.expectReadNext("+foo", &vm);
    try reader.expectReadNext("foo?", &vm);
    try reader.expectReadNext("foo!", &vm);
    try reader.expectReadNext("->string", &vm);
    try reader.expectReadNext(null, &vm);
}

test "read character named variants" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "#\\alarm #\\backspace #\\delete #\\escape #\\null #\\return #\\tab");
    try reader.expectReadNext("#\\alarm", &vm);
    try reader.expectReadNext("#\\backspace", &vm);
    try reader.expectReadNext("#\\delete", &vm);
    try reader.expectReadNext("#\\escape", &vm);
    try reader.expectReadNext("#\\null", &vm);
    try reader.expectReadNext("#\\return", &vm);
    try reader.expectReadNext("#\\tab", &vm);
    try reader.expectReadNext(null, &vm);
}

test "error unclosed list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var diag: Diagnostic = undefined;
    var reader = Reader.init(&vm, "(1 2 3");
    const result = reader.readNext(&diag);
    try testing.expectError(Error.ReadError, result);
    try testing.expectEqual(Diagnostic.Kind.unexpected_eof, diag.kind);
}

test "error unexpected close paren" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var diag: Diagnostic = undefined;
    var reader = Reader.init(&vm, "1)");
    _ = try reader.readNext(null);
    const result = reader.readNext(&diag);
    try testing.expectError(Error.ReadError, result);
    try testing.expectEqual(Diagnostic.Kind.unexpected_close_paren, diag.kind);
}

test "error empty dot list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var diag: Diagnostic = undefined;
    var reader = Reader.init(&vm, "(. 1)");
    const result = reader.readNext(&diag);
    try testing.expectError(Error.ReadError, result);
    try testing.expectEqual(Diagnostic.Kind.empty_dot_list, diag.kind);
}

test "error unexpected dot" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var diag: Diagnostic = undefined;
    var reader = Reader.init(&vm, ".");
    const result = reader.readNext(&diag);
    try testing.expectError(Error.ReadError, result);
    try testing.expectEqual(Diagnostic.Kind.unexpected_dot, diag.kind);
}

test "error readOne no values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var diag: Diagnostic = undefined;
    const result = Reader.readOne(&vm, "", &diag);
    try testing.expectError(ReadOneError.NoValue, result);
    try testing.expectEqual(Diagnostic.Kind.no_values, diag.kind);
}

test "error readOne multiple values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var diag: Diagnostic = undefined;
    const result = Reader.readOne(&vm, "1 2", &diag);
    try testing.expectError(ReadOneError.TooManyValues, result);
    try testing.expectEqual(Diagnostic.Kind.multiple_values, diag.kind);
}

test "error invalid bytevector" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var diag: Diagnostic = undefined;
    var reader = Reader.init(&vm, "#u8(256)");
    const result = reader.readNext(&diag);
    try testing.expectError(Error.ReadError, result);
    try testing.expectEqual(Diagnostic.Kind.invalid_bytevector, diag.kind);
}

test "error invalid sharpsign literal" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var diag: Diagnostic = undefined;
    var reader = Reader.init(&vm, "#invalid");
    const result = reader.readNext(&diag);
    try testing.expectError(Error.ReadError, result);
    try testing.expectEqual(Diagnostic.Kind.unexpected_token, diag.kind);
    try testing.expectEqualStrings("invalid sharpsign literal", diag.message);
}

test "error bare sharpsign" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var diag: Diagnostic = undefined;
    var reader = Reader.init(&vm, "#");
    const result = reader.readNext(&diag);
    try testing.expectError(Error.ReadError, result);
    try testing.expectEqual(Diagnostic.Kind.unexpected_token, diag.kind);
    try testing.expectEqualStrings("invalid sharpsign literal", diag.message);
}
