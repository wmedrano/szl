const std = @import("std");
const testing = std.testing;

const ErrorDetails = @import("../types/ErrorDetails.zig");
const Rational = @import("../types/number.zig").Rational;
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const SmallArrayList = @import("../utils/small_array_list.zig").SmallArrayList;
const Vm = @import("../Vm.zig");
const Tokenizer = @import("Tokenizer.zig");

const Reader = @This();

tokenizer: Tokenizer,
vm: *Vm,
last_token: ?Tokenizer.Token = null,

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

pub fn readNext(self: *Reader, error_details: *ErrorDetails) Error!?Val {
    const result = try self.readNextImpl(error_details);
    switch (result) {
        .atom => |a| return a.val,
        .end_expr => |token| {
            @branchHint(.cold);
            error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                .kind = .unexpected_close_paren,
                .line = token.line,
                .column = token.column,
                .message = "unexpected closing parenthesis",
                .lexeme = token.lexeme,
            } });
            return Error.ReadError;
        },
        .dot => |token| {
            @branchHint(.cold);
            error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                .kind = .unexpected_dot,
                .line = token.line,
                .column = token.column,
                .message = "unexpected dot notation",
                .lexeme = token.lexeme,
            } });
            return Error.ReadError;
        },
        .end => return null,
    }
}

pub fn readOne(vm: *Vm, source: []const u8, error_details: *ErrorDetails) ReadOneError!Val {
    var reader = Reader.init(vm, source);
    const first = try reader.readNext(error_details) orelse {
        @branchHint(.cold);
        error_details.addDiagnostic(vm.allocator(), .{ .reader = .{
            .kind = .no_values,
            .line = 1,
            .column = 1,
            .message = "expected at least one value",
            .lexeme = null,
        } });
        return ReadOneError.NoValue;
    };
    if (try reader.readNext(error_details)) |_| {
        @branchHint(.cold);
        const token = reader.last_token orelse Tokenizer.Token{ .type = .left_paren, .lexeme = "", .line = 1, .column = 1 };
        error_details.addDiagnostic(vm.allocator(), .{ .reader = .{
            .kind = .multiple_values,
            .line = token.line,
            .column = token.column,
            .message = "expected only one value, found multiple",
        } });
        return ReadOneError.TooManyValues;
    }
    return first;
}

const ReadResult = union(enum) {
    atom: struct {
        val: Val,
        token: Tokenizer.Token,
    },
    end_expr: Tokenizer.Token,
    dot: Tokenizer.Token,
    end,
};

inline fn readNextImpl(self: *Reader, error_details: *ErrorDetails) Error!ReadResult {
    const next_token = self.tokenizer.nextToken() orelse
        return ReadResult{ .end = {} };
    self.last_token = next_token;
    switch (next_token.type) {
        .left_paren => return try self.readList(next_token, error_details),
        .right_paren => return ReadResult{ .end_expr = next_token },
        .dot => return ReadResult{ .dot = next_token },
        .sharpsign_expr_start => return try self.readSharpsignExpr(next_token, error_details),
        .identifier => {
            // Classify the identifier based on its lexeme
            const lexeme = next_token.lexeme;

            // Check if it starts with '#' (sharpsign literal)
            if (lexeme.len > 0 and lexeme[0] == '#') {
                return try self.parseSharpsign(next_token, error_details);
            }

            // Check for special float literals
            if (std.mem.eql(u8, lexeme, "+inf.0") or std.mem.eql(u8, lexeme, "+Inf.0")) {
                return ReadResult{ .atom = .{
                    .val = Val.initFloat(std.math.inf(f64)),
                    .token = next_token,
                } };
            }
            if (std.mem.eql(u8, lexeme, "-inf.0") or std.mem.eql(u8, lexeme, "-Inf.0")) {
                return ReadResult{ .atom = .{
                    .val = Val.initFloat(-std.math.inf(f64)),
                    .token = next_token,
                } };
            }
            if (std.mem.eql(u8, lexeme, "+nan.0") or std.mem.eql(u8, lexeme, "+NaN.0")) {
                return ReadResult{ .atom = .{
                    .val = Val.initFloat(std.math.nan(f64)),
                    .token = next_token,
                } };
            }

            // Check if it's a number (starts with digit, +digit, -digit, or just + or -)
            if (lexeme.len > 0) {
                const first = lexeme[0];
                if (std.ascii.isDigit(first)) {
                    return try self.parseNumber(next_token, error_details);
                } else if ((first == '+' or first == '-')) {
                    if (lexeme.len == 1) {
                        // Just + or - by itself is a symbol
                        return try self.parseSymbol(next_token);
                    } else if (std.ascii.isDigit(lexeme[1])) {
                        // +/- followed by digit is a number
                        return try self.parseNumber(next_token, error_details);
                    }
                }
            }

            // Otherwise, it's a symbol
            return try self.parseSymbol(next_token);
        },
        .string => return try self.parseString(next_token, error_details),
        .quote => return try self.readQuote(next_token, error_details),
        .quasiquote => {
            @branchHint(.cold);
            error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                .kind = .unexpected_token,
                .line = next_token.line,
                .column = next_token.column,
                .message = "quasiquote syntax is not yet implemented",
                .lexeme = next_token.lexeme,
            } });
            return Error.NotImplemented;
        },
        .unquote => {
            @branchHint(.cold);
            error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                .kind = .unexpected_token,
                .line = next_token.line,
                .column = next_token.column,
                .message = "unquote syntax is not yet implemented",
                .lexeme = next_token.lexeme,
            } });
            return Error.NotImplemented;
        },
        .unquote_splicing => {
            @branchHint(.cold);
            error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                .kind = .unexpected_token,
                .line = next_token.line,
                .column = next_token.column,
                .message = "unquote-splicing syntax is not yet implemented",
                .lexeme = next_token.lexeme,
            } });
            return Error.NotImplemented;
        },
    }
}

fn readList(self: *Reader, open_paren_token: Tokenizer.Token, error_details: *ErrorDetails) Error!ReadResult {
    var elements = SmallArrayList(Val, 8){};
    defer elements.deinit(self.vm.allocator());
    var dot_idx: ?usize = null;
    const builder = self.vm.builder();

    while (true) {
        switch (try self.readNextImpl(error_details)) {
            .atom => |a| try elements.append(self.vm.allocator(), a.val),
            .end_expr => {
                const res = if (dot_idx == elements.len)
                    try builder.makePairs(elements.asSlice())
                else
                    try builder.makeList(elements.asSlice());
                return ReadResult{ .atom = .{
                    .val = res,
                    .token = open_paren_token,
                } };
            },
            .end => {
                @branchHint(.cold);
                error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                    .kind = .unexpected_eof,
                    .line = open_paren_token.line,
                    .column = open_paren_token.column,
                    .message = "missing closing parenthesis for list",
                    .hint = "add ')' to close the list",
                } });
                return Error.ReadError;
            },
            .dot => |dot_token| {
                @branchHint(.cold);
                if (elements.len == 0) {
                    error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                        .kind = .empty_dot_list,
                        .line = dot_token.line,
                        .column = dot_token.column,
                        .message = "dot notation requires at least one element before the dot",
                        .hint = "add at least one element before the dot, e.g., (a . b)",
                        .lexeme = dot_token.lexeme,
                    } });
                    return Error.ReadError;
                }
                dot_idx = elements.len + 1;
            },
        }
    }
}

fn readSharpsignExpr(self: *Reader, open_token: Tokenizer.Token, error_details: *ErrorDetails) Error!ReadResult {
    const lexeme = open_token.lexeme;
    if (std.mem.eql(u8, lexeme, "#(")) {
        return try self.readVector(open_token, error_details);
    }
    if (std.mem.eql(u8, lexeme, "#u8(")) {
        return try self.readBytevector(open_token, error_details);
    }
    error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
        .kind = .unsupported_sharpsign_expr,
        .line = open_token.line,
        .column = open_token.column,
        .message = "unsupported sharpsign expression",
        .lexeme = lexeme,
        .hint = "supported forms are #( for vectors and #u8( for bytevectors",
    } });
    return Error.ReadError;
}

fn readBytevector(self: *Reader, open_token: Tokenizer.Token, error_details: *ErrorDetails) Error!ReadResult {
    var bytes = std.ArrayList(u8){};
    defer bytes.deinit(self.vm.allocator());
    const builder = self.vm.builder();

    while (true) {
        switch (try self.readNextImpl(error_details)) {
            .atom => |a| {
                // Parse value as u8 (0-255)
                const int_val = a.val.asInt() orelse {
                    @branchHint(.cold);
                    error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                        .kind = .invalid_bytevector,
                        .line = a.token.line,
                        .column = a.token.column,
                        .message = "bytevector elements must be integers",
                        .hint = "use only integer values in range 0-255",
                        .lexeme = a.token.lexeme,
                    } });
                    return Error.ReadError;
                };
                if (int_val < 0 or int_val > 255) {
                    @branchHint(.cold);
                    error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                        .kind = .invalid_bytevector,
                        .line = a.token.line,
                        .column = a.token.column,
                        .message = "bytevector elements must be in range 0-255",
                        .hint = "use values between 0 and 255",
                        .lexeme = a.token.lexeme,
                    } });
                    return Error.ReadError;
                }
                try bytes.append(self.vm.allocator(), @intCast(int_val));
            },
            .end_expr => {
                const bv = try builder.makeBytevector(bytes.items);
                return ReadResult{ .atom = .{
                    .val = bv,
                    .token = open_token,
                } };
            },
            .end => {
                @branchHint(.cold);
                error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                    .kind = .unexpected_eof,
                    .line = open_token.line,
                    .column = open_token.column,
                    .message = "missing closing parenthesis for bytevector",
                    .hint = "add ')' to close the bytevector",
                } });
                return Error.ReadError;
            },
            .dot => |dot_token| {
                @branchHint(.cold);
                error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                    .kind = .unexpected_dot,
                    .line = dot_token.line,
                    .column = dot_token.column,
                    .message = "bytevectors do not support dot notation",
                    .lexeme = dot_token.lexeme,
                } });
                return Error.ReadError;
            },
        }
    }
}

fn readVector(self: *Reader, open_token: Tokenizer.Token, error_details: *ErrorDetails) Error!ReadResult {
    var elements = SmallArrayList(Val, 8){};
    defer elements.deinit(self.vm.allocator());
    const builder = self.vm.builder();

    while (true) {
        switch (try self.readNextImpl(error_details)) {
            .atom => |a| try elements.append(self.vm.allocator(), a.val),
            .end_expr => {
                const vec = try builder.makeVector(elements.asSlice());
                return ReadResult{ .atom = .{
                    .val = vec,
                    .token = open_token,
                } };
            },
            .end => {
                @branchHint(.cold);
                error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                    .kind = .unexpected_eof,
                    .line = open_token.line,
                    .column = open_token.column,
                    .message = "missing closing parenthesis for vector",
                    .hint = "add ')' to close the vector",
                } });
                return Error.ReadError;
            },
            .dot => |dot_token| {
                @branchHint(.cold);
                error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                    .kind = .unexpected_dot,
                    .line = dot_token.line,
                    .column = dot_token.column,
                    .message = "vectors do not support dot notation",
                    .lexeme = dot_token.lexeme,
                } });
                return Error.ReadError;
            },
        }
    }
}

fn readQuote(self: *Reader, quote_token: Tokenizer.Token, error_details: *ErrorDetails) Error!ReadResult {
    const builder = self.vm.builder();
    const next = try self.readNextImpl(error_details);

    switch (next) {
        .atom => |a| {
            const quote_symbol = try builder.makeStaticSymbol("quote");
            const list_items = [_]Val{ quote_symbol, a.val };
            const quoted = try builder.makeList(&list_items);
            return ReadResult{ .atom = .{
                .val = quoted,
                .token = quote_token,
            } };
        },
        .end => {
            @branchHint(.cold);
            error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                .kind = .unexpected_eof,
                .line = quote_token.line,
                .column = quote_token.column,
                .message = "expected value after quote",
                .hint = "add a value after the quote, e.g., 'x or '(1 2 3)",
            } });
            return Error.ReadError;
        },
        .end_expr => |token| {
            @branchHint(.cold);
            error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                .kind = .unexpected_close_paren,
                .line = token.line,
                .column = token.column,
                .message = "expected value after quote, found closing parenthesis",
                .lexeme = token.lexeme,
            } });
            return Error.ReadError;
        },
        .dot => |token| {
            @branchHint(.cold);
            error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                .kind = .unexpected_dot,
                .line = token.line,
                .column = token.column,
                .message = "expected value after quote, found dot",
                .lexeme = token.lexeme,
            } });
            return Error.ReadError;
        },
    }
}

fn parseSharpsign(self: Reader, token: Tokenizer.Token, error_details: *ErrorDetails) Error!ReadResult {
    const lexeme = token.lexeme;

    // Lexeme must start with '#'
    if (lexeme.len == 0 or lexeme[0] != '#') {
        @branchHint(.cold);
        error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
            .kind = .unexpected_token,
            .line = token.line,
            .column = token.column,
            .message = "invalid sharpsign literal",
            .lexeme = lexeme,
        } });
        return Error.ReadError;
    }

    // Check if it's a character literal (#\...)
    if (lexeme.len >= 2 and lexeme[1] == '\\') {
        return self.parseCharacter(token, error_details);
    }

    // Check if it's a boolean (#t, #f, #true, #false)
    if (std.mem.eql(u8, lexeme, "#t") or std.mem.eql(u8, lexeme, "#true")) {
        return ReadResult{ .atom = .{
            .val = Val.initBool(true),
            .token = token,
        } };
    }
    if (std.mem.eql(u8, lexeme, "#f") or std.mem.eql(u8, lexeme, "#false")) {
        return ReadResult{ .atom = .{
            .val = Val.initBool(false),
            .token = token,
        } };
    }

    // If we get here, it's an invalid sharpsign literal
    error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
        .kind = .unexpected_token,
        .line = token.line,
        .column = token.column,
        .message = "invalid sharpsign literal",
        .lexeme = lexeme,
    } });
    return Error.ReadError;
}

fn parseNumber(self: Reader, token: Tokenizer.Token, error_details: *ErrorDetails) Error!ReadResult {
    const lexeme = token.lexeme;
    if (lexeme.len == 0) {
        @branchHint(.cold);
        error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
            .kind = .invalid_number,
            .line = token.line,
            .column = token.column,
            .message = "empty number literal",
        } });
        return Error.ReadError;
    }

    // Check if this is a float (contains a decimal point) or rational (contains a slash)
    var has_decimal = false;
    var slash_idx: ?usize = null;
    for (lexeme, 0..) |c, i| {
        if (c == '.') {
            has_decimal = true;
            break;
        }
        if (c == '/') {
            slash_idx = i;
            break;
        }
    }

    if (has_decimal) {
        // Parse as float
        const f = std.fmt.parseFloat(f64, lexeme) catch {
            @branchHint(.cold);
            error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                .kind = .invalid_number,
                .line = token.line,
                .column = token.column,
                .message = "invalid floating-point number",
                .lexeme = lexeme,
            } });
            return Error.ReadError;
        };
        const val = Val.initFloat(f);
        return ReadResult{ .atom = .{
            .val = val,
            .token = token,
        } };
    }

    if (slash_idx) |idx| {
        return try self.parseRational(lexeme, idx, token, error_details);
    }

    // Parse as integer
    const n = std.fmt.parseInt(i64, lexeme, 10) catch |err| {
        @branchHint(.cold);
        const message = switch (err) {
            error.Overflow => "integer literal is too large",
            error.InvalidCharacter => "invalid digit in integer literal",
        };
        error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
            .kind = .invalid_number,
            .line = token.line,
            .column = token.column,
            .message = message,
            .lexeme = lexeme,
        } });
        return Error.ReadError;
    };

    const val = Val.initInt(n);
    return ReadResult{ .atom = .{
        .val = val,
        .token = token,
    } };
}

fn parseRational(self: Reader, lexeme: []const u8, slash_idx: usize, token: Tokenizer.Token, error_details: *ErrorDetails) Error!ReadResult {
    const numerator_str = lexeme[0..slash_idx];
    const denominator_str = lexeme[slash_idx + 1 ..];

    if (denominator_str.len == 0) {
        @branchHint(.cold);
        error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
            .kind = .invalid_number,
            .line = token.line,
            .column = token.column,
            .message = "rational number missing denominator",
            .lexeme = lexeme,
        } });
        return Error.ReadError;
    }

    // Parse as i64/u64 to handle large numbers that might reduce to smaller ones
    const numerator_i64 = std.fmt.parseInt(i64, numerator_str, 10) catch |err| {
        @branchHint(.cold);
        const message = switch (err) {
            error.Overflow => "rational numerator is too large",
            error.InvalidCharacter => "invalid digit in rational numerator",
        };
        error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
            .kind = .invalid_number,
            .line = token.line,
            .column = token.column,
            .message = message,
            .lexeme = lexeme,
        } });
        return Error.ReadError;
    };

    // Check for negative denominator before parsing
    if (denominator_str.len > 0 and (denominator_str[0] == '-' or denominator_str[0] == '+')) {
        @branchHint(.cold);
        error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
            .kind = .invalid_number,
            .line = token.line,
            .column = token.column,
            .message = "rational denominator cannot be negative",
            .lexeme = lexeme,
        } });
        return Error.ReadError;
    }

    const denominator_u64 = std.fmt.parseInt(u64, denominator_str, 10) catch |err| {
        @branchHint(.cold);
        const message = switch (err) {
            error.Overflow => "rational denominator is too large",
            error.InvalidCharacter => "invalid digit in rational denominator",
        };
        error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
            .kind = .invalid_number,
            .line = token.line,
            .column = token.column,
            .message = message,
            .lexeme = lexeme,
        } });
        return Error.ReadError;
    };

    // Use fromInt64 which handles reduction and validation
    const rational = Rational.fromInt64(numerator_i64, denominator_u64) catch |err| {
        @branchHint(.cold);
        const message = switch (err) {
            Rational.Error.DenominatorZero => "rational denominator cannot be zero",
            Rational.Error.NumeratorTooLarge => "rational numerator is too large after reduction",
            Rational.Error.DenominatorTooLarge => "rational denominator is too large after reduction",
        };
        error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
            .kind = .invalid_number,
            .line = token.line,
            .column = token.column,
            .message = message,
            .lexeme = lexeme,
        } });
        return Error.ReadError;
    };

    return ReadResult{ .atom = .{
        .val = Val.initRational(rational),
        .token = token,
    } };
}

fn parseSymbol(self: Reader, token: Tokenizer.Token) Error!ReadResult {
    const symbol = try self.vm.builder().makeSymbol(token.lexeme);
    return ReadResult{ .atom = .{
        .val = symbol,
        .token = token,
    } };
}

fn parseString(self: Reader, token: Tokenizer.Token, error_details: *ErrorDetails) Error!ReadResult {
    const lexeme = token.lexeme;
    // Token includes the quotes, so we need to strip them
    if (lexeme.len < 2 or lexeme[0] != '"' or lexeme[lexeme.len - 1] != '"') {
        @branchHint(.cold);
        error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
            .kind = .invalid_string,
            .line = token.line,
            .column = token.column,
            .message = "malformed string literal",
        } });
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
    return ReadResult{ .atom = .{
        .val = string_val,
        .token = token,
    } };
}

fn parseCharacter(self: Reader, token: Tokenizer.Token, error_details: *ErrorDetails) Error!ReadResult {
    const lexeme = token.lexeme;
    // Token format: #\<character>
    // Examples: #\a, #\space, #\newline, #\x3BB
    if (lexeme.len < 3 or lexeme[0] != '#' or lexeme[1] != '\\') {
        @branchHint(.cold);
        error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
            .kind = .invalid_character,
            .line = token.line,
            .column = token.column,
            .message = "malformed character literal",
        } });
        return Error.ReadError;
    }

    const char_part = lexeme[2..];

    // Handle single char
    if (char_part.len == 1)
        return ReadResult{ .atom = .{
            .val = Val.initChar(char_part[0]),
            .token = token,
        } };

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
        return ReadResult{ .atom = .{
            .val = Val.initChar(value),
            .token = token,
        } };

    // Handle hex
    if (char_part.len > 1 and char_part[0] == 'x') {
        // Hex escape: #\x3BB
        const hex_str = char_part[1..];
        if (hex_str.len == 0) {
            @branchHint(.cold);
            error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                .kind = .invalid_character,
                .line = token.line,
                .column = token.column,
                .message = "hex character escape requires at least one digit",
            } });
            return Error.ReadError;
        }
        const int_ch = std.fmt.parseInt(u21, hex_str, 16) catch {
            @branchHint(.cold);
            error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
                .kind = .invalid_character,
                .line = token.line,
                .column = token.column,
                .message = "invalid hex character escape",
            } });
            return Error.ReadError;
        };
        return ReadResult{ .atom = .{
            .val = Val.initChar(int_ch),
            .token = token,
        } };
    }

    // Unknown named character or invalid format
    error_details.addDiagnostic(self.vm.allocator(), .{ .reader = .{
        .kind = .invalid_character,
        .line = token.line,
        .column = token.column,
        .message = "unknown named character",
        .lexeme = lexeme,
    } });
    return Error.ReadError;
}

fn expectReadNext(self: *Reader, expect: ?[]const u8, vm: *const Vm) !void {
    var error_details = ErrorDetails{};
    defer error_details.deinit(testing.allocator);
    errdefer std.debug.print("Error details:\n{f}\n", .{error_details.pretty(vm, .nocolor)});

    const next = try self.readNext(&error_details);
    if (expect) |expected_str| {
        const val = next orelse {
            std.debug.print("Expected value '{s}' but got end of input\n", .{expected_str});
            return error.TestUnexpectedResult;
        };
        return testing.expectFmt(expected_str, "{f}", .{vm.pretty(val, .{})});
    } else if (next) |val| {
        std.debug.print("Expected end of input but got: {f}\n", .{vm.pretty(val, .{})});
        return error.TestUnexpectedResult;
    }
}

fn expectReadError(
    self: *Reader,
    expected_error: anyerror,
    expected_kind: ErrorDetails.ReadErrorInfo.Kind,
) !void {
    var error_details = ErrorDetails{};
    defer error_details.deinit(testing.allocator);
    errdefer std.debug.print("Error details:\n{f}\n", .{error_details.pretty(self.vm, .nocolor)});

    const actual_error = self.readNext(&error_details);
    try testing.expectError(expected_error, actual_error);
    try testing.expectEqual(expected_kind, error_details.diagnostics.items[0].reader.kind);
}

fn expectReadOneError(
    vm: *Vm,
    source: []const u8,
    expected_error: anyerror,
    expected_kind: ErrorDetails.ReadErrorInfo.Kind,
) !void {
    var error_details = ErrorDetails{};
    defer error_details.deinit(testing.allocator);
    errdefer std.debug.print("Error details:\n{f}\n", .{error_details.pretty(vm, .nocolor)});

    const actual_error = Reader.readOne(vm, source, &error_details);
    try testing.expectError(expected_error, actual_error);
    try testing.expectEqual(expected_kind, error_details.diagnostics.items[0].reader.kind);
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
    var error_details = ErrorDetails{};
    defer error_details.deinit(testing.allocator);
    const val1 = (try reader.readNext(&error_details)).?;
    const val2 = (try reader.readNext(&error_details)).?;
    const val3 = (try reader.readNext(&error_details)).?;
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

test "read special floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "+inf.0 -inf.0 +nan.0");
    var error_details = ErrorDetails{};
    defer error_details.deinit(testing.allocator);

    // Read +inf.0
    const pos_inf = (try reader.readNext(&error_details)).?;
    try testing.expect(pos_inf.asFloat() != null);
    try testing.expect(std.math.isPositiveInf(pos_inf.asFloat().?));

    // Read -inf.0
    const neg_inf = (try reader.readNext(&error_details)).?;
    try testing.expect(neg_inf.asFloat() != null);
    try testing.expect(std.math.isNegativeInf(neg_inf.asFloat().?));

    // Read +nan.0
    const nan_val = (try reader.readNext(&error_details)).?;
    try testing.expect(nan_val.asFloat() != null);
    try testing.expect(std.math.isNan(nan_val.asFloat().?));

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

test "read rational numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "1/2 3/4 -1/2 +5/3");
    try reader.expectReadNext("1/2", &vm);
    try reader.expectReadNext("3/4", &vm);
    try reader.expectReadNext("-1/2", &vm);
    try reader.expectReadNext("5/3", &vm);
    try reader.expectReadNext(null, &vm);
}

test "read rational numbers with automatic reduction" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "10/4 6/9 100/25");
    try reader.expectReadNext("5/2", &vm);
    try reader.expectReadNext("2/3", &vm);
    try reader.expectReadNext("4", &vm); // 100/25 reduces to 4/1, which becomes integer 4
    try reader.expectReadNext(null, &vm);
}

test "read rational numbers that reduce to integers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "25/1 50/10 -12/4");
    try reader.expectReadNext("25", &vm);
    try reader.expectReadNext("5", &vm);
    try reader.expectReadNext("-3", &vm);
    try reader.expectReadNext(null, &vm);
}

test "read large rational numbers that reduce to small ones" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test large numbers that reduce to small values
    // 6000000000/3000000000 = 2/1 = 2
    // 9000000000/3000000000 = 3/1 = 3
    // 10000000000/4000000000 = 5/2
    var reader = Reader.init(&vm, "6000000000/3000000000 9000000000/3000000000 10000000000/4000000000");
    try reader.expectReadNext("2", &vm);
    try reader.expectReadNext("3", &vm);
    try reader.expectReadNext("5/2", &vm);
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

    var reader = Reader.init(&vm, "(1 2 3");
    try reader.expectReadError(Error.ReadError, .unexpected_eof);
}

test "error unexpected close paren" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "1)");
    var error_details = ErrorDetails{};
    defer error_details.deinit(testing.allocator);
    _ = try reader.readNext(&error_details);
    try reader.expectReadError(Error.ReadError, .unexpected_close_paren);
}

test "error empty dot list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "(. 1)");
    try reader.expectReadError(Error.ReadError, .empty_dot_list);
}

test "error unexpected dot" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, ".");
    try reader.expectReadError(Error.ReadError, .unexpected_dot);
}

test "error readOne no values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectReadOneError(&vm, "", ReadOneError.NoValue, .no_values);
}

test "error readOne multiple values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectReadOneError(&vm, "1 2", ReadOneError.TooManyValues, .multiple_values);
}

test "error invalid bytevector" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "#u8(256)");
    try reader.expectReadError(Error.ReadError, .invalid_bytevector);
}

test "error invalid sharpsign literal" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var error_details = ErrorDetails{};
    defer error_details.deinit(testing.allocator);
    errdefer std.debug.print("Error details:\n{f}\n", .{error_details.pretty(&vm, .nocolor)});

    var reader = Reader.init(&vm, "#invalid");
    const result = reader.readNext(&error_details);
    try testing.expectError(Error.ReadError, result);
    try testing.expectEqual(ErrorDetails.ReadErrorInfo.Kind.unexpected_token, error_details.diagnostics.items[0].reader.kind);
    try testing.expectEqualStrings("invalid sharpsign literal", error_details.diagnostics.items[0].reader.message);
}

test "error bare sharpsign" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var error_details = ErrorDetails{};
    defer error_details.deinit(testing.allocator);
    errdefer std.debug.print("Error details:\n{f}\n", .{error_details.pretty(&vm, .nocolor)});

    var reader = Reader.init(&vm, "#");
    const result = reader.readNext(&error_details);
    try testing.expectError(Error.ReadError, result);
    try testing.expectEqual(ErrorDetails.ReadErrorInfo.Kind.unexpected_token, error_details.diagnostics.items[0].reader.kind);
    try testing.expectEqualStrings("invalid sharpsign literal", error_details.diagnostics.items[0].reader.message);
    try testing.expect(error_details.diagnostics.items[0].reader.lexeme != null);
    try testing.expectEqualStrings("#", error_details.diagnostics.items[0].reader.lexeme.?);
}

test "error invalid rational numerator" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    // Test invalid character in numerator
    {
        var error_details = ErrorDetails{};
        defer error_details.deinit(testing.allocator);
        errdefer std.debug.print("Error details:\n{f}\n", .{error_details.pretty(&vm, .nocolor)});

        var reader = Reader.init(&vm, "12abc/5");
        const result = reader.readNext(&error_details);
        try testing.expectError(Error.ReadError, result);
        try testing.expectEqual(ErrorDetails.ReadErrorInfo.Kind.invalid_number, error_details.diagnostics.items[0].reader.kind);
        try testing.expectEqualStrings("invalid digit in rational numerator", error_details.diagnostics.items[0].reader.message);
    }

    // Test overflow in numerator (during initial parse, before reduction can help)
    {
        var error_details = ErrorDetails{};
        defer error_details.deinit(testing.allocator);
        errdefer std.debug.print("Error details:\n{f}\n", .{error_details.pretty(&vm, .nocolor)});

        var reader = Reader.init(&vm, "99999999999999999999/1");
        const result = reader.readNext(&error_details);
        try testing.expectError(Error.ReadError, result);
        try testing.expectEqual(ErrorDetails.ReadErrorInfo.Kind.invalid_number, error_details.diagnostics.items[0].reader.kind);
        try testing.expectEqualStrings("rational numerator is too large", error_details.diagnostics.items[0].reader.message);
    }
}

test "error invalid rational denominator" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test invalid character in denominator
    {
        var error_details = ErrorDetails{};
        defer error_details.deinit(testing.allocator);
        errdefer std.debug.print("Error details:\n{f}\n", .{error_details.pretty(&vm, .nocolor)});

        var reader = Reader.init(&vm, "5/12xyz");
        const result = reader.readNext(&error_details);
        try testing.expectError(Error.ReadError, result);
        try testing.expectEqual(ErrorDetails.ReadErrorInfo.Kind.invalid_number, error_details.diagnostics.items[0].reader.kind);
        try testing.expectEqualStrings("invalid digit in rational denominator", error_details.diagnostics.items[0].reader.message);
    }

    // Test overflow in denominator (during initial parse, before reduction can help)
    {
        var error_details = ErrorDetails{};
        defer error_details.deinit(testing.allocator);
        errdefer std.debug.print("Error details:\n{f}\n", .{error_details.pretty(&vm, .nocolor)});

        var reader = Reader.init(&vm, "1/99999999999999999999");
        const result = reader.readNext(&error_details);
        try testing.expectError(Error.ReadError, result);
        try testing.expectEqual(ErrorDetails.ReadErrorInfo.Kind.invalid_number, error_details.diagnostics.items[0].reader.kind);
        try testing.expectEqualStrings("rational denominator is too large", error_details.diagnostics.items[0].reader.message);
    }
}

test "error zero denominator" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var error_details = ErrorDetails{};
    defer error_details.deinit(testing.allocator);
    errdefer std.debug.print("Error details:\n{f}\n", .{error_details.pretty(&vm, .nocolor)});

    var reader = Reader.init(&vm, "1/0");
    const result = reader.readNext(&error_details);
    try testing.expectError(Error.ReadError, result);
    try testing.expectEqual(ErrorDetails.ReadErrorInfo.Kind.invalid_number, error_details.diagnostics.items[0].reader.kind);
    try testing.expectEqualStrings("rational denominator cannot be zero", error_details.diagnostics.items[0].reader.message);
}

test "read negative denominator rational gives error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Negative denominators are not valid syntax
    var error_details = ErrorDetails{};
    defer error_details.deinit(testing.allocator);
    errdefer std.debug.print("Error details:\n{f}\n", .{error_details.pretty(&vm, .nocolor)});

    var reader = Reader.init(&vm, "1/-2");
    const result = reader.readNext(&error_details);
    try testing.expectError(Error.ReadError, result);
}

test "lexeme information in diagnostics" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test unexpected dot
    {
        var error_details = ErrorDetails{};
        defer error_details.deinit(testing.allocator);
        errdefer std.debug.print("Error details:\n{f}\n", .{error_details.pretty(&vm, .nocolor)});

        var reader = Reader.init(&vm, ".");
        _ = reader.readNext(&error_details) catch {};
        try testing.expectEqual(ErrorDetails.ReadErrorInfo.Kind.unexpected_dot, error_details.diagnostics.items[0].reader.kind);
        try testing.expect(error_details.diagnostics.items[0].reader.lexeme != null);
        try testing.expectEqualStrings(".", error_details.diagnostics.items[0].reader.lexeme.?);
    }

    // Test unexpected close paren
    {
        var error_details = ErrorDetails{};
        defer error_details.deinit(testing.allocator);
        errdefer std.debug.print("Error details:\n{f}\n", .{error_details.pretty(&vm, .nocolor)});

        var reader = Reader.init(&vm, "1 )");
        _ = try reader.readNext(&error_details);
        _ = reader.readNext(&error_details) catch {};
        try testing.expectEqual(ErrorDetails.ReadErrorInfo.Kind.unexpected_close_paren, error_details.diagnostics.items[0].reader.kind);
        try testing.expect(error_details.diagnostics.items[0].reader.lexeme != null);
        try testing.expectEqualStrings(")", error_details.diagnostics.items[0].reader.lexeme.?);
    }

    // Test invalid bytevector element
    {
        var error_details = ErrorDetails{};
        defer error_details.deinit(testing.allocator);
        errdefer std.debug.print("Error details:\n{f}\n", .{error_details.pretty(&vm, .nocolor)});

        var reader = Reader.init(&vm, "#u8(256)");
        _ = reader.readNext(&error_details) catch {};
        try testing.expectEqual(ErrorDetails.ReadErrorInfo.Kind.invalid_bytevector, error_details.diagnostics.items[0].reader.kind);
        try testing.expect(error_details.diagnostics.items[0].reader.lexeme != null);
        try testing.expectEqualStrings("256", error_details.diagnostics.items[0].reader.lexeme.?);
    }
}
