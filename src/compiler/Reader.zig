const std = @import("std");
const testing = std.testing;

const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");
const Tokenizer = @import("Tokenizer.zig");

const Reader = @This();

tokenizer: Tokenizer,
vm: *Vm,

pub fn init(vm: *Vm, source: []const u8) Reader {
    return Reader{
        .vm = vm,
        .tokenizer = Tokenizer.init(source),
    };
}

pub fn readNext(self: *Reader) Vm.Error!?Val {
    const result = try self.readNextImpl();
    switch (result) {
        .atom => |v| return v,
        .end_expr => return Vm.Error.ReadError,
        .dot => return Vm.Error.ReadError,
        .end => return null,
    }
}

pub const ReadOneError = Vm.Error || error{
    NoValue,
    TooManyValues,
};

pub fn readOne(vm: *Vm, source: []const u8) ReadOneError!Val {
    var reader = Reader.init(vm, source);
    const first = try reader.readNext() orelse return ReadOneError.NoValue;
    if (try reader.readNext()) |_| {
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

pub fn readNextImpl(self: *Reader) Vm.Error!ReadResult {
    const next_token = self.tokenizer.nextToken() orelse
        return ReadResult{ .end = {} };
    const builder = self.vm.builder();
    switch (next_token.type) {
        .left_paren => {
            var elements = std.ArrayList(Val){};
            defer elements.deinit(self.vm.allocator());
            var dot_idx: ?usize = null;
            while (true) {
                switch (try self.readNextImpl()) {
                    .atom => |v| try elements.append(self.vm.allocator(), v),
                    .end_expr => {
                        const res = if (dot_idx == elements.items.len)
                            try builder.makePairs(elements.items)
                        else
                            try builder.makeList(elements.items);
                        return ReadResult{ .atom = res };
                    },
                    .end => return Vm.Error.ReadError,
                    .dot => {
                        if (elements.items.len == 0) return Vm.Error.ReadError;
                        dot_idx = elements.items.len + 1;
                    },
                }
            }
        },
        .right_paren => return ReadResult{ .end_expr = {} },
        .dot => return ReadResult{ .dot = {} },
        .number => return try self.parseNumber(next_token.lexeme),
        .string => return Vm.Error.NotImplemented,
        .symbol => return try self.parseSymbol(next_token.lexeme),
        .boolean => return try self.parseBoolean(next_token.lexeme),
        .quote => {
            const next = try self.readNextImpl();
            switch (next) {
                .atom => |v| {
                    const quote_symbol = try builder.makeStaticSymbol("quote");
                    const list_items = [_]Val{ quote_symbol, v };
                    const quoted = try builder.makeList(&list_items);
                    return ReadResult{ .atom = quoted };
                },
                .end => return Vm.Error.ReadError,
                .end_expr => return Vm.Error.ReadError,
                .dot => return Vm.Error.ReadError,
            }
        },
        .quasiquote => return Vm.Error.NotImplemented,
        .unquote => return Vm.Error.NotImplemented,
        .unquote_splicing => return Vm.Error.NotImplemented,
    }
}

fn parseNumber(_: Reader, token: []const u8) Vm.Error!ReadResult {
    if (token.len == 0) return Vm.Error.ReadError;

    var is_negative = false;
    var start_idx: usize = 0;

    // Handle leading sign
    if (token[0] == '-') {
        is_negative = true;
        start_idx = 1;
    } else if (token[0] == '+') {
        start_idx = 1;
    }

    if (start_idx >= token.len) return Vm.Error.ReadError;

    var n: i64 = 0;
    for (token[start_idx..]) |digit| {
        if (digit < '0' or digit > '9') return Vm.Error.ReadError;
        n *= 10;
        n += digit - '0';
    }

    if (is_negative) n = -n;

    const val = Val.initInt(n);
    return ReadResult{ .atom = val };
}

fn parseSymbol(self: Reader, token: []const u8) Vm.Error!ReadResult {
    const symbol = try self.vm.builder().makeStaticSymbol(token);
    return ReadResult{ .atom = symbol };
}

fn parseBoolean(_: Reader, token: []const u8) Vm.Error!ReadResult {
    const value = if (std.mem.eql(u8, token, "#t") or std.mem.eql(u8, token, "#true"))
        true
    else if (std.mem.eql(u8, token, "#f") or std.mem.eql(u8, token, "#false"))
        false
    else
        return Vm.Error.ReadError;
    const val = Val.initBool(value);
    return ReadResult{ .atom = val };
}

fn expectReadNext(self: *Reader, expect: ?[]const u8, vm: *const Vm) !void {
    const end_of_read = "end_of_read";
    const expect_normalized = expect orelse end_of_read;
    if (try self.readNext()) |next| {
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

test "read list with symbols" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "(define x 42)");
    try reader.expectReadNext("(define x 42)", &vm);
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
