const std = @import("std");
const testing = std.testing;

const Tokenizer = @import("Tokenizer.zig");
const Vm = @import("Vm.zig");
const Val = Vm.Val;

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
        .end_expr => |exprs| {
            var owned = exprs;
            owned.deinit(self.vm.allocator());
            return Vm.Error.ReadError;
        },
        .end => return null,
    }
}

const ReadResult = union(enum) {
    atom: Val,
    end_expr: std.ArrayList(Val),
    end,
};

pub fn readNextImpl(self: *Reader) Vm.Error!ReadResult {
    const next_token = self.tokenizer.nextToken() orelse
        return ReadResult{ .end = {} };
    switch (next_token.type) {
        .left_paren => {
            var elements = std.ArrayList(Val){};
            errdefer elements.deinit(self.vm.allocator());
            while (true) {
                switch (try self.readNextImpl()) {
                    .atom => |v| try elements.append(self.vm.allocator(), v),
                    .end_expr => {
                        const list = try buildList(elements.items);
                        elements.deinit(self.vm.allocator());
                        return ReadResult{ .atom = list };
                    },
                    .end => {
                        elements.deinit(self.vm.allocator());
                        return Vm.Error.ReadError;
                    },
                }
            }
        },
        .right_paren => return ReadResult{ .end_expr = std.ArrayList(Val){} },
        .number => return try parseNumber(next_token.lexeme),
        .string => return Vm.Error.NotImplemented,
        .symbol => return Vm.Error.NotImplemented,
        .boolean => return Vm.Error.NotImplemented,
        .quote => return Vm.Error.NotImplemented,
        .quasiquote => return Vm.Error.NotImplemented,
        .unquote => return Vm.Error.NotImplemented,
        .unquote_splicing => return Vm.Error.NotImplemented,
    }
}

fn buildList(elements: []const Val) Vm.Error!Val {
    if (elements.len > 0) return Vm.Error.NotImplemented;
    return Val{ .empty_list = {} };
}

fn parseNumber(token: []const u8) Vm.Error!ReadResult {
    var n: i64 = 0;
    for (token) |digit| {
        if (digit < '0' or digit > '9') return Vm.Error.ReadError;
        n *= 10;
        n += digit - '0';
    }
    const val = Val{ .int = n };
    return ReadResult{ .atom = val };
}

test "read int" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "1 009");
    try testing.expectEqualDeep(Val{ .int = 1 }, reader.readNext());
    try testing.expectEqualDeep(Val{ .int = 9 }, reader.readNext());
    try testing.expectEqualDeep(null, reader.readNext());
}

// TODO: Add support for lists with elements.
test "read list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "()");
    try testing.expectEqualDeep(Val{ .empty_list = {} }, reader.readNext());
    try testing.expectEqualDeep(null, reader.readNext());
}
