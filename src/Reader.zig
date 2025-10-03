const std = @import("std");
const testing = std.testing;

const Tokenizer = @import("Tokenizer.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

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
                            try builder.makeImproperList(elements.items)
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
        .symbol => return Vm.Error.NotImplemented,
        .boolean => return Vm.Error.NotImplemented,
        .quote => return Vm.Error.NotImplemented,
        .quasiquote => return Vm.Error.NotImplemented,
        .unquote => return Vm.Error.NotImplemented,
        .unquote_splicing => return Vm.Error.NotImplemented,
    }
}

fn parseNumber(self: Reader, token: []const u8) Vm.Error!ReadResult {
    var n: i64 = 0;
    for (token) |digit| {
        if (digit < '0' or digit > '9') return Vm.Error.ReadError;
        n *= 10;
        n += digit - '0';
    }
    const val = self.vm.builder().makeInt(n);
    return ReadResult{ .atom = val };
}

test "read int" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "1 009");
    try testing.expectFmt("1", "{f}", .{(try reader.readNext()).?});
    try testing.expectFmt("9", "{f}", .{(try reader.readNext()).?});
    try testing.expectEqual(null, try reader.readNext());
}

test "read list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = Reader.init(&vm, "() (1 2 3)");
    try testing.expectFmt("()", "{f}", .{(try reader.readNext()).?});
    try testing.expectFmt("(1 2 3)", "{f}", .{(try reader.readNext()).?});
    try testing.expectEqual(null, try reader.readNext());
}

test "read pair" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var reader = Reader.init(&vm, "(1 . 2) (1 2 . 3)");

    try testing.expectFmt("(1 . 2)", "{f}", .{(try reader.readNext()).?});
    try testing.expectFmt("(1 2 . 3)", "{f}", .{(try reader.readNext()).?});
    try testing.expectEqual(null, try reader.readNext());
}
