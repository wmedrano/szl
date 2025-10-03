const std = @import("std");
const testing = std.testing;

const Cons = @import("Cons.zig");
const Symbol = @import("Symbol.zig");
const Vm = @import("Vm.zig");

const Val = @This();

data: Data,

pub const Data = union(enum) {
    empty_list,
    int: i64,
    pair: *Cons,
    symbol: Symbol,
};

pub fn isEmptyList(_: Val) bool {}

pub fn format(self: Val, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    switch (self.data) {
        .empty_list => try writer.writeAll("()"),
        .int => |n| try writer.print("{}", .{n}),
        .pair => |pair| {
            try writer.writeAll("(");
            try pair.car.format(writer);
            var current = pair.cdr;
            while (true) {
                switch (current.data) {
                    .empty_list => break,
                    .pair => |next_pair| {
                        try writer.writeAll(" ");
                        try next_pair.car.format(writer);
                        current = next_pair.cdr;
                    },
                    else => {
                        try writer.writeAll(" . ");
                        try current.format(writer);
                        break;
                    },
                }
            }
            try writer.writeAll(")");
        },
        .symbol => |s| try writer.writeAll(s.string),
    }
}

pub fn eq(self: Val, other: Val) bool {
    return std.meta.eql(self, other);
}

test "format empty list is empty parens" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    try testing.expectFmt("()", "{f}", .{b.makeEmptyList()});
}

test "format int produces int" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    try testing.expectFmt("42", "{f}", .{b.makeInt(42)});
}

test "format proper list produces parens surrounded list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const items = [_]Val{ b.makeInt(1), b.makeInt(2), b.makeInt(3) };
    try testing.expectFmt("(1 2 3)", "{f}", .{try b.makeList(&items)});
}

test "format improper list places dot before last element" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const items = [_]Val{ b.makeInt(1), b.makeInt(2), b.makeInt(3) };
    try testing.expectFmt("(1 2 . 3)", "{f}", .{try b.makeImproperList(&items)});
}

test "format nested list formats the nested list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const inner_items = [_]Val{ b.makeInt(2), b.makeInt(3) };
    const inner_list = try b.makeList(&inner_items);
    const outer_items = [_]Val{ b.makeInt(1), inner_list };
    try testing.expectFmt("(1 (2 3))", "{f}", .{try b.makeList(&outer_items)});
}

test "format symbol produces symbol string" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    try testing.expectFmt("foo", "{f}", .{try b.makeSymbol("foo")});
}

test "symbol with same string is eq" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const sym1 = try b.makeSymbol("foo");
    const sym2 = try b.makeSymbol("foo");

    try testing.expect(sym1.eq(sym2));
}

test "symbol with different identifiers are different" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const sym1 = try b.makeSymbol("foo");
    const sym2 = try b.makeSymbol("foobar");

    try testing.expect(!sym1.eq(sym2));
}
