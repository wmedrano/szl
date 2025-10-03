const std = @import("std");
const testing = std.testing;

const Cons = @import("Cons.zig");
const Vm = @import("Vm.zig");

const Val = @This();

data: Data,

pub const Data = union(enum) {
    empty_list,
    int: i64,
    pair: *Cons,
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
    }
}

test "format empty list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    try testing.expectFmt("()", "{f}", .{b.makeEmptyList()});
}

test "format int" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    try testing.expectFmt("42", "{f}", .{b.makeInt(42)});
}

test "format proper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const items = [_]Val{ b.makeInt(1), b.makeInt(2), b.makeInt(3) };
    try testing.expectFmt("(1 2 3)", "{f}", .{try b.makeList(&items)});
}

test "format improper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const items = [_]Val{ b.makeInt(1), b.makeInt(2), b.makeInt(3) };
    try testing.expectFmt("(1 2 . 3)", "{f}", .{try b.makeImproperList(&items)});
}

test "format nested list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const inner_items = [_]Val{ b.makeInt(2), b.makeInt(3) };
    const inner_list = try b.makeList(&inner_items);
    const outer_items = [_]Val{ b.makeInt(1), inner_list };
    try testing.expectFmt("(1 (2 3))", "{f}", .{try b.makeList(&outer_items)});
}
