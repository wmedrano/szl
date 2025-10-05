const std = @import("std");
const testing = std.testing;

const Handle = @import("object_pool.zig").Handle;
const Module = @import("Module.zig");
const Pair = @import("Pair.zig");
const PrettyPrinter = @import("PrettyPrinter.zig");
const Symbol = @import("Symbol.zig");
const Vm = @import("Vm.zig");

const Val = @This();

data: Data,

pub const Data = union(enum) {
    empty_list,
    module: Handle(Module),
    int: i64,
    pair: Handle(Pair),
    symbol: Symbol.Interned,
};

pub fn initSymbol(sym: Symbol.Interned) Val {
    return Val{ .data = .{ .symbol = sym } };
}

pub fn isEmptyList(_: Val) bool {}

pub fn pretty(self: Val, vm: *const Vm) PrettyPrinter {
    return PrettyPrinter{ .vm = vm, .val = self };
}

pub fn eq(self: Val, other: Val) bool {
    return std.meta.eql(self, other);
}

test "Val is small" {
    try testing.expectEqual(16, @sizeOf(Val));
}
