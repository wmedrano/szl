const std = @import("std");
const testing = std.testing;

const Cons = @import("Cons.zig");
const Handle = @import("object_pool.zig").Handle;
const Module = @import("Module.zig");
const PrettyPrinter = @import("PrettyPrinter.zig");
const Symbol = @import("Symbol.zig");
const Vm = @import("Vm.zig");

const Val = @This();

data: Data,

pub const Data = union(enum) {
    empty_list,
    module: Handle(Module),
    int: i64,
    pair: Handle(Cons),
    symbol: Symbol,
};

pub fn isEmptyList(_: Val) bool {}

pub fn pretty(self: Val, vm: *const Vm) PrettyPrinter {
    return PrettyPrinter{ .vm = vm, .val = self };
}

pub fn eq(self: Val, other: Val) bool {
    return std.meta.eql(self, other);
}
