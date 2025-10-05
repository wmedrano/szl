const std = @import("std");
const testing = std.testing;

const Handle = @import("object_pool.zig").Handle;
const Module = @import("Module.zig");
const Pair = @import("Pair.zig");
const PrettyPrinter = @import("PrettyPrinter.zig");
const Proc = @import("Proc.zig");
const Symbol = @import("Symbol.zig");
const Vm = @import("Vm.zig");

const Val = @This();

data: Data,

pub const Data = union(enum) {
    empty_list,
    int: i64,
    pair: Handle(Pair),
    symbol: Symbol.Interned,
    module: Handle(Module),
    proc: Handle(Proc),
    proc_builtin: Proc.Builtin,
};

pub fn initEmptyList() Val {
    return Val{ .data = .{ .empty_list = {} } };
}

pub fn initInt(x: i64) Val {
    return Val{ .data = .{ .int = x } };
}

pub fn initSymbol(sym: Symbol.Interned) Val {
    return Val{ .data = .{ .symbol = sym } };
}

pub fn initBuiltinProc(proc: Proc.Builtin) Val {
    return Val{ .data = .{ .proc_builtin = proc } };
}

pub fn isNull(self: Val) bool {
    return self.data == .empty_list;
}

pub fn isProc(self: Val) bool {
    return switch (self.data) {
        .proc, .proc_builtin => true,
        else => false,
    };
}

pub fn asSymbol(self: Val) ?Symbol.Interned {
    switch (self.data) {
        .symbol => |s| return s,
        else => return null,
    }
}

pub fn pretty(self: Val, vm: *const Vm) PrettyPrinter {
    return PrettyPrinter{ .vm = vm, .val = self };
}

pub fn eq(self: Val, other: Val) bool {
    return std.meta.eql(self, other);
}

test "Val is small" {
    try testing.expectEqual(16, @sizeOf(Val));
}
