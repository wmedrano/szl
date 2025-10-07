const std = @import("std");
const testing = std.testing;

const PrettyPrinter = @import("../utils/PrettyPrinter.zig");
const Vm = @import("../Vm.zig");
const Handle = @import("object_pool.zig").Handle;
const Module = @import("Module.zig");
const Pair = @import("Pair.zig");
const Proc = @import("Proc.zig");
const Symbol = @import("Symbol.zig");
const Vector = @import("Vector.zig");

const Val = @This();

data: Data,

pub const Closure = struct {
    proc: Handle(Proc),
    captures: Handle(Vector),
};

pub const Data = union(enum) {
    empty_list,
    boolean: bool,
    int: i64,
    pair: Handle(Pair),
    symbol: Symbol.Interned,
    module: Handle(Module),
    proc: Handle(Proc),
    closure: Closure,
    proc_builtin: Proc.Builtin,
    vector: Handle(Vector),
};

pub fn initEmptyList() Val {
    return Val{ .data = .{ .empty_list = {} } };
}

pub fn initInt(x: i64) Val {
    return Val{ .data = .{ .int = x } };
}

pub fn initBool(x: bool) Val {
    return Val{ .data = .{ .boolean = x } };
}

pub fn initSymbol(sym: Symbol.Interned) Val {
    return Val{ .data = .{ .symbol = sym } };
}

pub fn initProc(proc: Handle(Proc)) Val {
    return Val{ .data = .{ .proc = proc } };
}

pub fn initClosure(proc: Handle(Proc), captures: Handle(Vector)) Val {
    return Val{
        .data = .{
            .closure = .{
                .proc = proc,
                .captures = captures,
            },
        },
    };
}

pub fn initBuiltinProc(proc: Proc.Builtin) Val {
    return Val{ .data = .{ .proc_builtin = proc } };
}

pub fn isNull(self: Val) bool {
    return self.data == .empty_list;
}

pub fn isTruthy(self: Val) bool {
    switch (self.data) {
        .boolean => |x| return x,
        else => return true,
    }
}

pub fn isProc(self: Val) bool {
    return switch (self.data) {
        .proc, .closure, .proc_builtin => true,
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
