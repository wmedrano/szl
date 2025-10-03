const std = @import("std");

const Cons = @import("Cons.zig");
const Module = @import("Module.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const Inspector = @This();

vm: *Vm,

pub fn init(vm: *Vm) Inspector {
    return Inspector{ .vm = vm };
}

pub inline fn asInt(_: Inspector, val: Val) Vm.Error!i64 {
    return switch (val.data) {
        .int => |n| n,
        else => Vm.Error.WrongType,
    };
}

pub inline fn asModule(_: Inspector, val: Val) Vm.Error!*Module {
    return switch (val.data) {
        .module => |env| env,
        else => Vm.Error.WrongType,
    };
}

pub inline fn asPair(_: Inspector, val: Val) Vm.Error!*Cons {
    return switch (val.data) {
        .pair => |cons| cons,
        else => Vm.Error.WrongType,
    };
}

pub inline fn asSymbol(_: Inspector, val: Val) Vm.Error!Symbol {
    return switch (val.data) {
        .symbol => |sym| sym,
        else => Vm.Error.WrongType,
    };
}

pub fn asList(_: Inspector, allocator: std.mem.Allocator, val: Val) Vm.Error![]Val {
    var items = std.ArrayList(Val){};
    errdefer items.deinit(allocator);

    var current = val;
    while (true) {
        switch (current.data) {
            .empty_list => break,
            .pair => |cons| {
                try items.append(allocator, cons.car);
                current = cons.cdr;
            },
            else => return Vm.Error.WrongType,
        }
    }

    return try items.toOwnedSlice(allocator);
}
