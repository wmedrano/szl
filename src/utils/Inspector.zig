const std = @import("std");

const Handle = @import("../types/object_pool.zig").Handle;
const Module = @import("../types/Module.zig");
const Pair = @import("../types/Pair.zig");
const Proc = @import("../types/Proc.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

const Inspector = @This();

vm: *Vm,

pub fn init(vm: *Vm) Inspector {
    return Inspector{ .vm = vm };
}

pub inline fn asInt(_: Inspector, val: Val) error{WrongType}!i64 {
    return switch (val.data) {
        .int => |n| n,
        else => error.WrongType,
    };
}

pub inline fn asModule(_: Inspector, val: Val) Vm.Error!*Module {
    return switch (val.data) {
        .module => |env| env,
        else => Vm.Error.WrongType,
    };
}

pub inline fn asPair(self: Inspector, val: Val) Vm.Error!*Pair {
    return switch (val.data) {
        .pair => |h| self.vm.objects.pairs.get(h) orelse return Vm.Error.UndefinedBehavior,
        else => Vm.Error.WrongType,
    };
}

pub inline fn asSymbol(_: Inspector, val: Val) Vm.Error!Symbol {
    return switch (val.data) {
        .symbol => |sym| sym,
        else => Vm.Error.WrongType,
    };
}

const AsListError = error{
    OutOfMemory,
    UndefinedBehavior,
    WrongType,
};

pub fn asList(self: Inspector, allocator: std.mem.Allocator, val: Val) AsListError![]Val {
    var items = std.ArrayList(Val){};
    errdefer items.deinit(allocator);

    var current = val;
    while (true) {
        switch (current.data) {
            .empty_list => break,
            .pair => |h| {
                const pair = self.vm.objects.pairs.get(h) orelse
                    return Vm.Error.UndefinedBehavior;
                try items.append(allocator, pair.car);
                current = pair.cdr;
            },
            else => return Vm.Error.WrongType,
        }
    }

    return try items.toOwnedSlice(allocator);
}

pub inline fn handleToProc(self: Inspector, h: Handle(Proc)) Vm.Error!*Proc {
    return self.vm.objects.procs.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn handleToModule(self: Inspector, h: Handle(Module)) Vm.Error!*Module {
    return self.vm.objects.modules.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub fn findModule(self: Inspector, path: []const Symbol.Interned) ?Handle(Module) {
    var moduleIter = self.vm.objects.modules.iterator();
    while (moduleIter.next()) |module| {
        if (std.mem.eql(Symbol.Interned, module.value.namespace, path))
            return module.handle;
    }
    return null;
}
