const std = @import("std");

const Handle = @import("object_pool.zig").Handle;
const Module = @import("Module.zig");
const Proc = @import("Proc.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

pub const Instruction = union(enum) {
    push_const: Val,
    module_get: struct { module: Handle(Module), symbol: Symbol.Interned },
    squash: u32,
    eval: u32,
    ret,

    pub fn execute(self: Instruction, vm: *Vm) Vm.Error!void {
        switch (self) {
            .push_const => |val| try vm.context.push(vm.allocator(), val),
            .module_get => |g| try moduleGet(vm, g.module, g.symbol),
            .squash => |n| try vm.context.stackSquash(n),
            .eval => |n| try eval(vm, n),
            .ret => try vm.context.popStackFrame(.place_on_top),
        }
    }
};

fn moduleGet(vm: *Vm, module: Handle(Module), symbol: Symbol.Interned) !void {
    const m = vm.inspector().handleToModule(module) catch return Vm.Error.UndefinedBehavior;
    const val = m.getBySymbol(symbol) orelse return Vm.Error.UndefinedBehavior;
    try vm.context.push(vm.allocator(), val);
}

fn eval(vm: *Vm, n: u32) !void {
    const start = vm.context.stackLen() - n;
    const proc_idx = start - 1;
    const proc_val = vm.context.stackVal(proc_idx) orelse
        return Vm.Error.UndefinedBehavior;
    switch (proc_val.data) {
        // TODO: Raise an exception.
        .empty_list, .int, .module, .pair, .symbol => return Vm.Error.NotImplemented,
        .proc => |h| {
            const proc = try vm.inspector().handleToProc(h);
            try vm.context.pushStackFrame(vm.allocator(), .{
                .stack_start = start,
                .instructions = proc.instructions,
            });
        },
        .proc_builtin => |b| return evalBuiltin(vm, b, n),
    }
}

fn evalBuiltin(vm: *Vm, builtin: Proc.Builtin, arg_count: u32) !void {
    const inspector = vm.inspector();
    const args = vm.context.stackTopN(arg_count);
    switch (builtin) {
        .add => {
            var sum: i64 = 0;
            // TODO: Raise an exception.
            for (args) |v| sum += inspector.asInt(v) catch return error.NotImplemented;
            try vm.context.push(vm.allocator(), Val.initInt(sum));
            // proc + args + return_value.
            try vm.context.stackSquash(arg_count + 2);
        },
    }
}
