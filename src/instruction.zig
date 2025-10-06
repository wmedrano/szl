const std = @import("std");

const Handle = @import("types/object_pool.zig").Handle;
const Module = @import("types/Module.zig");
const Proc = @import("types/Proc.zig");
const Symbol = @import("types/Symbol.zig");
const Val = @import("types/Val.zig");
const Vm = @import("Vm.zig");

pub const Instruction = union(enum) {
    push_const: Val,
    module_get: struct { module: Handle(Module), symbol: Symbol.Interned },
    get_arg: u32,
    jump: i32,
    jump_if_not: i32,
    squash: u32,
    eval: u32,
    ret,

    pub fn execute(self: Instruction, vm: *Vm) Vm.Error!void {
        switch (self) {
            .push_const => |val| try vm.context.push(vm.allocator(), val),
            .module_get => |g| try moduleGet(vm, g.module, g.symbol),
            .get_arg => |idx| {
                const val = vm.context.stackLocal()[@intCast(idx)];
                try vm.context.push(vm.allocator(), val);
            },
            .jump => |n| try vm.context.jump(n),
            .jump_if_not => |n| {
                const test_val = vm.context.pop() orelse return Vm.Error.UndefinedBehavior;
                if (!test_val.isTruthy()) try vm.context.jump(n);
            },
            .squash => |n| try vm.context.stackSquash(n),
            .eval => |n| try eval(vm, n),
            .ret => try vm.context.popStackFrame(.place_on_top),
        }
    }
};

fn moduleGet(vm: *Vm, module: Handle(Module), symbol: Symbol.Interned) !void {
    const m = vm.inspector().handleToModule(module) catch return Vm.Error.UndefinedBehavior;
    const val = m.getBySymbol(symbol) orelse {
        std.debug.print("Trying to get symbol {f}\n", .{vm.pretty(Val.initSymbol(symbol))});
        return Vm.Error.UndefinedBehavior;
    };
    try vm.context.push(vm.allocator(), val);
}

fn eval(vm: *Vm, arg_count: u32) !void {
    const start = vm.context.stackLen() - arg_count;
    const proc_idx = start - 1;
    const proc_val = vm.context.stackVal(proc_idx) orelse
        return Vm.Error.UndefinedBehavior;
    switch (proc_val.data) {
        // TODO: Raise an exception.
        .empty_list, .boolean, .int, .module, .pair, .symbol => return Vm.Error.NotImplemented,
        .proc => |h| {
            const proc = try vm.inspector().handleToProc(h);
            // TODO: Raise an error.
            if (arg_count != proc.arg_count) return Vm.Error.NotImplemented;
            try vm.context.pushStackFrame(vm.allocator(), .{
                .stack_start = start,
                .instructions = proc.instructions,
            });
        },
        .proc_builtin => |b| return evalBuiltin(vm, b, arg_count),
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
