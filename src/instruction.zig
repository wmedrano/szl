const std = @import("std");

const Module = @import("types/Module.zig");
const Handle = @import("types/object_pool.zig").Handle;
const Proc = @import("types/Proc.zig");
const Symbol = @import("types/Symbol.zig");
const Val = @import("types/Val.zig");
const Vector = @import("types/Vector.zig");
const Vm = @import("Vm.zig");

pub const Instruction = union(enum) {
    push_const: Val,
    get_global: struct { module: Handle(Module), symbol: Symbol.Interned },
    get_arg: u32,
    get_local: u32,
    get_capture: u32,
    set_local: u32,
    jump: i32,
    jump_if_not: i32,
    squash: u32,
    eval: u32,
    make_closure: Closure,
    ret,

    const Closure = struct {
        proc: Handle(Proc),
        capture_count: u32,
    };

    pub fn execute(self: Instruction, vm: *Vm) Vm.Error!void {
        switch (self) {
            .push_const => |val| try vm.context.push(vm.allocator(), val),
            .get_global => |g| try moduleGet(vm, g.module, g.symbol),
            .get_arg => |idx| {
                const val = try vm.context.getArg(idx);
                try vm.context.push(vm.allocator(), val);
            },
            .get_local => |idx| {
                const val = try vm.context.getLocal(idx);
                try vm.context.push(vm.allocator(), val);
            },
            .get_capture => |idx| {
                const val = try vm.context.getCapture(idx);
                try vm.context.push(vm.allocator(), val);
            },
            .set_local => |idx| {
                const val = vm.context.pop() orelse return Vm.Error.UndefinedBehavior;
                vm.context.setLocal(idx, val);
            },
            .jump => |n| try vm.context.jump(n),
            .jump_if_not => |n| {
                const test_val = vm.context.pop() orelse return Vm.Error.UndefinedBehavior;
                if (!test_val.isTruthy()) try vm.context.jump(n);
            },
            .squash => |n| try vm.context.stackSquash(n),
            .eval => |n| try eval(vm, n),
            .make_closure => |c| try makeClosure(vm, c.proc, c.capture_count),
            .ret => try vm.context.popStackFrame(.place_on_top),
        }
    }
};

fn moduleGet(vm: *Vm, module: Handle(Module), symbol: Symbol.Interned) !void {
    const m = vm.inspector().handleToModule(module) catch return Vm.Error.UndefinedBehavior;
    const val = m.getBySymbol(symbol) orelse return Vm.Error.UndefinedBehavior;
    try vm.context.push(vm.allocator(), val);
}

fn eval(vm: *Vm, arg_count: u32) !void {
    const start = vm.context.stackLen() - arg_count;
    const proc_idx = start - 1;
    const proc_val = vm.context.stackVal(proc_idx) orelse
        return Vm.Error.UndefinedBehavior;
    switch (proc_val.data) {
        // TODO: Raise an exception.
        .empty_list, .boolean, .int, .module, .pair, .symbol, .vector => return Vm.Error.NotImplemented,
        .proc => |h| try evalProc(vm, h, null, arg_count, start),
        .closure => |h| return try evalProc(vm, h.proc, h.captures, arg_count, start),
        .proc_builtin => |b| return evalBuiltin(vm, b, arg_count),
    }
}

fn evalProc(vm: *Vm, h: Handle(Proc), maybe_captures: ?Handle(Vector), arg_count: u32, start: usize) !void {
    const proc = try vm.inspector().handleToProc(h);
    // 1. Check arguments.
    if (arg_count != proc.arg_count) return Vm.Error.NotImplemented;
    // 2. Initialize locals.
    try vm.context.pushMany(vm.allocator(), Val.initEmptyList(), proc.locals_count);
    // 3. Initialize captures.
    const actual_captures: u32 = if (maybe_captures) |captures_h| blk: {
        const captures_vec = try vm.inspector().handleToVector(captures_h);
        const captures = captures_vec.asSlice();
        try vm.context.pushSlice(vm.allocator(), captures);
        break :blk @intCast(captures.len);
    } else 0;
    if (actual_captures != proc.captures_count) return Vm.Error.UndefinedBehavior;
    // 4. Set the context.
    try vm.context.pushStackFrame(vm.allocator(), .{
        .stack_start = @intCast(start),
        .arg_count = arg_count,
        .locals_count = proc.locals_count,
        .instructions = proc.instructions,
    });
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

fn makeClosure(vm: *Vm, proc: Handle(Proc), count: u32) !void {
    // TODO: Capture the variables mutable. Operations like `set!` should affect
    // all captured references.
    const capture_vals = vm.context.stackTopN(count);
    const captures_h = try vm.builder().makeVectorHandle(capture_vals);
    vm.context.popMany(count);
    try vm.context.push(vm.allocator(), Val.initClosure(proc, captures_h));
}
