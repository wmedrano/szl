const std = @import("std");
const testing = std.testing;

const Context = @import("Context.zig");
const Closure = @import("types/Closure.zig");
const Continuation = @import("types/Continuation.zig");
const Module = @import("types/Module.zig");
const NativeProc = @import("types/NativeProc.zig");
const Handle = @import("types/object_pool.zig").Handle;
const Proc = @import("types/Proc.zig");
const Symbol = @import("types/Symbol.zig");
const Val = @import("types/Val.zig");
const Vector = @import("types/Vector.zig");
const Vm = @import("Vm.zig");

pub const Instruction = union(enum) {
    push_const: Val,
    get_global: struct { module: Handle(Module), symbol: Symbol },
    get_proc,
    get_arg: u32,
    get_local: u32,
    get_capture: u32,
    set_global: Global,
    set_local: u32,
    jump: i32,
    jump_if_not: i32,
    squash: u32,
    eval: u32,
    make_closure: Handle(Proc),
    ret,

    const Global = struct {
        module: Handle(Module),
        symbol: Symbol,
    };

    pub fn toVal(self: Instruction, vm: *Vm) Vm.Error!Val {
        const builder = vm.builder();
        switch (self) {
            .push_const => |val| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("push-const"),
                    val,
                };
                return builder.makeList(&items);
            },
            .get_global => |g| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("get-global"),
                    Val{ .data = .{ .module = g.module } },
                    Val.initSymbol(g.symbol),
                };
                return builder.makeList(&items);
            },
            .get_proc => {
                const items = [_]Val{
                    try builder.makeStaticSymbol("get-proc"),
                };
                return builder.makeList(&items);
            },
            .get_arg => |idx| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("get-arg"),
                    Val.initInt(@intCast(idx)),
                };
                return builder.makeList(&items);
            },
            .get_local => |idx| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("get-local"),
                    Val.initInt(@intCast(idx)),
                };
                return builder.makeList(&items);
            },
            .get_capture => |idx| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("get-capture"),
                    Val.initInt(@intCast(idx)),
                };
                return builder.makeList(&items);
            },
            .set_global => |g| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("set-global"),
                    Val{ .data = .{ .module = g.module } },
                    Val.initSymbol(g.symbol),
                };
                return builder.makeList(&items);
            },
            .set_local => |idx| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("set-local"),
                    Val.initInt(@intCast(idx)),
                };
                return builder.makeList(&items);
            },
            .jump => |n| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("jump"),
                    Val.initInt(n),
                };
                return builder.makeList(&items);
            },
            .jump_if_not => |n| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("jump-if-not"),
                    Val.initInt(n),
                };
                return builder.makeList(&items);
            },
            .squash => |n| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("squash"),
                    Val.initInt(@intCast(n)),
                };
                return builder.makeList(&items);
            },
            .eval => |n| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("eval"),
                    Val.initInt(@intCast(n)),
                };
                return builder.makeList(&items);
            },
            .make_closure => |h| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("make-closure"),
                    Val.initProc(h),
                };
                return builder.makeList(&items);
            },
            .ret => {
                const items = [_]Val{
                    try builder.makeStaticSymbol("ret"),
                };
                return builder.makeList(&items);
            },
        }
    }

    pub fn execute(self: Instruction, vm: *Vm) Vm.Error!void {
        switch (self) {
            .push_const => |val| try vm.context.push(vm.allocator(), val),
            .get_global => |g| try moduleGet(vm, g.module, g.symbol),
            .get_proc => {
                const val = vm.context.getProc();
                try vm.context.push(vm.allocator(), val);
            },
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
            .set_global => |g| {
                const val = vm.context.top() orelse return Vm.Error.UndefinedBehavior;
                const m = try vm.inspector().handleToModule(g.module);
                try m.setBySymbol(vm.allocator(), g.symbol, val);
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
            .make_closure => |proc| try makeClosure(vm, proc),
            .ret => try vm.context.popStackFrame(.place_on_top),
        }
    }
};

pub fn executeUntilEnd(vm: *Vm) Vm.Error!Val {
    while (vm.context.nextInstruction()) |instruction| {
        try instruction.execute(vm);
    }
    return vm.context.top() orelse Val.initEmptyList();
}

fn moduleGet(vm: *Vm, module: Handle(Module), symbol: Symbol) !void {
    const m = vm.inspector().handleToModule(module) catch return Vm.Error.UndefinedBehavior;
    const val = m.getBySymbol(symbol) orelse {
        return Vm.Error.UndefinedBehavior;
    };
    try vm.context.push(vm.allocator(), val);
}

const EvalOptions = struct {
    arg_count: u32,
    exception_handler: ?Val = null,
};

fn eval(vm: *Vm, arg_count: u32) Vm.Error!void {
    const proc_val = vm.context.pop() orelse return Vm.Error.UndefinedBehavior;
    const start = vm.context.stackLen() - arg_count;
    switch (proc_val.data) {
        // TODO: Raise an exception.
        .empty_list, .boolean, .int, .float, .module, .pair, .string, .symbol, .vector, .syntax_rules => return Vm.Error.NotImplemented,
        .proc => |h| try evalProc(vm, h, arg_count, start),
        .closure => |h| return try evalClosure(vm, h, arg_count, start),
        .native_proc => |p| return evalNativeProc(vm, p, arg_count),
        .continuation => |c| return evalContinuation(vm, c, arg_count),
    }
}

fn evalProc(vm: *Vm, h: Handle(Proc), arg_count: u32, start: usize) !void {
    const proc = try vm.inspector().handleToProc(h);
    // 1. Check arguments.
    if (arg_count != proc.arg_count) {
        return Vm.Error.NotImplemented;
    }
    // 2. Initialize locals.
    try vm.context.pushMany(vm.allocator(), Val.initEmptyList(), proc.locals_count);
    if (proc.captures_count != 0) return Vm.Error.UndefinedBehavior;
    // 3. Set the context.
    try vm.context.pushStackFrame(vm.allocator(), .{
        .stack_start = @intCast(start),
        .arg_count = arg_count,
        .proc = Val.initProc(h),
        .instructions = proc.instructions,
    });
}

fn evalClosure(vm: *Vm, h: Handle(Closure), arg_count: u32, start: usize) !void {
    const closure = try vm.inspector().handleToClosure(h);
    // 1. Check arguments.
    if (arg_count != closure.arg_count) {
        return Vm.Error.NotImplemented;
    }
    // 2. Initialize locals.
    try vm.context.pushMany(vm.allocator(), Val.initEmptyList(), closure.locals_count);
    // 3. Set the context.
    try vm.context.pushStackFrame(vm.allocator(), .{
        .stack_start = @intCast(start),
        .arg_count = arg_count,
        .proc = Val.initClosure(h),
        .instructions = closure.instructions,
        .captures = closure.captures,
    });
}

fn evalNativeProc(vm: *Vm, builtin: *const NativeProc, arg_count: u32) !void {
    try builtin.unsafe_impl(vm, arg_count);
}

fn evalContinuation(vm: *Vm, handle: Handle(Continuation), arg_count: u32) !void {
    const inspector = vm.inspector();
    if (arg_count != 1) return Vm.Error.NotImplemented;
    const arg = vm.context.pop() orelse return Vm.Error.UndefinedBehavior;
    const continuation = inspector.handleToContinuation(handle) catch
        return Vm.Error.NotImplemented;
    std.mem.swap(Context, &continuation.context, &vm.context);
    try vm.context.push(vm.allocator(), arg);
}

fn makeClosure(vm: *Vm, proc_h: Handle(Proc)) !void {
    // TODO: Capture the variables mutable. Operations like `set!` should affect
    // all captured references.
    const inspector = vm.inspector();
    const proc = try inspector.handleToProc(proc_h);
    const capture_vals = vm.context.stackTopN(proc.captures_count);
    const closure = try vm.builder().makeClosure(proc.*, capture_vals);
    try vm.context.push(vm.allocator(), Val.initClosure(closure));
}
