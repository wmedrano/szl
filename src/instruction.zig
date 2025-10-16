const std = @import("std");
const testing = std.testing;

const Context = @import("Context.zig");
const Diagnostics = @import("Diagnostics.zig");
const Continuation = @import("types/Continuation.zig");
const Module = @import("types/Module.zig");
const NativeProc = @import("types/NativeProc.zig");
const Handle = @import("types/object_pool.zig").Handle;
const Proc = @import("types/Proc.zig");
const Symbol = @import("types/Symbol.zig");
const Val = @import("types/Val.zig");
const Vector = Val.Vector;
const Vm = @import("Vm.zig");

pub const Instruction = union(enum) {
    load_const: i32,
    load_global: Symbol,
    load_proc,
    load_arg: u32,
    load_local: u32,
    set_global: Symbol,
    set_local: u32,
    jump: i32,
    jump_if_not: i32,
    squash: u32,
    eval: u32,
    make_closure: Handle(Proc),
    ret,

    pub fn toVal(self: Instruction, vm: *Vm) Vm.Error!Val {
        const builder = vm.builder();
        switch (self) {
            .load_const => |idx| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("load-const"),
                    Val.initInt(idx),
                };
                return builder.makeList(&items);
            },
            .load_global => |sym| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("load-global"),
                    Val.initSymbol(sym),
                };
                return builder.makeList(&items);
            },
            .load_proc => {
                const items = [_]Val{
                    try builder.makeStaticSymbol("load-proc"),
                };
                return builder.makeList(&items);
            },
            .load_arg => |idx| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("load-arg"),
                    Val.initInt(@intCast(idx)),
                };
                return builder.makeList(&items);
            },
            .load_local => |idx| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("load-local"),
                    Val.initInt(@intCast(idx)),
                };
                return builder.makeList(&items);
            },
            .set_global => |sym| {
                const items = [_]Val{
                    try builder.makeStaticSymbol("set-global"),
                    Val.initSymbol(sym),
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

    pub fn execute(self: Instruction, vm: *Vm, diagnostics: ?*Diagnostics) Vm.Error!void {
        errdefer if (diagnostics) |d| d.setStackTrace(vm);
        switch (self) {
            .load_const => |idx| {
                const val = vm.context.getConstant(@intCast(idx));
                try vm.context.push(vm.allocator(), val);
            },
            .load_global => |sym| try moduleGet(vm, sym, diagnostics),
            .load_proc => {
                const val = vm.context.getProc();
                try vm.context.push(vm.allocator(), val);
            },
            .load_arg => |idx| {
                const val = vm.context.getArg(idx);
                try vm.context.push(vm.allocator(), val);
            },
            .load_local => |idx| {
                const val = vm.context.getLocal(idx);
                try vm.context.push(vm.allocator(), val);
            },
            .set_global => |sym| {
                const val = vm.context.top() orelse return Vm.Error.UndefinedBehavior;
                const m = try vm.inspector().handleToModule(vm.context.module().?);
                try m.setBySymbol(vm.allocator(), sym, val);
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
            .eval => |n| try eval(vm, n, diagnostics),
            .make_closure => |proc| try makeClosure(vm, proc),
            .ret => _ = vm.context.popStackFrame(.return_top),
        }
    }
};

pub fn executeUntilEnd(vm: *Vm, diagnostics: ?*Diagnostics) Vm.Error!Val {
    while (vm.context.nextInstruction()) |instruction| {
        try instruction.execute(vm, diagnostics);
    }
    return vm.context.top() orelse Val.initEmptyList();
}

fn moduleGet(vm: *Vm, symbol: Symbol, diagnostics: ?*Diagnostics) !void {
    const module = vm.context.module() orelse {
        if (diagnostics) |d| d.addDiagnostic(.{ .undefined_behavior = "Failed to get current environment" });
        return Vm.Error.UndefinedBehavior;
    };
    const m = vm.inspector().handleToModule(module) catch {
        if (diagnostics) |d| d.addDiagnostic(.{ .undefined_behavior = "Failed to resolve module handle" });
        return Vm.Error.UndefinedBehavior;
    };
    const val = m.getBySymbol(symbol) orelse {
        if (diagnostics) |d| d.addDiagnostic(.{ .undefined_variable = .{
            .module = module,
            .symbol = symbol,
        } });
        return Vm.Error.UncaughtException;
    };
    try vm.context.push(vm.allocator(), val);
}

const EvalOptions = struct {
    arg_count: u32,
    exception_handler: ?Val = null,
};

fn eval(vm: *Vm, arg_count: u32, diagnostics: ?*Diagnostics) Vm.Error!void {
    const proc_val = vm.context.pop() orelse {
        if (diagnostics) |d| d.addDiagnostic(.{ .undefined_behavior = "VM.eval could not find any value to call" });
        return Vm.Error.UndefinedBehavior;
    };
    const start = vm.context.stackLen() - arg_count;
    switch (proc_val.data) {
        .empty_list, .boolean, .int, .float, .char, .module, .pair, .string, .symbol, .vector, .bytevector, .syntax_rules, .record, .record_descriptor => {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .not_callable = proc_val });
            }
            return Vm.Error.UncaughtException;
        },
        .proc => |h| try evalProc(vm, h, arg_count, start, diagnostics),
        .native_proc => |p| return evalNativeProc(vm, diagnostics, p, start, arg_count),
        .continuation => |c| return evalContinuation(vm, c, arg_count, diagnostics),
    }
}

fn evalProc(vm: *Vm, h: Handle(Proc), arg_count: u32, start: usize, diagnostics: ?*Diagnostics) !void {
    const proc = try vm.inspector().handleToProc(h);
    // 1. Check arguments.
    if (arg_count != proc.arg_count) {
        if (diagnostics) |d| d.addDiagnostic(.{ .wrong_arg_count = .{
            .expected = proc.arg_count,
            .got = arg_count,
            .proc = Val.initProc(h),
        } });
        return Vm.Error.UncaughtException;
    }
    // 2. Initialize locals.
    try vm.context.pushMany(vm.allocator(), Val.initBool(false), proc.locals_count);
    // 3. Set the context.
    try vm.context.pushStackFrame(vm.allocator(), .{
        .stack_start = @intCast(start),
        .arg_count = arg_count,
        .module = proc.module,
        .proc = Val.initProc(h),
        .instructions = proc.instructions,
        .constants = proc.constants,
    });
}

fn evalNativeProc(vm: *Vm, diagnostics: ?*Diagnostics, builtin: *const NativeProc, stack_start: u32, arg_count: u32) !void {
    // 1. Set the stack frame for debugging purposes.
    try vm.context.pushStackFrame(vm.allocator(), .{
        .stack_start = stack_start,
        .arg_count = arg_count,
        .instruction_idx = 0,
        .module = null,
        .proc = Val.initNativeProc(builtin),
        // If the native function does not pop its own stack, it will
        // automatically return its top value.
        .instructions = &.{.{ .ret = {} }},
        .constants = &.{},
    });
    try builtin.unsafe_impl(vm, diagnostics, arg_count);
}

fn evalContinuation(vm: *Vm, handle: Handle(Continuation), arg_count: u32, diagnostics: ?*Diagnostics) !void {
    const inspector = vm.inspector();
    if (arg_count != 1) {
        if (diagnostics) |d| {
            d.addDiagnostic(.{ .wrong_arg_count = .{
                .expected = 1,
                .got = arg_count,
                .proc = Val.initContinuation(handle),
            } });
        }
        return Vm.Error.UncaughtException;
    }
    const arg = vm.context.pop() orelse return Vm.Error.UndefinedBehavior;
    const continuation = inspector.handleToContinuation(handle) catch {
        if (diagnostics) |d| d.addDiagnostic(.{ .undefined_behavior = "Failed to resolve continuation handle" });
        return Vm.Error.UndefinedBehavior;
    };
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

test "instruction is small" {
    try testing.expectEqual(8, @sizeOf(Instruction));
}
