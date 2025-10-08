const std = @import("std");
const testing = std.testing;

const Context = @import("Context.zig");
const Continuation = @import("types/Continuation.zig");
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
    get_arg: i32,
    get_local: u32,
    get_capture: u32,
    set_global: Global,
    set_local: u32,
    jump: i32,
    jump_if_not: i32,
    squash: u32,
    eval: u32,
    make_closure: Closure,
    ret,

    const Global = struct {
        module: Handle(Module),
        symbol: Symbol.Interned,
    };

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
            .make_closure => |c| try makeClosure(vm, c.proc, c.capture_count),
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

fn moduleGet(vm: *Vm, module: Handle(Module), symbol: Symbol.Interned) !void {
    const m = vm.inspector().handleToModule(module) catch return Vm.Error.UndefinedBehavior;
    const val = m.getBySymbol(symbol) orelse {
        return Vm.Error.UndefinedBehavior;
    };
    try vm.context.push(vm.allocator(), val);
}

const EvalOptoins = struct {
    arg_count: u32,
    exception_handler: ?Val = null,
};

fn eval(vm: *Vm, arg_count: u32) Vm.Error!void {
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
        .continuation => |c| return evalContinuation(vm, c, arg_count),
    }
}

fn evalProc(vm: *Vm, h: Handle(Proc), maybe_captures: ?Handle(Vector), arg_count: u32, start: usize) !void {
    const proc = try vm.inspector().handleToProc(h);
    // 1. Check arguments.
    if (arg_count != proc.arg_count) {
        return Vm.Error.NotImplemented;
    }
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
    const builder = vm.builder();
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
        .lte => {
            // Validate all arguments are ints first.
            // TODO: Raise an exception.
            var int_args = try std.ArrayList(i64).initCapacity(vm.allocator(), arg_count);
            defer int_args.deinit(vm.allocator());
            for (args) |v| {
                const val = inspector.asInt(v) catch return error.NotImplemented;
                int_args.appendAssumeCapacity(val);
            }

            // Check if ordered.
            var is_ordered = true;
            if (int_args.items.len > 1) {
                for (0..int_args.items.len - 1) |i| {
                    if (int_args.items[i] > int_args.items[i + 1]) {
                        is_ordered = false;
                        break;
                    }
                }
            }
            try vm.context.push(vm.allocator(), Val.initBool(is_ordered));
            // proc + args + return_value.
            try vm.context.stackSquash(arg_count + 2);
        },
        .call_cc => {
            if (arg_count != 1) return Vm.Error.NotImplemented;
            const cont = try vm.builder().makeContinuation(vm.context);
            try vm.context.push(vm.allocator(), cont);
            try eval(vm, 1);
        },
        .with_exception_handler => {
            if (arg_count != 2) return Vm.Error.NotImplemented;
            const thunk = vm.context.pop() orelse return Vm.Error.UndefinedBehavior;
            const handler = vm.context.pop() orelse return Vm.Error.UndefinedBehavior;
            try vm.context.setExceptionHandler(handler);
            try vm.context.push(vm.allocator(), thunk);
            try eval(vm, 0);
        },
        .raise_continuable => {
            if (arg_count != 1) return Vm.Error.NotImplemented;
            const obj = vm.context.pop() orelse return Vm.Error.UndefinedBehavior;
            const handler = vm.context.currentExceptionHandler() orelse return Vm.Error.UncaughtException;
            try vm.context.pushSlice(vm.allocator(), &.{ handler, obj });
            try eval(vm, 1);
        },
        .szl_raise_next => {
            if (arg_count != 1) return Vm.Error.NotImplemented;
            const obj = vm.context.pop() orelse return Vm.Error.UndefinedBehavior;
            _ = try vm.context.unwindUntilException();
            try vm.context.popStackFrame(.discard);
            _ = try vm.context.unwindUntilException();
            const global_mod_handle = inspector.findModule(&.{
                try builder.makeSymbolInterned(Symbol.init("scheme")),
                try builder.makeSymbolInterned(Symbol.init("base")),
            }) orelse return Vm.Error.UndefinedBehavior;
            const global_mod = try inspector.handleToModule(global_mod_handle);
            const raise_proc = global_mod.getBySymbol(try builder.makeSymbolInterned(Symbol.init("raise"))) orelse
                return Vm.Error.UndefinedBehavior;
            try vm.context.pushSlice(vm.allocator(), &.{ raise_proc, obj });
            try eval(vm, 1);
        },
    }
}

fn evalContinuation(vm: *Vm, handle: Handle(Continuation), arg_count: u32) !void {
    const inspector = vm.inspector();
    if (arg_count != 1) return Vm.Error.NotImplemented;
    const continuation = inspector.handleToContinuation(handle) catch
        return Vm.Error.NotImplemented;
    const arg = vm.context.pop() orelse return Vm.Error.UndefinedBehavior;
    std.mem.swap(Context, &continuation.context, &vm.context);
    try vm.context.push(vm.allocator(), arg);
}

fn makeClosure(vm: *Vm, proc: Handle(Proc), count: u32) !void {
    // TODO: Capture the variables mutable. Operations like `set!` should affect
    // all captured references.
    const capture_vals = vm.context.stackTopN(count);
    const captures_h = try vm.builder().makeVectorHandle(capture_vals);
    vm.context.popMany(count);
    try vm.context.push(vm.allocator(), Val.initClosure(proc, captures_h));
}

test "+ on ints sums ints" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initInt(10),
        try vm.evalStr("(+ 1 2 3 4)"),
    );
}

test "empty + returns 0" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initInt(0),
        try vm.evalStr("(+)"),
    );
}

test "+ on non-ints returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(+ #t)"));
}

test "<= on ordered ints returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initBool(true),
        try vm.evalStr("(<= 1 2 3)"),
    );
}

test "<= on non-ordered ints returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initBool(false),
        try vm.evalStr("(<= 3 2 1)"),
    );
}

test "<= on equal ints returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initBool(true),
        try vm.evalStr("(<= 1 1 2)"),
    );
}

test "<= with less than 2 args returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(Val.initBool(true), try vm.evalStr("(<= )"));
    try testing.expectEqual(Val.initBool(true), try vm.evalStr("(<= 1)"));
}

test "<= on non-ints returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(<= #t)"));
}

test "raise-continuable calls exception handler and continues" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const source =
        \\ (define (proc1 n) (raise-continuable n))
        \\ (define (proc2 n) (proc1 n))
        \\ (with-exception-handler
        \\   (lambda (x) (+ x 100))
        \\   (lambda () (proc2 5)))
    ;
    try testing.expectEqual(
        Val.initInt(105),
        try vm.evalStr(source),
    );
}

test "raise calls all exceptions" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const source =
        \\ (define one 0)
        \\ (define two 0)
        \\ (define (set-one! err) (define one 1))
        \\ (define (set-two! err) (define two 2))
        \\ (with-exception-handler set-one!
        \\   (lambda ()
        \\     (with-exception-handler set-two!
        \\       (lambda () (raise -1)))))
    ;
    try testing.expectError(error.UncaughtException, vm.evalStr(source));
    try testing.expectEqual(Val.initInt(1), try vm.evalStr("one"));
    try testing.expectEqual(Val.initInt(2), try vm.evalStr("two"));
}

test "call/cc can stop exception from propagating" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // TODO: Use symbols instead of ints once supported by reader.
    const source =
        \\ (define (bad-thunk) (raise 0))
        \\ (call/cc (lambda (exit)
        \\   (with-exception-handler
        \\     (lambda (err) (exit -1))
        \\     bad-thunk)))
    ;
    try testing.expectEqual(
        Val.initInt(-1),
        try vm.evalStr(source),
    );
}
