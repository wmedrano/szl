const std = @import("std");
const testing = std.testing;

const Instruction = @import("instruction.zig").Instruction;
const NativeProc = @import("types/NativeProc.zig");
const Symbol = @import("types/Symbol.zig");
const Val = @import("types/Val.zig");
const Vm = @import("Vm.zig");

pub const add = NativeProc.withRawArgs(struct {
    pub const name = "+";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        const inspector = vm.inspector();
        var sum: i64 = 0;
        // TODO: Raise an exception.
        for (args) |v| sum += inspector.asInt(v) catch return .{ .err = error.NotImplemented };
        return .{ .val = Val.initInt(sum) };
    }
});

pub const sub = NativeProc.withRawArgs(struct {
    pub const name = "-";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        const inspector = vm.inspector();
        // TODO: Raise an exception.
        const result: i64 = switch (args.len) {
            0 => return .{ .err = error.NotImplemented },
            1 => blk: {
                const n = inspector.asInt(args[0]) catch return .{ .err = error.NotImplemented };
                break :blk -n;
            },
            else => blk: {
                var diff = inspector.asInt(args[0]) catch return .{ .err = error.NotImplemented };
                for (args[1..]) |v| diff -= inspector.asInt(v) catch return .{ .err = error.NotImplemented };
                break :blk diff;
            },
        };
        return .{ .val = Val.initInt(result) };
    }
});

pub const lte = NativeProc.withRawArgs(struct {
    pub const name = "<=";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        const inspector = vm.inspector();

        // Check if ordered by comparing adjacent pairs.
        // TODO: Raise an exception instead of NotImplemented.
        const is_ordered = switch (args.len) {
            0 => true,
            1 => blk: {
                // Validate single argument is an integer
                _ = inspector.asInt(args[0]) catch return .{ .err = error.NotImplemented };
                break :blk true;
            },
            else => blk: {
                var prev = inspector.asInt(args[0]) catch return .{ .err = error.NotImplemented };
                for (args[1..]) |v| {
                    const curr = inspector.asInt(v) catch return .{ .err = error.NotImplemented };
                    if (prev > curr) break :blk false;
                    prev = curr;
                }
                break :blk true;
            },
        };
        return .{ .val = Val.initBool(is_ordered) };
    }
});

pub const call_cc = NativeProc{
    .name = "call/cc",
    .unsafe_impl = &callCcImpl,
};

fn callCcImpl(vm: *Vm, arg_count: u32) Vm.Error!void {
    if (arg_count != 1) return Vm.Error.NotImplemented;
    const proc = vm.context.pop() orelse return Vm.Error.NotImplemented;
    const cont = try vm.builder().makeContinuation(vm.context);
    try vm.context.pushSlice(vm.allocator(), &.{ cont, proc });
    try (Instruction{ .eval = 1 }).execute(vm);
}

pub const with_exception_handler = NativeProc{
    .name = "with-exception-handler",
    .unsafe_impl = &withExceptionHandlerImpl,
};

fn withExceptionHandlerImpl(vm: *Vm, arg_count: u32) Vm.Error!void {
    if (arg_count != 2) return Vm.Error.NotImplemented;
    const thunk = vm.context.pop() orelse return Vm.Error.UndefinedBehavior;
    const handler = vm.context.pop() orelse return Vm.Error.UndefinedBehavior;
    try vm.context.setExceptionHandler(handler);
    try vm.context.push(vm.allocator(), thunk);
    try (Instruction{ .eval = 0 }).execute(vm);
}

pub const raise_continuable = NativeProc{
    .name = "raise-continuable",
    .unsafe_impl = &raiseContinuableImpl,
};

fn raiseContinuableImpl(vm: *Vm, arg_count: u32) Vm.Error!void {
    if (arg_count != 1) return Vm.Error.NotImplemented;
    const handler = vm.context.currentExceptionHandler() orelse return Vm.Error.UncaughtException;
    try vm.context.push(vm.allocator(), handler);
    try (Instruction{ .eval = 1 }).execute(vm);
}

pub const szl_raise_next = NativeProc{
    .name = "%szl-raise-next",
    .unsafe_impl = &szlRaiseNextImpl,
};

fn szlRaiseNextImpl(vm: *Vm, arg_count: u32) Vm.Error!void {
    const inspector = vm.inspector();
    const builder = vm.builder();
    if (arg_count != 1) return Vm.Error.NotImplemented;
    const err = vm.context.top() orelse return Vm.Error.UndefinedBehavior;
    _ = try vm.context.unwindBeforeExceptionHandler();
    const global_mod_handle = inspector.findModule(&.{
        try builder.makeStaticSymbolHandle("scheme"),
        try builder.makeStaticSymbolHandle("base"),
    }) orelse return Vm.Error.UndefinedBehavior;
    const global_mod = try inspector.handleToModule(global_mod_handle);
    const raise_proc = global_mod.getBySymbol(try builder.makeStaticSymbolHandle("raise")) orelse
        return Vm.Error.UndefinedBehavior;
    try vm.context.pushSlice(vm.allocator(), &.{ err, raise_proc });
    try (Instruction{ .eval = 1 }).execute(vm);
}

pub const apply = NativeProc{
    .name = "apply",
    .unsafe_impl = &applyImpl,
};

fn applyImpl(vm: *Vm, arg_count: u32) Vm.Error!void {
    if (arg_count < 2) return Vm.Error.NotImplemented;
    const args = vm.context.stackTopNUnstable(arg_count);
    const proc = args[0];
    @memmove(args[0 .. args.len - 1], args[1..]);
    _ = vm.context.pop();
    const proc_args = vm.context.pop() orelse return Vm.Error.UndefinedBehavior;
    var rest_iter = vm.inspector().iteratePairs(proc_args);
    var proc_args_count = arg_count - 2;
    while (try rest_iter.next()) |next| {
        proc_args_count += 1;
        try vm.context.push(vm.allocator(), next);
    }
    try vm.context.push(vm.allocator(), proc);
    try (Instruction{ .eval = proc_args_count }).execute(vm);
}

pub const import = NativeProc{
    .name = "import",
    .unsafe_impl = &importImpl,
};

// TODO: This should support the full `import` syntax specified by r7rs.
fn importImpl(vm: *Vm, arg_count: u32) Vm.Error!void {
    if (arg_count != 1) return Vm.Error.NotImplemented;
    const inspector = vm.inspector();
    const module_specifier = try inspector.listToSliceAlloc(vm.allocator(), vm.context.top() orelse
        return Vm.Error.UndefinedBehavior);
    defer vm.allocator().free(module_specifier);
    const module_symbols = try vm.allocator().alloc(Symbol, module_specifier.len);
    defer vm.allocator().free(module_symbols);
    for (module_specifier, module_symbols) |val, *sym| {
        sym.* = val.asSymbol() orelse return Vm.Error.NotImplemented;
    }
    // TODO: Import into the correct environment.
    const dst_module = try inspector.getReplEnv();
    const src_module = inspector.findModule(module_symbols) orelse return Vm.Error.NotImplemented;
    try (try inspector.handleToModule(dst_module)).import(
        vm.allocator(),
        (try inspector.handleToModule(src_module)).*,
    );
}

pub const proc_instructions = NativeProc.withRawArgs(struct {
    pub const name = "proc-instructions";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 1) return .{ .err = error.NotImplemented };
        const proc_val = args[0];
        const proc_handle = switch (proc_val.data) {
            .proc => |h| h,
            .closure => |c| c.proc,
            else => return .{ .err = error.NotImplemented },
        };
        const proc = vm.objects.procs.get(proc_handle) orelse return .{ .err = error.UndefinedBehavior };
        const instructions = vm.allocator().alloc(Val, proc.instructions.len) catch |e| return .{ .err = e };
        defer vm.allocator().free(instructions);
        for (instructions, proc.instructions) |*dst, src| {
            dst.* = src.toVal(vm) catch |e| return .{ .err = e };
        }
        const list = vm.builder().makeList(instructions) catch |e| return .{ .err = e };
        return .{ .val = list };
    }
});

pub const string_length = NativeProc.withRawArgs(struct {
    pub const name = "string-length";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 1) return .{ .err = error.NotImplemented };
        const inspector = vm.inspector();
        const string = inspector.asString(args[0]) catch return .{ .err = error.WrongType };
        const len: i64 = @intCast(string.asSlice().len);
        return .{ .val = Val.initInt(len) };
    }
});

test "+ on ints sums ints" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("10", "(+ 1 2 3 4)");
}

test "empty + returns 0" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0", "(+)");
}

test "+ on non-ints returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(+ #t)", null));
}

test "- on two ints subtracts them" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("5", "(- 10 5)");
    try vm.expectEval("-5", "(- 5 10)");
}

test "- on multiple ints subtracts sequentially" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("5", "(- 10 3 2)");
    try vm.expectEval("0", "(- 10 5 5)");
}

test "- with single arg returns negation" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("-5", "(- 5)");
    try vm.expectEval("5", "(- -5)");
    try vm.expectEval("0", "(- 0)");
}

test "- with no args returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(-)", null));
}

test "- on non-ints returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(- #t)", null));
    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(- 5 #f)", null));
}

test "<= on ordered ints returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<= 1 2 3)");
}

test "<= on non-ordered ints returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(<= 3 2 1)");
}

test "<= on equal ints returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<= 1 1 2)");
}

test "<= with less than 2 args returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<= )");
    try vm.expectEval("#t", "(<= 1)");
}

test "<= on non-ints returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(<= #t)", null));
}

test "raise-continuable calls exception handler and continues" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval(
        "105",
        \\ (define (proc1 n) (raise-continuable n))
        \\ (define (proc2 n) (proc1 n))
        \\ (with-exception-handler
        \\   (lambda (x) (+ x 100))
        \\   (lambda () (proc2 5)))
        ,
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
        \\       (lambda () (raise 'exception)))))
    ;
    try testing.expectError(error.UncaughtException, vm.evalStr(source, null));
    try vm.expectEval("1", "one");
    try vm.expectEval("2", "two");
}

test "call/cc can stop exception from propagating" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval(
        "recovered",
        \\ (define (bad-thunk) (raise 'bad))
        \\ (call/cc (lambda (exit)
        \\   (with-exception-handler
        \\     (lambda (err) (exit 'recovered))
        \\     bad-thunk)))
        ,
    );
}

test "proc-instructions reveals bytecode" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval(
        "((get-arg 0) (push-const 1) (get-global #<environment:module:(user repl)> <=) (eval 2) (jump-if-not 2) (get-arg 0) (jump 14) (get-arg 0) (push-const 1) (get-global #<environment:module:(user repl)> -) (eval 2) (get-global #<environment:module:(user repl)> fib) (eval 1) (get-arg 0) (push-const 2) (get-global #<environment:module:(user repl)> -) (eval 2) (get-global #<environment:module:(user repl)> fib) (eval 1) (get-global #<environment:module:(user repl)> +) (eval 2) (ret))",
        \\ (define (fib n) (if (<= n 1)
        \\                   n
        \\                   (+ (fib (- n 1))
        \\                      (fib (- n 2)))))
        \\ (import '(sizzle unstable compiler))
        \\ (proc-instructions fib)
        ,
    );
}

test "string-length on empty string returns 0" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0", "(string-length \"\")");
}

test "string-length on normal string returns length" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("5", "(string-length \"hello\")");
    try vm.expectEval("11", "(string-length \"hello world\")");
}

test "string-length on string with escape sequences returns processed length" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("6", "(string-length \"hello\\n\")");
    try vm.expectEval("6", "(string-length \"hello\\t\")");
}

test "string-length on non-string returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.WrongType, vm.evalStr("(string-length 42)", null));
    try testing.expectError(Vm.Error.WrongType, vm.evalStr("(string-length #t)", null));
}

test "apply with only list argument then evaluates correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("10", "(apply + '(1 2 3 4))");
}

test "apply with one arg and list then evaluates correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("20", "(apply + 10 '(1 2 3 4))");
}

test "apply with two args and list then evaluates correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("40", "(apply + 10 20 '(1 2 3 4))");
}

test "apply with three args and list then evaluates correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("70", "(apply + 10 20 30 '(1 2 3 4))");
}
