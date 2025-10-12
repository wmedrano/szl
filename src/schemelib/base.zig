const std = @import("std");
const testing = std.testing;

const Instruction = @import("../instruction.zig").Instruction;
const Module = @import("../types/Module.zig");
const NativeProc = @import("../types/NativeProc.zig");
const Handle = @import("../types/object_pool.zig").Handle;
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Builder = @import("../utils/Builder.zig");
const Vm = @import("../Vm.zig");
const boolean_fns = @import("boolean_fns.zig");
const equivalence_fns = @import("equivalence_fns.zig");
const number_fns = @import("number_fns.zig");

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

pub fn init(vm: *Vm) Vm.Error!Handle(Module) {
    const b = vm.builder();

    const env_handle = try b.makeEnvironment(&.{
        (try b.makeStaticSymbolHandle("scheme")),
        (try b.makeStaticSymbolHandle("base")),
    }, &[_]Builder.Definition{
        .{ .symbol = (try b.makeStaticSymbolHandle("+")), .value = Val.initNativeProc(&number_fns.add) },
        .{ .symbol = (try b.makeStaticSymbolHandle("-")), .value = Val.initNativeProc(&number_fns.sub) },
        .{ .symbol = (try b.makeStaticSymbolHandle("<")), .value = Val.initNativeProc(&number_fns.lt) },
        .{ .symbol = (try b.makeStaticSymbolHandle("<=")), .value = Val.initNativeProc(&number_fns.lte) },
        .{ .symbol = (try b.makeStaticSymbolHandle(">")), .value = Val.initNativeProc(&number_fns.gt) },
        .{ .symbol = (try b.makeStaticSymbolHandle("number?")), .value = Val.initNativeProc(&number_fns.number_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("integer?")), .value = Val.initNativeProc(&number_fns.integer_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("not")), .value = Val.initNativeProc(&boolean_fns.not) },
        .{ .symbol = (try b.makeStaticSymbolHandle("boolean?")), .value = Val.initNativeProc(&boolean_fns.boolean_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("eq?")), .value = Val.initNativeProc(&equivalence_fns.eq_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("equal?")), .value = Val.initNativeProc(&equivalence_fns.equal_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("apply")), .value = Val.initNativeProc(&apply) },
        .{ .symbol = (try b.makeStaticSymbolHandle("call/cc")), .value = Val.initNativeProc(&call_cc) },
        .{ .symbol = (try b.makeStaticSymbolHandle("call-with-current-continuation")), .value = Val.initNativeProc(&call_cc) },
        .{ .symbol = (try b.makeStaticSymbolHandle("with-exception-handler")), .value = Val.initNativeProc(&with_exception_handler) },
        .{ .symbol = (try b.makeStaticSymbolHandle("raise-continuable")), .value = Val.initNativeProc(&raise_continuable) },
        .{ .symbol = (try b.makeStaticSymbolHandle("%szl-raise-next")), .value = Val.initNativeProc(&szl_raise_next) },
        .{ .symbol = (try b.makeStaticSymbolHandle("import")), .value = Val.initNativeProc(&import) },
        .{ .symbol = (try b.makeStaticSymbolHandle("string-length")), .value = Val.initNativeProc(&string_length) },
    });
    _ = try vm.evalStr(@embedFile("base.scm"), env_handle);

    return env_handle;
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

test "cond" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try vm.expectEval(
        "greater",
        "(cond ((> 3 2) 'greater) ((< 3 2) 'less))",
    );
    try vm.expectEval(
        "equal",
        "(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))",
    );
}

test "unless" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try vm.expectEval("a", "(unless #f 'a)");
    try vm.expectEval("c", "(unless #f 'a 'b' 'c)");
    try vm.expectEval("()", "(unless #t 'a)");
    try vm.expectEval("()", "(unless #t 'a 'b 'c)");
}
