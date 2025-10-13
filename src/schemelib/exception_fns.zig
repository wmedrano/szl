const std = @import("std");
const testing = std.testing;

const Diagnostics = @import("../Diagnostics.zig");
const Instruction = @import("../instruction.zig").Instruction;
const NativeProc = @import("../types/NativeProc.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

pub const with_exception_handler = NativeProc{
    .name = "with-exception-handler",
    .unsafe_impl = &withExceptionHandlerImpl,
    .docstring =
    \\(with-exception-handler handler thunk)
    \\
    \\It is an error if handler does not accept one argument. It is also an error if thunk does not accept zero arguments.
    \\
    \\The with-exception-handler procedure returns the results of invoking thunk. Handler is installed as the current exception handler in the dynamic environment used for the invocation of thunk.
    ,
};

fn withExceptionHandlerImpl(vm: *Vm, diagnostics: ?*Diagnostics, arg_count: u32) Vm.Error!void {
    // TODO: Add diagnostics
    if (arg_count != 2) {
        if (diagnostics) |d| {
            d.appendWrongArgCount(.{ .expected = 2, .got = arg_count, .proc = Val.initNativeProc(&with_exception_handler) });
        }
        return Vm.Error.UncaughtException;
    }
    const thunk = vm.context.pop() orelse return Vm.Error.NotImplemented;
    const handler = vm.context.pop() orelse return Vm.Error.NotImplemented;
    try vm.context.setExceptionHandler(handler);
    try vm.context.push(vm.allocator(), thunk);
    try (Instruction{ .eval = 0 }).execute(vm, null);
}

pub const raise_continuable = NativeProc{
    .name = "raise-continuable",
    .unsafe_impl = &raiseContinuableImpl,
    .docstring =
    \\(raise-continuable obj)
    \\
    \\Raises a continuable exception by invoking the current exception handler on obj. The handler is called with the same dynamic environment as that of the call to raise-continuable, with the exception that the current exception handler is the one that was in place when the handler being called was installed. If the handler returns, the values it returns become the values returned by the call to raise-continuable.
    ,
};

fn raiseContinuableImpl(vm: *Vm, diagnostics: ?*Diagnostics, arg_count: u32) Vm.Error!void {
    if (arg_count != 1) {
        if (diagnostics) |d| {
            d.appendWrongArgCount(.{ .expected = 1, .got = arg_count, .proc = Val.initNativeProc(&raise_continuable) });
        }
        return Vm.Error.UncaughtException;
    }
    const handler = vm.context.currentExceptionHandler() orelse return Vm.Error.UncaughtException;
    try vm.context.push(vm.allocator(), handler);
    try (Instruction{ .eval = 1 }).execute(vm, null);
}

pub const szl_raise_next = NativeProc{
    .name = "%szl-raise-next",
    .unsafe_impl = &szlRaiseNextImpl,
    .docstring =
    \\(%szl-raise-next obj)
    \\
    \\Internal procedure for propagating exceptions to the next handler in the chain. This is not part of the R7RS standard.
    ,
};

fn szlRaiseNextImpl(vm: *Vm, _: ?*Diagnostics, arg_count: u32) Vm.Error!void {
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
    try (Instruction{ .eval = 1 }).execute(vm, null);
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
    try testing.expectError(error.UncaughtException, vm.evalStr(source, null, null));
    try vm.expectEval("1", "one");
    try vm.expectEval("2", "two");
}
