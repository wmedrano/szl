const std = @import("std");
const testing = std.testing;

const Diagnostics = @import("../Diagnostics.zig");
const Instruction = @import("../instruction.zig").Instruction;
const NativeProc = @import("../types/NativeProc.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

pub const call_cc = NativeProc{
    .name = "call/cc",
    .unsafe_impl = &callCcImpl,
    .docstring =
    \\(call-with-current-continuation proc)
    \\(call/cc proc)
    \\
    \\It is an error if proc does not accept one argument.
    \\
    \\The procedure call-with-current-continuation (or its equivalent abbreviation call/cc) packages the current continuation as an "escape procedure" and passes it as an argument to proc. The escape procedure is a Scheme procedure that, if it is later called, will abandon whatever continuation is in effect at that later time and will instead use the continuation that was in effect when the escape procedure was created.
    ,
};

fn callCcImpl(vm: *Vm, diagnostics: ?*Diagnostics, arg_count: u32) Vm.Error!void {
    if (arg_count != 1) {
        if (diagnostics) |d| {
            d.appendWrongArgCount(.{ .expected = 1, .got = arg_count, .proc = Val.initNativeProc(&call_cc) });
        }
    }
    const proc = vm.context.pop() orelse return Vm.Error.NotImplemented;
    const cont = try vm.builder().makeContinuation(vm.context);
    try vm.context.pushSlice(vm.allocator(), &.{ cont, proc });
    try (Instruction{ .eval = 1 }).execute(vm, null);
}

pub const apply = NativeProc{
    .name = "apply",
    .unsafe_impl = &applyImpl,
    .docstring =
    \\(apply proc arg1 … args)
    \\
    \\The apply procedure calls proc with the elements of the list (append (list arg1 …) args) as the actual arguments.
    \\
    \\(apply + (list 3 4)) => 7
    ,
};

fn applyImpl(vm: *Vm, _: ?*Diagnostics, arg_count: u32) Vm.Error!void {
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
    try (Instruction{ .eval = proc_args_count }).execute(vm, null);
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
