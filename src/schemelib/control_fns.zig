const std = @import("std");
const testing = std.testing;

const Instruction = @import("../instruction.zig").Instruction;
const ErrorDetails = @import("../types/ErrorDetails.zig");
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

fn callCcImpl(vm: *Vm, diagnostics: *ErrorDetails, arg_count: u32) Vm.Error!void {
    if (arg_count != 1) {
        @branchHint(.cold);
        diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_count = .{
            .expected = 1,
            .got = arg_count,
            .proc = Val.initNativeProc(&call_cc),
        } });
        return Vm.Error.UncaughtException;
    }
    const proc = vm.context.pop() orelse return Vm.Error.UndefinedBehavior;
    if (!proc.isProc()) {
        @branchHint(.cold);
        diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
            .expected = "procedure",
            .got = proc,
            .proc = Val.initNativeProc(&call_cc),
            .arg_name = "proc",
            .arg_position = 0,
        } });
        return Vm.Error.UncaughtException;
    }
    const cont = try vm.builder().makeContinuation(vm.context);
    try vm.context.pushSlice(vm.allocator(), &.{ cont, proc });
    var error_details = ErrorDetails{};
    // Note: Don't deinit - if it gets converted to a Val, the VM will manage it
    try (Instruction{ .eval = 1 }).execute(vm, &error_details);
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

fn applyImpl(vm: *Vm, error_details: *ErrorDetails, arg_count: u32) Vm.Error!void {
    if (arg_count < 2) {
        @branchHint(.cold);
        error_details.addDiagnostic(vm.allocator(), .{ .wrong_arg_count = .{
            .expected = 2,
            .got = arg_count,
            .proc = Val.initNativeProc(&apply),
        } });
        return Vm.Error.UncaughtException;
    }
    const args = vm.context.stackTopNUnstable(arg_count);
    const proc = args[0];
    @memmove(args[0 .. args.len - 1], args[1..]);
    _ = vm.context.pop();
    const proc_args = vm.context.pop() orelse {
        return Vm.Error.UndefinedBehavior;
    };
    var rest_iter = vm.inspector().iteratePairs(proc_args);
    var proc_args_count = arg_count - 2;
    while (rest_iter.next()) |maybe_next| {
        if (maybe_next) |next| {
            proc_args_count += 1;
            try vm.context.push(vm.allocator(), next);
        } else break;
    } else |err| {
        @branchHint(.cold);
        switch (err) {
            error.UncaughtException => {
                error_details.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "list",
                    .got = proc_args,
                    .proc = Val.initNativeProc(&apply),
                    .arg_name = "args",
                    .arg_position = arg_count - 1,
                } });
                return Vm.Error.UncaughtException;
            },
            error.UndefinedBehavior => {
                error_details.addDiagnostic(
                    vm.allocator(),
                    .{ .undefined_behavior = "Invalid list structure in apply" },
                );
                return Vm.Error.UndefinedBehavior;
            },
        }
    }
    try vm.context.push(vm.allocator(), proc);
    try (Instruction{ .eval = proc_args_count }).execute(vm, error_details);
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
