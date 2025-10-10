const std = @import("std");
const testing = std.testing;

const Instruction = @import("../instruction.zig").Instruction;
const Vm = @import("../Vm.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");

const NativeProc = @This();

name: []const u8,
unsafe_impl: *const fn (*Vm, arg_count: u32) Vm.Error!void,

pub const add = NativeProc{
    .name = "+",
    .unsafe_impl = &addImpl,
};

fn addImpl(vm: *Vm, arg_count: u32) Vm.Error!void {
    const inspector = vm.inspector();
    const args = vm.context.stackTopN(arg_count);
    var sum: i64 = 0;
    // TODO: Raise an exception.
    for (args) |v| sum += inspector.asInt(v) catch return error.NotImplemented;
    vm.context.popMany(arg_count);
    try vm.context.push(vm.allocator(), Val.initInt(sum));
}

pub const lte = NativeProc{
    .name = "<=",
    .unsafe_impl = &lteImpl,
};

fn lteImpl(vm: *Vm, arg_count: u32) Vm.Error!void {
    const inspector = vm.inspector();
    const args = vm.context.stackTopN(arg_count);

    // Check if ordered by comparing adjacent pairs.
    // TODO: Raise an exception instead of NotImplemented.
    const is_ordered = switch (args.len) {
        0 => true,
        1 => blk: {
            // Validate single argument is an integer
            _ = inspector.asInt(args[0]) catch return error.NotImplemented;
            break :blk true;
        },
        else => blk: {
            var prev = inspector.asInt(args[0]) catch return error.NotImplemented;
            for (args[1..]) |v| {
                const curr = inspector.asInt(v) catch return error.NotImplemented;
                if (prev > curr) break :blk false;
                prev = curr;
            }
            break :blk true;
        },
    };
    vm.context.popMany(arg_count);
    try vm.context.push(vm.allocator(), Val.initBool(is_ordered));
}

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
        try builder.makeSymbolInterned(Symbol.init("scheme")),
        try builder.makeSymbolInterned(Symbol.init("base")),
    }) orelse return Vm.Error.UndefinedBehavior;
    const global_mod = try inspector.handleToModule(global_mod_handle);
    const raise_proc = global_mod.getBySymbol(try builder.makeSymbolInterned(Symbol.init("raise"))) orelse
        return Vm.Error.UndefinedBehavior;
    try vm.context.pushSlice(vm.allocator(), &.{ err, raise_proc });
    try (Instruction{ .eval = 1 }).execute(vm);
}

test "+ on ints sums ints" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initInt(10),
        try vm.evalStr("(+ 1 2 3 4)", null),
    );
}

test "empty + returns 0" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initInt(0),
        try vm.evalStr("(+)", null),
    );
}

test "+ on non-ints returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(+ #t)", null));
}

test "<= on ordered ints returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initBool(true),
        try vm.evalStr("(<= 1 2 3)", null),
    );
}

test "<= on non-ordered ints returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initBool(false),
        try vm.evalStr("(<= 3 2 1)", null),
    );
}

test "<= on equal ints returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initBool(true),
        try vm.evalStr("(<= 1 1 2)", null),
    );
}

test "<= with less than 2 args returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(Val.initBool(true), try vm.evalStr("(<= )", null));
    try testing.expectEqual(Val.initBool(true), try vm.evalStr("(<= 1)", null));
}

test "<= on non-ints returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(<= #t)", null));
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
        try vm.evalStr(source, null),
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
    try testing.expectEqual(Val.initInt(1), try vm.evalStr("one", null));
    try testing.expectEqual(Val.initInt(2), try vm.evalStr("two", null));
}

test "call/cc can stop exception from propagating" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const source =
        \\ (define (bad-thunk) (raise 'bad))
        \\ (call/cc (lambda (exit)
        \\   (with-exception-handler
        \\     (lambda (err) (exit 'recovered))
        \\     bad-thunk)))
    ;
    try testing.expectEqual(
        try vm.builder().makeSymbol(Symbol.init("recovered")),
        try vm.evalStr(source, null),
    );
}
