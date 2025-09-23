//! Pair operations for the Scheme interpreter.
//!
//! This module provides native implementations of pair operations
//! including type checking (pair?), construction (cons), and accessors (car, cdr).

const std = @import("std");
const testing = std.testing;

const instruction = @import("../instruction.zig");
const Instruction = instruction.Instruction;
const Pair = @import("../types/Pair.zig");
const Procedure = @import("../Procedure.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

// Define all native procedures as constants
const pair_predicate_native = Procedure.Native{
    .name = "pair?",
    .func = pairPredicateFunc,
};

const cons_native = Procedure.Native{
    .name = "cons",
    .func = consFunc,
};

const car_native = Procedure.Native{
    .name = "car",
    .func = carFunc,
};

const cdr_native = Procedure.Native{
    .name = "cdr",
    .func = cdrFunc,
};

/// Registers all pair functions with the virtual machine.
///
/// Args:
///   vm: Pointer to the VM instance to register pair functions with.
///
/// Errors:
///   May return allocation errors if registering functions fails.
pub fn register(vm: *Vm) !void {
    try vm.builder().defineNativeProc(&pair_predicate_native);
    try vm.builder().defineNativeProc(&cons_native);
    try vm.builder().defineNativeProc(&car_native);
    try vm.builder().defineNativeProc(&cdr_native);
}

/// Native implementation of the 'pair?' type predicate.
/// Returns #t if the argument is a pair, #f otherwise.
/// Expects exactly one argument.
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   A Val containing #t if the argument is a pair, #f otherwise.
///   If wrong number of arguments is provided, raises an error instead of returning.
fn pairPredicateFunc(ctx: Procedure.Context) Val {
    const args = ctx.localStack();
    if (args.len != 1) {
        instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
        return Val.init({});
    }

    switch (args[0].repr) {
        .pair => return Val.init(true),
        else => return Val.init(false),
    }
}

/// Native implementation of the 'cons' constructor.
/// Creates a new pair with the given car and cdr values.
/// Expects exactly two arguments.
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   A Val containing the new pair.
///   If wrong number of arguments is provided, raises an error instead of returning.
fn consFunc(ctx: Procedure.Context) Val {
    const args = ctx.localStack();
    if (args.len != 2) {
        instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
        return Val.init({});
    }

    const pair = Pair.init(args[0], args[1]);
    return ctx.vm.builder().build(pair) catch {
        instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
        return Val.init({});
    };
}

/// Native implementation of the 'car' accessor.
/// Returns the first element (car) of a pair.
/// Expects exactly one argument that must be a pair.
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   A Val containing the car of the pair.
///   If wrong number of arguments is provided or argument is not a pair, raises an error instead of returning.
fn carFunc(ctx: Procedure.Context) Val {
    const args = ctx.localStack();
    if (args.len != 1) {
        instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
        return Val.init({});
    }

    switch (args[0].repr) {
        .pair => |handle| {
            const pair = ctx.vm.inspector().resolve(Pair, handle) catch {
                instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init({});
            };
            return pair.car;
        },
        else => {
            instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
            return Val.init({});
        },
    }
}

/// Native implementation of the 'cdr' accessor.
/// Returns the second element (cdr) of a pair.
/// Expects exactly one argument that must be a pair.
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   A Val containing the cdr of the pair.
///   If wrong number of arguments is provided or argument is not a pair, raises an error instead of returning.
fn cdrFunc(ctx: Procedure.Context) Val {
    const args = ctx.localStack();
    if (args.len != 1) {
        instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
        return Val.init({});
    }

    switch (args[0].repr) {
        .pair => |handle| {
            const pair = ctx.vm.inspector().resolve(Pair, handle) catch {
                instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init({});
            };
            return pair.cdr;
        },
        else => {
            instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
            return Val.init({});
        },
    }
}

////////////////////////////////////////////////////////////////////////////////
// Pair predicate tests
////////////////////////////////////////////////////////////////////////////////

test "pair? with pair returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(pair? (cons 1 2))");
}

test "pair? with list returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(pair? '(1 2 3))");
}

test "pair? with nil returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(pair? '())");
}

test "pair? with integer returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(pair? 42)");
}

test "pair? with symbol returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(pair? 'hello)");
}

test "pair? with no arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(pair?)"),
    );
}

test "pair? with multiple arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(pair? (cons 1 2) (cons 3 4))"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// Cons tests
////////////////////////////////////////////////////////////////////////////////

test "cons creates pair" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(1 . 2)", "(cons 1 2)");
}

test "cons with nil creates list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(1)", "(cons 1 '())");
}

test "cons with list extends list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(0 1 2)", "(cons 0 '(1 2))");
}

test "cons with no arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(cons)"),
    );
}

test "cons with one argument is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(cons 1)"),
    );
}

test "cons with three arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(cons 1 2 3)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// Car tests
////////////////////////////////////////////////////////////////////////////////

test "car returns first element of pair" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1", "(car (cons 1 2))");
}

test "car returns first element of list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1", "(car '(1 2 3))");
}

test "car with nil is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(car '())"),
    );
}

test "car with non-pair is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(car 42)"),
    );
}

test "car with no arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(car)"),
    );
}

test "car with multiple arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(car (cons 1 2) (cons 3 4))"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// Cdr tests
////////////////////////////////////////////////////////////////////////////////

test "cdr returns second element of pair" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("2", "(cdr (cons 1 2))");
}

test "cdr returns rest of list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(2 3)", "(cdr '(1 2 3))");
}

test "cdr of single element list returns nil" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("()", "(cdr '(1))");
}

test "cdr with nil is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(cdr '())"),
    );
}

test "cdr with non-pair is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(cdr 42)"),
    );
}

test "cdr with no arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(cdr)"),
    );
}

test "cdr with multiple arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(cdr (cons 1 2) (cons 3 4))"),
    );
}