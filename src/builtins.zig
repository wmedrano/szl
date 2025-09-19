//! Built-in functions and procedures for the Scheme interpreter.
//!
//! This module provides the registration of native built-in functions that
//! are available to Scheme programs at runtime. These functions form the
//! core standard library of operations available in the interpreter.

const std = @import("std");
const testing = std.testing;

const Instruction = @import("Instruction.zig");
const Procedure = @import("Procedure.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

/// Registers all built-in functions with the virtual machine.
///
/// This function should be called during VM initialization to make all
/// standard Scheme built-in procedures available to executing programs.
///
/// Args:
///   vm: Pointer to the VM instance to register built-ins with.
///
/// Errors:
///   May return allocation errors if registering built-ins fails.
pub fn register(vm: *Vm) !void {
    try vm.builder().define(
        Symbol.init("szl-define"),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init("szl-define")),
            .implementation = Procedure.initNative(szlDefineFunc),
        }),
    );
    try vm.builder().define(
        Symbol.init("+"),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init("+")),
            .implementation = Procedure.initNative(addFunc),
        }),
    );
    try vm.builder().define(
        Symbol.init("-"),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init("-")),
            .implementation = Procedure.initNative(subFunc),
        }),
    );
    try vm.builder().define(
        Symbol.init("*"),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init("*")),
            .implementation = Procedure.initNative(mulFunc),
        }),
    );
    try vm.builder().define(
        Symbol.init("/"),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init("/")),
            .implementation = Procedure.initNative(divFunc),
        }),
    );
    try vm.builder().define(
        Symbol.init("<"),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init("<")),
            .implementation = Procedure.initNative(lessThanFunc),
        }),
    );
    try vm.builder().define(
        Symbol.init(">"),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init(">")),
            .implementation = Procedure.initNative(greaterThanFunc),
        }),
    );
    try vm.builder().define(
        Symbol.init("<="),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init("<=")),
            .implementation = Procedure.initNative(lessThanOrEqualFunc),
        }),
    );
    try vm.builder().define(
        Symbol.init(">="),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init(">=")),
            .implementation = Procedure.initNative(greaterThanOrEqualFunc),
        }),
    );
    try vm.builder().define(
        Symbol.init("="),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init("=")),
            .implementation = Procedure.initNative(equalFunc),
        }),
    );
}

/// Native implementation of the '+' arithmetic operator.
/// Adds all numeric arguments together, returning 0 for no arguments.
/// Expects all arguments to be numeric values (either i64 or f64).
/// Returns f64 if any operand is f64, otherwise returns i64.
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   A Val containing the sum of all numeric arguments.
///   If any argument is not numeric, raises an error instead of returning.
fn addFunc(ctx: Procedure.Context) Val {
    const args = ctx.localStack();
    var sum_i64: i64 = 0;
    var sum_f64: f64 = 0.0;
    var has_float = false;

    for (args) |arg| {
        switch (arg.repr) {
            .i64 => |val| sum_i64 += val,
            .f64 => |val| {
                has_float = true;
                sum_f64 += val;
            },
            else => {
                Instruction.raiseWithError(ctx.vm, Val.init({}));
                return Val.init({});
            },
        }
    }

    if (has_float) {
        sum_f64 += @as(f64, @floatFromInt(sum_i64));
        return Val.init(sum_f64);
    } else {
        return Val.init(sum_i64);
    }
}

/// Native implementation of the '-' arithmetic operator.
/// Subtracts all subsequent numeric arguments from the first argument.
/// With no arguments, returns 0. With one argument, returns its negation.
/// Expects all arguments to be numeric values (either i64 or f64).
/// Returns f64 if any operand is f64, otherwise returns i64.
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   A Val containing the result of the subtraction operation.
///   If any argument is not numeric, raises an error instead of returning.
fn subFunc(ctx: Procedure.Context) Val {
    const args = ctx.localStack();
    if (args.len == 0) return Val.init(0);

    var result_i64: i64 = 0;
    var result_f64: f64 = 0.0;
    var has_float = false;

    // Handle first argument
    switch (args[0].repr) {
        .i64 => |val| result_i64 = val,
        .f64 => |val| {
            has_float = true;
            result_f64 = val;
        },
        else => {
            Instruction.raiseWithError(ctx.vm, Val.init({}));
            return Val.init({});
        },
    }

    // If only one argument, return its negation
    if (args.len == 1) {
        if (has_float) {
            return Val.init(-result_f64);
        } else {
            return Val.init(-result_i64);
        }
    }

    // Subtract remaining arguments
    for (args[1..]) |arg| {
        switch (arg.repr) {
            .i64 => |val| result_i64 -= val,
            .f64 => |val| {
                has_float = true;
                result_f64 -= val;
            },
            else => {
                Instruction.raiseWithError(ctx.vm, Val.init({}));
                return Val.init({});
            },
        }
    }

    if (has_float) {
        result_f64 += @as(f64, @floatFromInt(result_i64));
        return Val.init(result_f64);
    } else {
        return Val.init(result_i64);
    }
}

/// Native implementation of the '*' arithmetic operator.
/// Multiplies all numeric arguments together, returning 1 for no arguments.
/// Expects all arguments to be numeric values (either i64 or f64).
/// Returns f64 if any operand is f64, otherwise returns i64.
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   A Val containing the product of all numeric arguments.
///   If any argument is not numeric, raises an error instead of returning.
fn mulFunc(ctx: Procedure.Context) Val {
    const args = ctx.localStack();
    var result_i64: i64 = 1;
    var result_f64: f64 = 1.0;
    var has_float = false;

    for (args) |arg| {
        switch (arg.repr) {
            .i64 => |val| result_i64 *= val,
            .f64 => |val| {
                has_float = true;
                result_f64 *= val;
            },
            else => {
                Instruction.raiseWithError(ctx.vm, Val.init({}));
                return Val.init({});
            },
        }
    }

    if (has_float) {
        result_f64 *= @as(f64, @floatFromInt(result_i64));
        return Val.init(result_f64);
    } else {
        return Val.init(result_i64);
    }
}

/// Native implementation of the '/' arithmetic operator.
/// Divides the first argument by all subsequent arguments.
/// All arguments are converted to f64 for division operations.
/// With one argument, returns 1.0 / argument (reciprocal).
/// Expects all arguments to be numeric values (either i64 or f64).
/// Always returns f64.
///
/// TODO: This implementation will change once rational numbers are supported.
/// Division should ideally preserve exact rationals when possible.
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   A Val containing the result of the division operation as f64.
///   If any argument is not numeric or division by zero occurs, raises an error instead of returning.
fn divFunc(ctx: Procedure.Context) Val {
    const args = ctx.localStack();
    if (args.len == 0) {
        Instruction.raiseWithError(ctx.vm, Val.init({}));
        return Val.init({});
    }

    // Convert first argument to f64
    var result: f64 = switch (args[0].repr) {
        .i64 => |val| @as(f64, @floatFromInt(val)),
        .f64 => |val| val,
        else => {
            Instruction.raiseWithError(ctx.vm, Val.init({}));
            return Val.init({});
        },
    };

    // If only one argument, return its reciprocal
    if (args.len == 1) {
        if (result == 0.0) {
            Instruction.raiseWithError(ctx.vm, Val.init({}));
            return Val.init({});
        }
        return Val.init(1.0 / result);
    }

    // Divide by remaining arguments
    for (args[1..]) |arg| {
        const divisor: f64 = switch (arg.repr) {
            .i64 => |val| @as(f64, @floatFromInt(val)),
            .f64 => |val| val,
            else => {
                Instruction.raiseWithError(ctx.vm, Val.init({}));
                return Val.init({});
            },
        };

        if (divisor == 0.0) {
            Instruction.raiseWithError(ctx.vm, Val.init({}));
            return Val.init({});
        }

        result /= divisor;
    }

    return Val.init(result);
}

/// Native implementation of the 'szl-define' procedure for defining global variables.
/// Defines a global variable with the given symbol name and value.
/// Expects exactly two arguments: a symbol and a value to associate with it.
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   Returns an unspecified value if successful.
///   If arguments are invalid or definition fails, raises an error instead of returning.
fn szlDefineFunc(ctx: Procedure.Context) Val {
    const args = ctx.localStack();
    if (args.len != 2) {
        Instruction.raiseWithError(ctx.vm, Val.init({}));
        return Val.init({});
    }
    const symbol = ctx.vm.fromVal(Symbol.Interned, args[0]) catch {
        Instruction.raiseWithError(ctx.vm, Val.init({}));
        return Val.init({});
    };
    const val = args[1];
    ctx.vm.builder().define(symbol, val) catch {
        Instruction.raiseWithError(ctx.vm, Val.init({}));
        return Val.init({});
    };
    return Val.init({});
}

/// Generic comparison function that applies a predicate to consecutive pairs of numeric arguments.
/// Returns #t if all consecutive pairs satisfy the predicate, #f otherwise.
/// With 0 or 1 arguments, returns #t (vacuously true).
/// Expects all arguments to be numeric values (either i64 or f64).
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///   predicate: Function that takes two f64 values and returns true if the comparison holds.
///
/// Returns:
///   A Val containing #t if all comparisons hold, #f otherwise.
///   If any argument is not numeric, raises an error instead of returning.
fn compareFunc(ctx: Procedure.Context, predicate: *const fn (f64, f64) bool) Val {
    const args = ctx.localStack();

    // 0 or 1 arguments: vacuously true
    if (args.len <= 1) return Val.init(true);

    for (0..args.len - 1) |i| {
        const curr_val: f64 = switch (args[i].repr) {
            .i64 => |val| @as(f64, @floatFromInt(val)),
            .f64 => |val| val,
            else => {
                Instruction.raiseWithError(ctx.vm, Val.init({}));
                return Val.init({});
            },
        };

        const next_val: f64 = switch (args[i + 1].repr) {
            .i64 => |val| @as(f64, @floatFromInt(val)),
            .f64 => |val| val,
            else => {
                Instruction.raiseWithError(ctx.vm, Val.init({}));
                return Val.init({});
            },
        };

        if (!predicate(curr_val, next_val)) {
            return Val.init(false);
        }
    }

    return Val.init(true);
}

/// Predicate function for less-than comparison.
fn isLessThan(a: f64, b: f64) bool {
    return a < b;
}

/// Predicate function for greater-than comparison.
fn isGreaterThan(a: f64, b: f64) bool {
    return a > b;
}

/// Predicate function for less-than-or-equal comparison.
fn isLessThanOrEqual(a: f64, b: f64) bool {
    return a <= b;
}

/// Predicate function for greater-than-or-equal comparison.
fn isGreaterThanOrEqual(a: f64, b: f64) bool {
    return a >= b;
}

/// Predicate function for equality comparison.
fn isEqual(a: f64, b: f64) bool {
    return a == b;
}

/// Native implementation of the '<' comparison operator.
/// Compares numeric arguments in order, returning #t if each argument is
/// strictly less than the next one, #f otherwise.
/// With 0 or 1 arguments, returns #t (vacuously true).
/// Expects all arguments to be numeric values (either i64 or f64).
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   A Val containing #t if the comparison holds, #f otherwise.
///   If any argument is not numeric, raises an error instead of returning.
fn lessThanFunc(ctx: Procedure.Context) Val {
    return compareFunc(ctx, isLessThan);
}

/// Native implementation of the '>' comparison operator.
/// Compares numeric arguments in order, returning #t if each argument is
/// strictly greater than the next one, #f otherwise.
/// With 0 or 1 arguments, returns #t (vacuously true).
/// Expects all arguments to be numeric values (either i64 or f64).
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   A Val containing #t if the comparison holds, #f otherwise.
///   If any argument is not numeric, raises an error instead of returning.
fn greaterThanFunc(ctx: Procedure.Context) Val {
    return compareFunc(ctx, isGreaterThan);
}

/// Native implementation of the '<=' comparison operator.
/// Compares numeric arguments in order, returning #t if each argument is
/// less than or equal to the next one, #f otherwise.
/// With 0 or 1 arguments, returns #t (vacuously true).
/// Expects all arguments to be numeric values (either i64 or f64).
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   A Val containing #t if the comparison holds, #f otherwise.
///   If any argument is not numeric, raises an error instead of returning.
fn lessThanOrEqualFunc(ctx: Procedure.Context) Val {
    return compareFunc(ctx, isLessThanOrEqual);
}

/// Native implementation of the '>=' comparison operator.
/// Compares numeric arguments in order, returning #t if each argument is
/// greater than or equal to the next one, #f otherwise.
/// With 0 or 1 arguments, returns #t (vacuously true).
/// Expects all arguments to be numeric values (either i64 or f64).
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   A Val containing #t if the comparison holds, #f otherwise.
///   If any argument is not numeric, raises an error instead of returning.
fn greaterThanOrEqualFunc(ctx: Procedure.Context) Val {
    return compareFunc(ctx, isGreaterThanOrEqual);
}

/// Native implementation of the '=' equality operator.
/// Compares numeric arguments in order, returning #t if all arguments are
/// equal to each other, #f otherwise.
/// With 0 or 1 arguments, returns #t (vacuously true).
/// Expects all arguments to be numeric values (either i64 or f64).
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   A Val containing #t if all arguments are equal, #f otherwise.
///   If any argument is not numeric, raises an error instead of returning.
fn equalFunc(ctx: Procedure.Context) Val {
    return compareFunc(ctx, isEqual);
}

test "+ with no arguments returns 0" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(0),
        try vm.evalStr("(+)"),
    );
}

test "+ with single argument returns argument" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(42),
        try vm.evalStr("(+ 42)"),
    );
}

test "+ with two arguments returns sum" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(7),
        try vm.evalStr("(+ 3 4)"),
    );
}

test "+ with multiple arguments sums all" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(15),
        try vm.evalStr("(+ 1 2 3 4 5)"),
    );
}

test "+ with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(-2),
        try vm.evalStr("(+ -5 3)"),
    );
}

test "+ with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(+ #t)"),
    );
}

test "+ with floating point numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(3.14),
        try vm.evalStr("(+ 3.14)"),
    );
}

test "+ with mixed integer and float returns float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.evalStr("(+ 4 3.14)");
    const expected = 7.14;
    const actual = vm.fromVal(f64, result) catch unreachable;
    try testing.expectApproxEqRel(expected, actual, 1e-10);
}

test "+ with multiple floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(6.28),
        try vm.evalStr("(+ 3.14 3.14)"),
    );
}

test "- with no arguments returns 0" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(0),
        try vm.evalStr("(-)"),
    );
}

test "- with single argument returns negation" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(-42),
        try vm.evalStr("(- 42)"),
    );
}

test "- with two arguments returns difference" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(1),
        try vm.evalStr("(- 5 4)"),
    );
}

test "- with multiple arguments subtracts all from first" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(-5),
        try vm.evalStr("(- 10 1 2 3 4 5)"),
    );
}

test "- with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(-8),
        try vm.evalStr("(- -5 3)"),
    );
}

test "- with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(- #t)"),
    );
}

test "- with floating point numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(-3.14),
        try vm.evalStr("(- 3.14)"),
    );
}

test "- with mixed integer and float returns float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.evalStr("(- 7 3.14)");
    const expected = 3.86;
    const actual = vm.fromVal(f64, result) catch unreachable;
    try testing.expectApproxEqRel(expected, actual, 1e-10);
}

test "- with multiple floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(0.0),
        try vm.evalStr("(- 3.14 3.14)"),
    );
}

test "* with no arguments returns 1" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(1),
        try vm.evalStr("(*)"),
    );
}

test "* with single argument returns argument" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(42),
        try vm.evalStr("(* 42)"),
    );
}

test "* with two arguments returns product" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(12),
        try vm.evalStr("(* 3 4)"),
    );
}

test "* with multiple arguments multiplies all" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(120),
        try vm.evalStr("(* 1 2 3 4 5)"),
    );
}

test "* with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(-15),
        try vm.evalStr("(* -5 3)"),
    );
}

test "* with zero returns zero" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(0),
        try vm.evalStr("(* 5 0 3)"),
    );
}

test "* with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(* #t)"),
    );
}

test "* with floating point numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(3.14),
        try vm.evalStr("(* 3.14)"),
    );
}

test "* with mixed integer and float returns float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.evalStr("(* 4 3.14)");
    const expected = 12.56;
    const actual = vm.fromVal(f64, result) catch unreachable;
    try testing.expectApproxEqRel(expected, actual, 1e-10);
}

test "* with multiple floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.evalStr("(* 3.14 2.0)");
    const expected = 6.28;
    const actual = vm.fromVal(f64, result) catch unreachable;
    try testing.expectApproxEqRel(expected, actual, 1e-10);
}

test "/ with no arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(/)"),
    );
}

test "/ with single argument returns reciprocal" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.evalStr("(/ 4)");
    const expected = 0.25;
    const actual = vm.fromVal(f64, result) catch unreachable;
    try testing.expectApproxEqRel(expected, actual, 1e-10);
}

test "/ with two arguments returns quotient" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.evalStr("(/ 12 4)");
    const expected = 3.0;
    const actual = vm.fromVal(f64, result) catch unreachable;
    try testing.expectApproxEqRel(expected, actual, 1e-10);
}

test "/ with multiple arguments divides sequentially" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.evalStr("(/ 120 2 3 4)");
    const expected = 5.0;
    const actual = vm.fromVal(f64, result) catch unreachable;
    try testing.expectApproxEqRel(expected, actual, 1e-10);
}

test "/ with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.evalStr("(/ -15 3)");
    const expected = -5.0;
    const actual = vm.fromVal(f64, result) catch unreachable;
    try testing.expectApproxEqRel(expected, actual, 1e-10);
}

test "/ with division by zero is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(/ 5 0)"),
    );
}

test "/ with reciprocal of zero is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(/ 0)"),
    );
}

test "/ with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(/ #t)"),
    );
}

test "/ with floating point numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.evalStr("(/ 3.14 2.0)");
    const expected = 1.57;
    const actual = vm.fromVal(f64, result) catch unreachable;
    try testing.expectApproxEqRel(expected, actual, 1e-10);
}

test "/ with mixed integer and float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.evalStr("(/ 10 4.0)");
    const expected = 2.5;
    const actual = vm.fromVal(f64, result) catch unreachable;
    try testing.expectApproxEqRel(expected, actual, 1e-10);
}

test "/ always returns float even for integer division" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.evalStr("(/ 10 5)");
    const expected = 2.0;
    const actual = vm.fromVal(f64, result) catch unreachable;
    try testing.expectApproxEqRel(expected, actual, 1e-10);
}

test "< with no arguments returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(<)"),
    );
}

test "< with single argument returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(< 42)"),
    );
}

test "< with two arguments - true case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(< 3 4)"),
    );
}

test "< with two arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(false),
        try vm.evalStr("(< 4 3)"),
    );
}

test "< with equal arguments returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(false),
        try vm.evalStr("(< 5 5)"),
    );
}

test "< with multiple arguments - all true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(< 1 2 3 4 5)"),
    );
}

test "< with multiple arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(false),
        try vm.evalStr("(< 1 2 2 4 5)"),
    );
}

test "< with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(< -5 -3 0 2)"),
    );
}

test "< with floating point numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(< 1.5 2.7 3.14)"),
    );
}

test "< with mixed integer and float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(< 3 3.14 4)"),
    );
}

test "< with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(< #t 5)"),
    );
}

test "> with no arguments returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(>)"),
    );
}

test "> with single argument returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(> 42)"),
    );
}

test "> with two arguments - true case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(> 4 3)"),
    );
}

test "> with two arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(false),
        try vm.evalStr("(> 3 4)"),
    );
}

test "> with equal arguments returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(false),
        try vm.evalStr("(> 5 5)"),
    );
}

test "> with multiple arguments - all true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(> 5 4 3 2 1)"),
    );
}

test "> with multiple arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(false),
        try vm.evalStr("(> 5 4 4 2 1)"),
    );
}

test "> with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(> 2 0 -3 -5)"),
    );
}

test "> with floating point numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(> 3.14 2.7 1.5)"),
    );
}

test "> with mixed integer and float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(> 4 3.14 3)"),
    );
}

test "> with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(> 5 #t)"),
    );
}

test "<= with no arguments returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(<=)"),
    );
}

test "<= with single argument returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(<= 42)"),
    );
}

test "<= with two arguments - true case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(<= 3 4)"),
    );
}

test "<= with two arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(false),
        try vm.evalStr("(<= 4 3)"),
    );
}

test "<= with equal arguments returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(<= 5 5)"),
    );
}

test "<= with multiple arguments - all true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(<= 1 2 2 3 5)"),
    );
}

test "<= with multiple arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(false),
        try vm.evalStr("(<= 1 2 3 2 5)"),
    );
}

test "<= with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(<= -5 -3 0 2)"),
    );
}

test "<= with floating point numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(<= 1.5 2.7 3.14)"),
    );
}

test "<= with mixed integer and float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(<= 3 3.14 4)"),
    );
}

test "<= with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(<= #t 5)"),
    );
}

test ">= with no arguments returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(>=)"),
    );
}

test ">= with single argument returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(>= 42)"),
    );
}

test ">= with two arguments - true case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(>= 4 3)"),
    );
}

test ">= with two arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(false),
        try vm.evalStr("(>= 3 4)"),
    );
}

test ">= with equal arguments returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(>= 5 5)"),
    );
}

test ">= with multiple arguments - all true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(>= 5 4 4 3 1)"),
    );
}

test ">= with multiple arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(false),
        try vm.evalStr("(>= 5 4 3 4 1)"),
    );
}

test ">= with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(>= 2 0 -3 -5)"),
    );
}

test ">= with floating point numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(>= 3.14 2.7 1.5)"),
    );
}

test ">= with mixed integer and float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(>= 4 3.14 3)"),
    );
}

test ">= with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(>= 5 #t)"),
    );
}

test "= with no arguments returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(=)"),
    );
}

test "= with single argument returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(= 42)"),
    );
}

test "= with two equal arguments returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(= 5 5)"),
    );
}

test "= with two unequal arguments returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(false),
        try vm.evalStr("(= 3 4)"),
    );
}

test "= with multiple equal arguments returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(= 5 5 5 5 5)"),
    );
}

test "= with multiple arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(false),
        try vm.evalStr("(= 5 5 5 4 5)"),
    );
}

test "= with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(= -5 -5 -5)"),
    );
}

test "= with floating point numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(= 3.14 3.14 3.14)"),
    );
}

test "= with mixed integer and float - equal values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(= 3 3.0)"),
    );
}

test "= with mixed integer and float - unequal values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(false),
        try vm.evalStr("(= 3 3.14)"),
    );
}

test "= with zero values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(true),
        try vm.evalStr("(= 0 0.0 0)"),
    );
}

test "= with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(= #t 5)"),
    );
}
