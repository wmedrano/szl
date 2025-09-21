//! Arithmetic operations for the Scheme interpreter.
//!
//! This module provides native implementations of basic arithmetic operations
//! including addition, subtraction, multiplication, and division.

const std = @import("std");
const testing = std.testing;

const Instruction = @import("../Instruction.zig");
const Procedure = @import("../Procedure.zig");
const Symbol = @import("../Symbol.zig");
const Val = @import("../Val.zig");
const Vm = @import("../Vm.zig");

/// Registers all arithmetic functions with the virtual machine.
///
/// Args:
///   vm: Pointer to the VM instance to register arithmetic functions with.
///
/// Errors:
///   May return allocation errors if registering functions fails.
pub fn register(vm: *Vm) !void {
    try vm.builder().define(
        Symbol.init("+"),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init("+")),
            .implementation = .{ .native = .{ .func = addFunc } },
        }),
    );
    try vm.builder().define(
        Symbol.init("-"),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init("-")),
            .implementation = .{ .native = .{ .func = subFunc } },
        }),
    );
    try vm.builder().define(
        Symbol.init("*"),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init("*")),
            .implementation = .{ .native = .{ .func = mulFunc } },
        }),
    );
    try vm.builder().define(
        Symbol.init("/"),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init("/")),
            .implementation = .{ .native = .{ .func = divFunc } },
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

test "+ with no arguments returns 0" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0", "(+)");
}

test "+ with single argument returns argument" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("42", "(+ 42)");
}

test "+ with two arguments returns sum" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("7", "(+ 3 4)");
}

test "+ with multiple arguments sums all" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("15", "(+ 1 2 3 4 5)");
}

test "+ with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("-2", "(+ -5 3)");
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

    try vm.expectEval("3.14", "(+ 3.14)");
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

    try vm.expectEval("6.28", "(+ 3.14 3.14)");
}

test "- with no arguments returns 0" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0", "(-)");
}

test "- with single argument returns negation" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("-42", "(- 42)");
}

test "- with two arguments returns difference" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1", "(- 5 4)");
}

test "- with multiple arguments subtracts all from first" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("-5", "(- 10 1 2 3 4 5)");
}

test "- with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("-8", "(- -5 3)");
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

    try vm.expectEval("-3.14", "(- 3.14)");
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

    try vm.expectEval("0.0", "(- 3.14 3.14)");
}

test "* with no arguments returns 1" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1", "(*)");
}

test "* with single argument returns argument" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("42", "(* 42)");
}

test "* with two arguments returns product" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("12", "(* 3 4)");
}

test "* with multiple arguments multiplies all" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("120", "(* 1 2 3 4 5)");
}

test "* with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("-15", "(* -5 3)");
}

test "* with zero returns zero" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0", "(* 5 0 3)");
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

    try vm.expectEval("3.14", "(* 3.14)");
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
