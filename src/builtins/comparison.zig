//! Comparison operations for the Scheme interpreter.
//!
//! This module provides native implementations of comparison operations
//! including less than, greater than, equality, and their variants.

const std = @import("std");
const testing = std.testing;

const Instruction = @import("../Instruction.zig");
const Procedure = @import("../Procedure.zig");
const Symbol = @import("../Symbol.zig");
const Val = @import("../Val.zig");
const Vm = @import("../Vm.zig");

/// Registers all comparison functions with the virtual machine.
///
/// Args:
///   vm: Pointer to the VM instance to register comparison functions with.
///
/// Errors:
///   May return allocation errors if registering functions fails.
pub fn register(vm: *Vm) !void {
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
