//! Mathematical functions for the Scheme interpreter.
//!
//! This module provides native implementations of mathematical functions
//! such as absolute value and other numeric operations.

const std = @import("std");
const testing = std.testing;

const Instruction = @import("../Instruction.zig");
const Procedure = @import("../Procedure.zig");
const Symbol = @import("../Symbol.zig");
const Val = @import("../Val.zig");
const Vm = @import("../Vm.zig");

/// Registers all mathematical functions with the virtual machine.
///
/// Args:
///   vm: Pointer to the VM instance to register mathematical functions with.
///
/// Errors:
///   May return allocation errors if registering functions fails.
pub fn register(vm: *Vm) !void {
    try vm.builder().define(
        Symbol.init("abs"),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init("abs")),
            .implementation = Procedure.initNative(absFunc),
        }),
    );
}

/// Native implementation of the 'abs' absolute value function.
/// Returns the absolute value of a single numeric argument.
/// Expects exactly one argument that must be a numeric value (either i64 or f64).
/// Returns the same type as the input (i64 for i64 input, f64 for f64 input).
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   A Val containing the absolute value of the input argument.
///   If the wrong number of arguments or non-numeric argument is provided, raises an error instead of returning.
fn absFunc(ctx: Procedure.Context) Val {
    const args = ctx.localStack();
    if (args.len != 1) {
        Instruction.raiseWithError(ctx.vm, Val.init({}));
        return Val.init({});
    }

    switch (args[0].repr) {
        .i64 => |val| {
            if (val < 0) {
                return Val.init(-val);
            } else {
                return Val.init(val);
            }
        },
        .f64 => |val| {
            if (val < 0.0) {
                return Val.init(-val);
            } else {
                return Val.init(val);
            }
        },
        else => {
            Instruction.raiseWithError(ctx.vm, Val.init({}));
            return Val.init({});
        },
    }
}

test "abs with positive integer returns same value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("42", "(abs 42)");
}

test "abs with negative integer returns positive value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("42", "(abs -42)");
}

test "abs with zero returns zero" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0", "(abs 0)");
}

test "abs with positive float returns same value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3.14", "(abs 3.14)");
}

test "abs with negative float returns positive value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3.14", "(abs -3.14)");
}

test "abs with zero float returns zero" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0.0", "(abs 0.0)");
}

test "abs with no arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(abs)"),
    );
}

test "abs with multiple arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(abs 5 10)"),
    );
}

test "abs with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(abs #t)"),
    );
}
