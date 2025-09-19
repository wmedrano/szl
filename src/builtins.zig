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
