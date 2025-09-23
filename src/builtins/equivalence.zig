//! Equivalence operations for the Scheme interpreter.
//!
//! This module provides native implementations of equivalence predicates
//! including eq? which tests for object identity.

const std = @import("std");
const testing = std.testing;

const instruction = @import("../instruction.zig");
const Instruction = instruction.Instruction;
const Procedure = @import("../Procedure.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

const eq_native = Procedure.Native{
    .name = "eq?",
    .func = eqFunc,
};

/// Registers all equivalence functions with the virtual machine.
///
/// Args:
///   vm: Pointer to the VM instance to register equivalence functions with.
///
/// Errors:
///   May return allocation errors if registering functions fails.
pub fn register(vm: *Vm) !void {
    try vm.builder().defineNativeProc(&eq_native);
}

/// Native implementation of the 'eq?' equivalence predicate.
/// Tests for object identity - returns #t if both arguments refer to the same object.
/// For immediate values (nil, booleans, numbers, characters), this is value equality.
/// For heap objects (symbols, pairs, procedures), this tests reference equality.
/// Expects exactly two arguments.
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   A Val containing #t if the arguments are identical, #f otherwise.
///   If the wrong number of arguments is provided, raises an error instead of returning.
fn eqFunc(ctx: Procedure.Context) Val {
    const args = ctx.localStack();
    if (args.len != 2) {
        instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
        return Val.init(ctx.vm.common_symbols.@"*unspecified*");
    }
    return Val.init(args[0].eq(args[1]));
}

test "eq? with same immediate values returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(eq? 42 42)");
    try vm.expectEval("#t", "(eq? #t #t)");
    try vm.expectEval("#t", "(eq? #f #f)");
    try vm.expectEval("#t", "(eq? #\\a #\\a)");
}

test "eq? with different immediate values returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(eq? 42 43)");
    try vm.expectEval("#f", "(eq? #t #f)");
    try vm.expectEval("#f", "(eq? #\\a #\\b)");
    try vm.expectEval("#f", "(eq? 42 #t)");
}

test "eq? with nil returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(eq? '() '())");
}

test "eq? with same symbols returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(eq? 'hello 'hello)");
}

test "eq? with different symbols returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(eq? 'hello 'world)");
}

test "eq? with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(eq?)"),
    );

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(eq? 1)"),
    );

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(eq? 1 2 3)"),
    );
}

test "eq? with mixed types returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(eq? 42 'foo)");
    try vm.expectEval("#f", "(eq? #t 1)");
    try vm.expectEval("#f", "(eq? '() #f)");
}
