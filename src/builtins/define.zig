//! Variable definition functions for the Scheme interpreter.
//!
//! This module provides native implementations for defining global variables
//! and other binding operations in the Scheme environment.

const std = @import("std");
const testing = std.testing;

const instruction = @import("../instruction.zig");
const Instruction = instruction.Instruction;
const Procedure = @import("../Procedure.zig");
const Symbol = @import("../Symbol.zig");
const Val = @import("../Val.zig");
const Vm = @import("../Vm.zig");

/// Registers all definition functions with the virtual machine.
///
/// Args:
///   vm: Pointer to the VM instance to register definition functions with.
///
/// Errors:
///   May return allocation errors if registering functions fails.
pub fn register(vm: *Vm) !void {
    try vm.builder().define(
        Symbol.init("szl-define"),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init("szl-define")),
            .implementation = .{ .native = .{ .func = szlDefineFunc } },
        }),
    );
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
        instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
        return Val.init(ctx.vm.common_symbols.@"*unspecified*");
    }
    const symbol = ctx.vm.fromVal(Symbol.Interned, args[0]) catch {
        instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
        return Val.init(ctx.vm.common_symbols.@"*unspecified*");
    };
    const val = args[1];
    ctx.vm.builder().define(symbol, val) catch {
        instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
        return Val.init(ctx.vm.common_symbols.@"*unspecified*");
    };
    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
}

test "szl-define defines global variable successfully" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("*unspecified*", "(szl-define 'foo 42)");
    try testing.expectEqual(
        Val.init(42),
        vm.inspector().get(Symbol.init("foo")),
    );
}

test "szl-define with wrong number of arguments raises error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.SzlError,
        vm.evalStr("(szl-define)"),
    );
    try testing.expectError(
        error.SzlError,
        vm.evalStr("(szl-define 'foo)"),
    );
    try testing.expectError(
        error.SzlError,
        vm.evalStr("(szl-define 'foo 42 'extra)"),
    );
}

test "szl-define with invalid symbol type raises error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test with non-symbol first argument (number)
    try testing.expectError(
        error.SzlError,
        vm.evalStr("(szl-define 42 'value)"),
    );

    // Test with non-symbol first argument (boolean)
    try testing.expectError(
        error.SzlError,
        vm.evalStr("(szl-define #t 'value)"),
    );

    // Test with non-symbol first argument (list expression)
    try testing.expectError(
        error.SzlError,
        vm.evalStr("(szl-define '(+ 1 2) 'value)"),
    );
}

test "szl-define can redefine existing variable with new value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Define initial value
    try vm.expectEval("*unspecified*", "(szl-define 'x 100)");
    try testing.expectEqual(
        Val.init(100),
        vm.inspector().get(Symbol.init("x")),
    );

    // Redefine with new value
    try vm.expectEval("*unspecified*", "(szl-define 'x 200)");
    try testing.expectEqual(
        Val.init(200),
        vm.inspector().get(Symbol.init("x")),
    );
}

test "szl-define can redefine variable with different type" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Define as integer
    try vm.expectEval("*unspecified*", "(szl-define 'var 42)");
    try testing.expectEqual(
        Val.init(42),
        vm.inspector().get(Symbol.init("var")),
    );

    // Redefine as boolean
    try vm.expectEval("*unspecified*", "(szl-define 'var #t)");
    try testing.expectEqual(
        Val.init(true),
        vm.inspector().get(Symbol.init("var")),
    );

    // Redefine as different integer
    try vm.expectEval("*unspecified*", "(szl-define 'var -5)");
    try testing.expectEqual(
        Val.init(-5),
        vm.inspector().get(Symbol.init("var")),
    );
}

test "szl-define multiple redefinitions preserve only latest value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Define initial value
    try vm.expectEval("*unspecified*", "(szl-define 'counter 1)");

    // Multiple redefinitions
    try vm.expectEval("*unspecified*", "(szl-define 'counter 2)");
    try vm.expectEval("*unspecified*", "(szl-define 'counter 3)");
    try vm.expectEval("*unspecified*", "(szl-define 'counter 4)");

    // Only the latest value should be preserved
    try testing.expectEqual(
        Val.init(4),
        vm.inspector().get(Symbol.init("counter")),
    );
}
