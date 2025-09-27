//! Equivalence operations for the Scheme interpreter.
//!
//! This module provides native implementations of equivalence predicates
//! including eq? which tests for object identity.

const std = @import("std");
const testing = std.testing;

const instruction = @import("../instruction.zig");
const Instruction = instruction.Instruction;
const Proc = @import("../Proc.zig");
const NativeProc = @import("../NativeProc.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

/// Native implementation of the `eq?` equivalence predicate.
///
/// Tests for object identity - returns #t if both arguments refer to the same object.
/// For immediate values (nil, booleans, numbers, characters), this is value equality.
/// For heap objects (symbols, pairs, procedures), this tests reference equality.
///
/// Args:
///   arg1: The first value to compare
///   arg2: The second value to compare
///
/// Returns:
///   #t if the arguments are identical, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
const eq_native = NativeProc.Native{
    .name = "eq?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }
            return Val.init(args[0].eq(args[1]));
        }
    }.func,
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
        Vm.Error.UncaughtException,
        vm.evalStr("(eq?)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(eq? 1)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
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
