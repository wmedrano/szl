//! Boolean operations for the Scheme interpreter.
//!
//! This module provides native implementations of boolean operations including
//! type checking (boolean?), logical negation (not), and boolean equality (boolean=?).

const std = @import("std");
const testing = std.testing;

const instruction = @import("../instruction.zig");
const Instruction = instruction.Instruction;
const Procedure = @import("../Procedure.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

/// Native implementation of the `boolean?` type predicate procedure.
///
/// Checks if the given argument is a boolean value (#t or #f). Returns #t if the
/// argument is boolean, #f otherwise. This is a standard Scheme type predicate.
///
/// Args:
///   value: Any Scheme value to test for boolean type
///
/// Returns:
///   #t if the argument is a boolean value, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
const boolean_predicate_native = Procedure.Native{
    .name = "boolean?",
    .func = struct {
        fn func(ctx: Procedure.Context) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            switch (args[0].repr) {
                .boolean => return Val.init(true),
                else => return Val.init(false),
            }
        }
    }.func,
};

/// Native implementation of the `not` logical negation procedure.
///
/// Returns the logical negation of the given argument. In Scheme, only #f is
/// considered false; all other values (including 0, empty lists, etc.) are truthy.
/// Therefore, `not` returns #t only when given #f, and returns #f for all other values.
///
/// Args:
///   value: Any Scheme value to logically negate
///
/// Returns:
///   #t if the argument is #f, #f for all other values
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
const not_native = Procedure.Native{
    .name = "not",
    .func = struct {
        fn func(ctx: Procedure.Context) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            switch (args[0].repr) {
                .boolean => |val| return Val.init(!val),
                else => return Val.init(false), // All non-#f values are truthy, so not returns #f
            }
        }
    }.func,
};

/// Native implementation of the `boolean=?` equality comparison procedure.
///
/// Tests whether all arguments are boolean values and are equal to each other.
/// Returns #t if all arguments are the same boolean value, #f if they differ.
/// With 0 arguments, returns #t (vacuously true). With 1 argument, returns #t
/// if it's a boolean (type check), otherwise raises a type error.
///
/// Args:
///   values...: Zero or more values to compare (all must be booleans)
///
/// Returns:
///   #t if all arguments are equal boolean values, #f if they differ
///
/// Errors:
///   - `type-error`: When any argument is not a boolean value
const boolean_equal_native = Procedure.Native{
    .name = "boolean=?",
    .func = struct {
        fn func(ctx: Procedure.Context) Vm.Error!Val {
            const args = ctx.localStack();

            // 0 arguments: vacuously true
            if (args.len == 0) return Val.init(true);

            // Get the first boolean value to compare against
            const first_bool: bool = switch (args[0].repr) {
                .boolean => |val| val,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init({});
                },
            };

            // Check that all remaining arguments are booleans and equal to the first
            for (args[1..]) |arg| {
                const curr_bool: bool = switch (arg.repr) {
                    .boolean => |val| val,
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    },
                };

                if (curr_bool != first_bool) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Registers all boolean functions with the virtual machine.
///
/// Args:
///   vm: Pointer to the VM instance to register boolean functions with.
///
/// Errors:
///   May return allocation errors if registering functions fails.
pub fn register(vm: *Vm) !void {
    try vm.builder().defineNativeProc(&boolean_predicate_native);
    try vm.builder().defineNativeProc(&not_native);
    try vm.builder().defineNativeProc(&boolean_equal_native);
}


////////////////////////////////////////////////////////////////////////////////
// Boolean predicate tests
////////////////////////////////////////////////////////////////////////////////

test "boolean? with boolean true returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(boolean? #t)");
}

test "boolean? with boolean false returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(boolean? #f)");
}

test "boolean? with integer returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(boolean? 42)");
}

test "boolean? with symbol returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(boolean? 'hello)");
}

test "boolean? with nil returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(boolean? '())");
}

test "boolean? with no arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(boolean?)"),
    );
}

test "boolean? with multiple arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(boolean? #t #f)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// Logical negation tests
////////////////////////////////////////////////////////////////////////////////

test "not with #f returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(not #f)");
}

test "not with #t returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(not #t)");
}

test "not with integer returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(not 42)");
}

test "not with zero returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(not 0)");
}

test "not with symbol returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(not 'hello)");
}

test "not with nil returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(not '())");
}

test "not with no arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(not)"),
    );
}

test "not with multiple arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(not #t #f)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// Boolean equality tests
////////////////////////////////////////////////////////////////////////////////

test "boolean=? with no arguments returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(boolean=?)");
}

test "boolean=? with one boolean argument returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(boolean=? #t)");
    try vm.expectEval("#t", "(boolean=? #f)");
}

test "boolean=? with one non-boolean argument is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(boolean=? 42)"),
    );
    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(boolean=? 'hello)"),
    );
}

test "boolean=? with all #t returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(boolean=? #t #t)");
    try vm.expectEval("#t", "(boolean=? #t #t #t)");
}

test "boolean=? with all #f returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(boolean=? #f #f)");
    try vm.expectEval("#t", "(boolean=? #f #f #f)");
}

test "boolean=? with mixed booleans returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(boolean=? #t #f)");
    try vm.expectEval("#f", "(boolean=? #f #t)");
    try vm.expectEval("#f", "(boolean=? #t #t #f)");
}

test "boolean=? with non-boolean argument is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(boolean=? #t 42)"),
    );
    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(boolean=? 42 #t)"),
    );
}
