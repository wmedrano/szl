//! Pair operations for the Scheme interpreter.
//!
//! This module provides native implementations of pair operations
//! including type checking (pair?), construction (cons), and accessors (car, cdr).

const std = @import("std");
const testing = std.testing;

const instruction = @import("../instruction.zig");
const Instruction = instruction.Instruction;
const Procedure = @import("../Procedure.zig");
const Pair = @import("../types/Pair.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

/// Native implementation of the `pair?` type predicate.
///
/// Determines whether the given argument is a pair (cons cell) or not.
/// This is the standard Scheme predicate for testing pair types.
///
/// Args:
///   arg: The value to test for being a pair
///
/// Returns:
///   `#t` if the argument is a pair, `#f` otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
const pair_predicate_native = Procedure.Native{
    .name = "pair?",
    .func = struct {
        fn func(ctx: Procedure.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            switch (args[0].repr) {
                .pair => return Val.init(true),
                else => return Val.init(false),
            }
        }
    }.func,
};

/// Native implementation of the `cons` constructor.
///
/// Creates a new pair (cons cell) with the given car and cdr values.
/// This is the fundamental list constructor in Scheme.
///
/// Args:
///   car: The first element of the pair
///   cdr: The second element of the pair
///
/// Returns:
///   A new pair containing the car and cdr values
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
///   - `invalid-argument`: When pair construction fails (allocation error)
const cons_native = Procedure.Native{
    .name = "cons",
    .func = struct {
        fn func(ctx: Procedure.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            const pair = Pair.init(args[0], args[1]);
            return ctx.vm.builder().build(pair) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init({});
            };
        }
    }.func,
};

/// Native implementation of the `car` accessor.
///
/// Returns the first element (car) of a pair. This is one of the fundamental
/// pair accessors in Scheme, used to extract the head of a list.
///
/// Args:
///   pair: A pair value to extract the car from
///
/// Returns:
///   The car (first element) of the pair
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a pair or pair resolution fails
const car_native = Procedure.Native{
    .name = "car",
    .func = struct {
        fn func(ctx: Procedure.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            switch (args[0].repr) {
                .pair => |handle| {
                    const pair = ctx.vm.inspector().resolve(Pair, handle) catch {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    };
                    return pair.car;
                },
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init({});
                },
            }
        }
    }.func,
};

/// Native implementation of the `cdr` accessor.
///
/// Returns the second element (cdr) of a pair. This is one of the fundamental
/// pair accessors in Scheme, used to extract the tail of a list.
///
/// Args:
///   pair: A pair value to extract the cdr from
///
/// Returns:
///   The cdr (second element) of the pair
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a pair or pair resolution fails
const cdr_native = Procedure.Native{
    .name = "cdr",
    .func = struct {
        fn func(ctx: Procedure.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            switch (args[0].repr) {
                .pair => |handle| {
                    const pair = ctx.vm.inspector().resolve(Pair, handle) catch {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    };
                    return pair.cdr;
                },
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init({});
                },
            }
        }
    }.func,
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
        Vm.Error.UncaughtException,
        vm.evalStr("(pair?)"),
    );
}

test "pair? with multiple arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
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
        Vm.Error.UncaughtException,
        vm.evalStr("(cons)"),
    );
}

test "cons with one argument is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(cons 1)"),
    );
}

test "cons with three arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
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
        Vm.Error.UncaughtException,
        vm.evalStr("(car '())"),
    );
}

test "car with non-pair is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(car 42)"),
    );
}

test "car with no arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(car)"),
    );
}

test "car with multiple arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
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
        Vm.Error.UncaughtException,
        vm.evalStr("(cdr '())"),
    );
}

test "cdr with non-pair is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(cdr 42)"),
    );
}

test "cdr with no arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(cdr)"),
    );
}

test "cdr with multiple arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(cdr (cons 1 2) (cons 3 4))"),
    );
}
