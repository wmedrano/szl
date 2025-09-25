//! Number operations for the Scheme interpreter.
//!
//! This module provides native implementations of all numeric operations
//! including arithmetic (+, -, *, /), comparisons (<, >, =, etc.), and
//! mathematical functions (abs, etc.).

const std = @import("std");
const testing = std.testing;

const instruction = @import("../instruction.zig");
const Instruction = instruction.Instruction;
const Procedure = @import("../Procedure.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

/// Native implementation of the '+' arithmetic operator.
///
/// Performs addition on zero or more numeric arguments. With no arguments, returns 0.
/// With multiple arguments, returns the sum of all arguments.
/// Returns f64 if any operand is f64, otherwise returns i64.
///
/// Args:
///   Any number of numeric arguments (i64 or f64)
///
/// Returns:
///   The sum of all numeric arguments as Val(i64) or Val(f64)
///
/// Errors:
///   - `type-error`: When any argument is not a numeric value
const add_native = Procedure.Native{
    .name = "+",
    .func = struct {
        fn func(ctx: Procedure.Context) Vm.Error!Val {
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
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
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
    }.func,
};

/// Native implementation of the '-' arithmetic operator.
///
/// Performs subtraction on zero or more numeric arguments. With no arguments, returns 0.
/// With one argument, returns its negation. With multiple arguments, subtracts all
/// subsequent arguments from the first.
/// Returns f64 if any operand is f64, otherwise returns i64.
///
/// Args:
///   Any number of numeric arguments (i64 or f64)
///
/// Returns:
///   The result of subtraction as Val(i64) or Val(f64)
///
/// Errors:
///   - `type-error`: When any argument is not a numeric value
const sub_native = Procedure.Native{
    .name = "-",
    .func = struct {
        fn func(ctx: Procedure.Context) Vm.Error!Val {
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
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
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
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
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
    }.func,
};

/// Native implementation of the '*' arithmetic operator.
///
/// Performs multiplication on zero or more numeric arguments. With no arguments, returns 1.
/// With multiple arguments, returns the product of all arguments.
/// Returns f64 if any operand is f64, otherwise returns i64.
///
/// Args:
///   Any number of numeric arguments (i64 or f64)
///
/// Returns:
///   The product of all numeric arguments as Val(i64) or Val(f64)
///
/// Errors:
///   - `type-error`: When any argument is not a numeric value
const mul_native = Procedure.Native{
    .name = "*",
    .func = struct {
        fn func(ctx: Procedure.Context) Vm.Error!Val {
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
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
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
    }.func,
};

/// Native implementation of the '/' arithmetic operator.
///
/// Performs division on one or more numeric arguments. With one argument, returns
/// its reciprocal (1.0 / argument). With multiple arguments, divides the first
/// argument by all subsequent arguments. Always returns f64 result.
///
/// Args:
///   One or more numeric arguments (i64 or f64)
///
/// Returns:
///   The result of division as Val(f64)
///
/// Errors:
///   - `wrong-number-of-arguments`: When no arguments are provided
///   - `type-error`: When any argument is not a numeric value
///   - `division-by-zero`: When division by zero is attempted
const div_native = Procedure.Native{
    .name = "/",
    .func = struct {
        fn func(ctx: Procedure.Context) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len == 0) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            // Convert first argument to f64
            var result: f64 = switch (args[0].repr) {
                .i64 => |val| @as(f64, @floatFromInt(val)),
                .f64 => |val| val,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init({});
                },
            };

            // If only one argument, return its reciprocal
            if (args.len == 1) {
                if (result == 0.0) {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"division-by-zero"));
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
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    },
                };

                if (divisor == 0.0) {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"division-by-zero"));
                    return Val.init({});
                }

                result /= divisor;
            }

            return Val.init(result);
        }
    }.func,
};

/// Native implementation of the '<' comparison operator.
///
/// Compares numeric arguments in order, returning #t if each argument is
/// strictly less than the next one, #f otherwise.
/// With 0 or 1 arguments, returns #t (vacuously true).
///
/// Args:
///   Any number of numeric arguments (i64 or f64)
///
/// Returns:
///   #t if all consecutive pairs satisfy a < b, #f otherwise
///
/// Errors:
///   - `type-error`: When any argument is not a numeric value
const less_than_native = Procedure.Native{
    .name = "<",
    .func = struct {
        fn func(ctx: Procedure.Context) Vm.Error!Val {
            const args = ctx.localStack();

            // 0 or 1 arguments: vacuously true
            if (args.len <= 1) return Val.init(true);

            for (0..args.len - 1) |i| {
                const curr_val: f64 = switch (args[i].repr) {
                    .i64 => |val| @as(f64, @floatFromInt(val)),
                    .f64 => |val| val,
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    },
                };

                const next_val: f64 = switch (args[i + 1].repr) {
                    .i64 => |val| @as(f64, @floatFromInt(val)),
                    .f64 => |val| val,
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    },
                };

                if (!(curr_val < next_val)) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the '>' comparison operator.
///
/// Compares numeric arguments in order, returning #t if each argument is
/// strictly greater than the next one, #f otherwise.
/// With 0 or 1 arguments, returns #t (vacuously true).
///
/// Args:
///   Any number of numeric arguments (i64 or f64)
///
/// Returns:
///   #t if all consecutive pairs satisfy a > b, #f otherwise
///
/// Errors:
///   - `type-error`: When any argument is not a numeric value
const greater_than_native = Procedure.Native{
    .name = ">",
    .func = struct {
        fn func(ctx: Procedure.Context) Vm.Error!Val {
            const args = ctx.localStack();

            // 0 or 1 arguments: vacuously true
            if (args.len <= 1) return Val.init(true);

            for (0..args.len - 1) |i| {
                const curr_val: f64 = switch (args[i].repr) {
                    .i64 => |val| @as(f64, @floatFromInt(val)),
                    .f64 => |val| val,
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    },
                };

                const next_val: f64 = switch (args[i + 1].repr) {
                    .i64 => |val| @as(f64, @floatFromInt(val)),
                    .f64 => |val| val,
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    },
                };

                if (!(curr_val > next_val)) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the '<=' comparison operator.
///
/// Compares numeric arguments in order, returning #t if each argument is
/// less than or equal to the next one, #f otherwise.
/// With 0 or 1 arguments, returns #t (vacuously true).
///
/// Args:
///   Any number of numeric arguments (i64 or f64)
///
/// Returns:
///   #t if all consecutive pairs satisfy a <= b, #f otherwise
///
/// Errors:
///   - `type-error`: When any argument is not a numeric value
const less_than_or_equal_native = Procedure.Native{
    .name = "<=",
    .func = struct {
        fn func(ctx: Procedure.Context) Vm.Error!Val {
            const args = ctx.localStack();

            // 0 or 1 arguments: vacuously true
            if (args.len <= 1) return Val.init(true);

            for (0..args.len - 1) |i| {
                const curr_val: f64 = switch (args[i].repr) {
                    .i64 => |val| @as(f64, @floatFromInt(val)),
                    .f64 => |val| val,
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    },
                };

                const next_val: f64 = switch (args[i + 1].repr) {
                    .i64 => |val| @as(f64, @floatFromInt(val)),
                    .f64 => |val| val,
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    },
                };

                if (!(curr_val <= next_val)) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the '>=' comparison operator.
///
/// Compares numeric arguments in order, returning #t if each argument is
/// greater than or equal to the next one, #f otherwise.
/// With 0 or 1 arguments, returns #t (vacuously true).
///
/// Args:
///   Any number of numeric arguments (i64 or f64)
///
/// Returns:
///   #t if all consecutive pairs satisfy a >= b, #f otherwise
///
/// Errors:
///   - `type-error`: When any argument is not a numeric value
const greater_than_or_equal_native = Procedure.Native{
    .name = ">=",
    .func = struct {
        fn func(ctx: Procedure.Context) Vm.Error!Val {
            const args = ctx.localStack();

            // 0 or 1 arguments: vacuously true
            if (args.len <= 1) return Val.init(true);

            for (0..args.len - 1) |i| {
                const curr_val: f64 = switch (args[i].repr) {
                    .i64 => |val| @as(f64, @floatFromInt(val)),
                    .f64 => |val| val,
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    },
                };

                const next_val: f64 = switch (args[i + 1].repr) {
                    .i64 => |val| @as(f64, @floatFromInt(val)),
                    .f64 => |val| val,
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    },
                };

                if (!(curr_val >= next_val)) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the '=' equality operator.
///
/// Compares numeric arguments for equality, returning #t if all arguments are
/// equal to each other, #f otherwise.
/// With 0 or 1 arguments, returns #t (vacuously true).
///
/// Args:
///   Any number of numeric arguments (i64 or f64)
///
/// Returns:
///   #t if all arguments are numerically equal, #f otherwise
///
/// Errors:
///   - `type-error`: When any argument is not a numeric value
const equal_native = Procedure.Native{
    .name = "=",
    .func = struct {
        fn func(ctx: Procedure.Context) Vm.Error!Val {
            const args = ctx.localStack();

            // 0 or 1 arguments: vacuously true
            if (args.len <= 1) return Val.init(true);

            for (0..args.len - 1) |i| {
                const curr_val: f64 = switch (args[i].repr) {
                    .i64 => |val| @as(f64, @floatFromInt(val)),
                    .f64 => |val| val,
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    },
                };

                const next_val: f64 = switch (args[i + 1].repr) {
                    .i64 => |val| @as(f64, @floatFromInt(val)),
                    .f64 => |val| val,
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    },
                };

                if (!(curr_val == next_val)) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the 'abs' absolute value function.
///
/// Returns the absolute value of a single numeric argument.
/// Preserves the input type (i64 input returns i64, f64 input returns f64).
///
/// Args:
///   Exactly one numeric argument (i64 or f64)
///
/// Returns:
///   The absolute value as Val(i64) or Val(f64) matching input type
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a numeric value
const abs_native = Procedure.Native{
    .name = "abs",
    .func = struct {
        fn func(ctx: Procedure.Context) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
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
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init({});
                },
            }
        }
    }.func,
};

/// Registers all number functions with the virtual machine.
///
/// Args:
///   vm: Pointer to the VM instance to register number functions with.
///
/// Errors:
///   May return allocation errors if registering functions fails.
pub fn register(vm: *Vm) !void {
    // Arithmetic operations
    try vm.builder().defineNativeProc(&add_native);
    try vm.builder().defineNativeProc(&sub_native);
    try vm.builder().defineNativeProc(&mul_native);
    try vm.builder().defineNativeProc(&div_native);

    // Comparison operations
    try vm.builder().defineNativeProc(&less_than_native);
    try vm.builder().defineNativeProc(&greater_than_native);
    try vm.builder().defineNativeProc(&less_than_or_equal_native);
    try vm.builder().defineNativeProc(&greater_than_or_equal_native);
    try vm.builder().defineNativeProc(&equal_native);

    // Mathematical functions
    try vm.builder().defineNativeProc(&abs_native);
}


////////////////////////////////////////////////////////////////////////////////
// Arithmetic operations
////////////////////////////////////////////////////////////////////////////////

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
        Vm.Error.UncaughtException,
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
        Vm.Error.UncaughtException,
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
        Vm.Error.UncaughtException,
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
        Vm.Error.UncaughtException,
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
        Vm.Error.UncaughtException,
        vm.evalStr("(/ 5 0)"),
    );
}

test "/ with reciprocal of zero is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(/ 0)"),
    );
}

test "/ with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
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

////////////////////////////////////////////////////////////////////////////////
// Comparison operators
////////////////////////////////////////////////////////////////////////////////

test "< with no arguments returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<)");
}

test "< with single argument returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(< 42)");
}

test "< with two arguments - true case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(< 3 4)");
}

test "< with two arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(< 4 3)");
}

test "< with equal arguments returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(< 5 5)");
}

test "< with multiple arguments - all true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(< 1 2 3 4 5)");
}

test "< with multiple arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(< 1 2 2 4 5)");
}

test "< with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(< -5 -3 0 2)");
}

test "< with floating point numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(< 1.5 2.7 3.14)");
}

test "< with mixed integer and float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(< 3 3.14 4)");
}

test "< with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(< #t 5)"),
    );
}

test "> with no arguments returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(>)");
}

test "> with single argument returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(> 42)");
}

test "> with two arguments - true case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(> 4 3)");
}

test "> with two arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(> 3 4)");
}

test "> with equal arguments returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(> 5 5)");
}

test "> with multiple arguments - all true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(> 5 4 3 2 1)");
}

test "> with multiple arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(> 5 4 4 2 1)");
}

test "> with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(> 2 0 -3 -5)");
}

test "> with floating point numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(> 3.14 2.7 1.5)");
}

test "> with mixed integer and float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(> 4 3.14 3)");
}

test "> with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(> 5 #t)"),
    );
}

test "<= with no arguments returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<=)");
}

test "<= with single argument returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<= 42)");
}

test "<= with two arguments - true case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<= 3 4)");
}

test "<= with two arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(<= 4 3)");
}

test "<= with equal arguments returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<= 5 5)");
}

test "<= with multiple arguments - all true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<= 1 2 2 3 5)");
}

test "<= with multiple arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(<= 1 2 3 2 5)");
}

test "<= with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<= -5 -3 0 2)");
}

test "<= with floating point numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<= 1.5 2.7 3.14)");
}

test "<= with mixed integer and float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<= 3 3.14 4)");
}

test "<= with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(<= #t 5)"),
    );
}

test ">= with no arguments returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(>=)");
}

test ">= with single argument returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(>= 42)");
}

test ">= with two arguments - true case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(>= 4 3)");
}

test ">= with two arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(>= 3 4)");
}

test ">= with equal arguments returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(>= 5 5)");
}

test ">= with multiple arguments - all true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(>= 5 4 4 3 1)");
}

test ">= with multiple arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(>= 5 4 3 4 1)");
}

test ">= with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(>= 2 0 -3 -5)");
}

test ">= with floating point numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(>= 3.14 2.7 1.5)");
}

test ">= with mixed integer and float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(>= 4 3.14 3)");
}

test ">= with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(>= 5 #t)"),
    );
}

test "= with no arguments returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(=)");
}

test "= with single argument returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(= 42)");
}

test "= with two equal arguments returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(= 5 5)");
}

test "= with two unequal arguments returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(= 3 4)");
}

test "= with multiple equal arguments returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(= 5 5 5 5 5)");
}

test "= with multiple arguments - false case" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(= 5 5 5 4 5)");
}

test "= with negative numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(= -5 -5 -5)");
}

test "= with floating point numbers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(= 3.14 3.14 3.14)");
}

test "= with mixed integer and float - equal values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(= 3 3.0)");
}

test "= with mixed integer and float - unequal values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(= 3 3.14)");
}

test "= with zero values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(= 0 0.0 0)");
}

test "= with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(= #t 5)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// Mathematical functions
////////////////////////////////////////////////////////////////////////////////

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
        Vm.Error.UncaughtException,
        vm.evalStr("(abs)"),
    );
}

test "abs with multiple arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(abs 5 10)"),
    );
}

test "abs with non-number is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(abs #t)"),
    );
}
