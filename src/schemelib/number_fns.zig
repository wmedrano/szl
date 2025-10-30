const std = @import("std");
const testing = std.testing;

const ErrorDetails = @import("../types/ErrorDetails.zig");
const Instruction = @import("../instruction.zig").Instruction;
const Module = @import("../types/Module.zig");
const NativeProc = @import("../types/NativeProc.zig");
const number = @import("../types/number.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Number = Val.Number;
const Vm = @import("../Vm.zig");

/// Compare two numbers exactly, preserving precision for exact numbers.
/// Returns the ordering relationship between a and b.
fn compareNumbers(a: Number, b: Number) std.math.Order {
    // Fast path: same type comparisons
    switch (a) {
        .int => |a_int| switch (b) {
            .int => |b_int| return std.math.order(a_int, b_int),
            .rational => |b_rat| {
                // Convert a to rational: a_int/1, compare with b_rat
                // a_int/1 < b_num/b_den iff a_int * b_den < b_num * 1
                const lhs = a_int * @as(i64, b_rat.denominator);
                const rhs = @as(i64, b_rat.numerator);
                return std.math.order(lhs, rhs);
            },
            .float => |b_float| {
                // Try exact comparison if float is an exact integer within i64 range
                const min_i64_f64: f64 = @floatFromInt(std.math.minInt(i64));
                const max_i64_f64: f64 = 9223372036854775808.0; // 2^63, just above max i64
                if (@round(b_float) == b_float and b_float >= min_i64_f64 and b_float < max_i64_f64) {
                    const b_int: i64 = @intFromFloat(b_float);
                    return std.math.order(a_int, b_int);
                }
                // Fall back to float comparison
                const a_float: f64 = @floatFromInt(a_int);
                return std.math.order(a_float, b_float);
            },
        },
        .rational => |a_rat| switch (b) {
            .int => |b_int| {
                // a_num/a_den < b_int/1 iff a_num * 1 < b_int * a_den
                const lhs = @as(i64, a_rat.numerator);
                const rhs = b_int * @as(i64, a_rat.denominator);
                return std.math.order(lhs, rhs);
            },
            .rational => |b_rat| {
                // a_num/a_den < b_num/b_den iff a_num * b_den < b_num * a_den
                const lhs = @as(i64, a_rat.numerator) * @as(i64, b_rat.denominator);
                const rhs = @as(i64, b_rat.numerator) * @as(i64, a_rat.denominator);
                return std.math.order(lhs, rhs);
            },
            .float => |b_float| {
                // Convert rational to float for comparison with inexact number
                const a_float = a.asFloat();
                return std.math.order(a_float, b_float);
            },
        },
        .float => |a_float| switch (b) {
            .int => |b_int| {
                // Try exact comparison if a is an exact integer within i64 range
                const min_i64_f64: f64 = @floatFromInt(std.math.minInt(i64));
                const max_i64_f64: f64 = 9223372036854775808.0; // 2^63, just above max i64
                if (@round(a_float) == a_float and a_float >= min_i64_f64 and a_float < max_i64_f64) {
                    const a_int: i64 = @intFromFloat(a_float);
                    return std.math.order(a_int, b_int);
                }
                // Fall back to float comparison
                const b_float: f64 = @floatFromInt(b_int);
                return std.math.order(a_float, b_float);
            },
            .rational => |_| {
                // Convert rational to float for comparison with inexact number
                const b_float = b.asFloat();
                return std.math.order(a_float, b_float);
            },
            .float => |b_float| return std.math.order(a_float, b_float),
        },
    }
}

/// Returns true if a < b
fn isLessThan(a: Number, b: Number) bool {
    return compareNumbers(a, b) == .lt;
}

/// Returns true if a <= b
fn isLessThanOrEqual(a: Number, b: Number) bool {
    return compareNumbers(a, b) != .gt;
}

/// Returns true if a > b
fn isGreaterThan(a: Number, b: Number) bool {
    return compareNumbers(a, b) == .gt;
}

/// Returns true if a >= b
fn isGreaterThanOrEqual(a: Number, b: Number) bool {
    return compareNumbers(a, b) != .lt;
}

/// Returns true if a == b
fn isEqual(a: Number, b: Number) bool {
    return compareNumbers(a, b) == .eq;
}

/// Helper function to validate all arguments are numbers and populate diagnostics on error
/// Reports diagnostics for ALL invalid arguments, not just the first one
fn validateNumberArgs(vm: *Vm, args: []const Val, diagnostics: *ErrorDetails, proc: Val) !void {
    var has_error = false;
    for (args, 0..) |v, i| {
        if (v.asNumber() == null) {
            @branchHint(.cold);
            has_error = true;
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "number",
                    .got = v,
                    .proc = proc,
                    .arg_name = null,
                    .arg_position = @intCast(i),
                } });
        }
    }
    if (has_error) {
        return Vm.Error.UncaughtException;
    }
}

/// Generic comparison helper that checks if all arguments are ordered according to compareFn
fn checkOrdered(vm: *Vm, args: []const Val, diagnostics: *ErrorDetails, proc: Val, comptime compare_fn: fn (Number, Number) bool) Vm.Error!Val {
    // Validate all arguments are numbers first
    try validateNumberArgs(vm, args, diagnostics, proc);

    const is_ordered = switch (args.len) {
        0 => true,
        1 => true,
        else => blk: {
            var prev = args[0].asNumber().?; // Safe because we validated above
            for (args[1..]) |v| {
                const curr = v.asNumber().?; // Safe because we validated above
                if (!compare_fn(prev, curr)) break :blk false;
                prev = curr;
            }
            break :blk true;
        },
    };
    return Val.initBool(is_ordered);
}

pub const add = NativeProc.withRawArgs(struct {
    pub const name = "+";
    pub const docstring =
        \\(+ z1 ...)
        \\
        \\Returns the sum of the arguments.
        \\(+ 3 4) =>  7
        \\(+ 3)   =>  3
        \\(+)     =>  0
    ;

    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        // Validate all arguments are numbers first
        try validateNumberArgs(vm, args, diagnostics, Val.initNativeProc(&add));

        var int_sum: i64 = 0;
        var rational_num: i64 = 0;
        var rational_den: i64 = 1;
        var float_sum: f64 = 0.0;
        var has_rational = false;
        var has_float = false;
        for (args) |v| {
            const num = v.asNumber().?; // Safe because we validated above
            switch (num) {
                .int => |x| int_sum += x,
                .rational => |r| {
                    // Add rational: rational_num/rational_den + r.numerator/r.denominator
                    // = (rational_num * r.denominator + r.numerator * rational_den) / (rational_den * r.denominator)
                    rational_num = rational_num * @as(i64, r.denominator) + @as(i64, r.numerator) * rational_den;
                    rational_den *= r.denominator;
                    has_rational = true;
                },
                .float => |x| {
                    float_sum += x;
                    has_float = true;
                },
            }
        }
        if (has_float) {
            float_sum += @floatFromInt(int_sum);
            if (has_rational) {
                float_sum += @as(f64, @floatFromInt(rational_num)) / @as(f64, @floatFromInt(rational_den));
            }
            return Val.initFloat(float_sum);
        }
        if (has_rational) {
            rational_num += int_sum * rational_den;
            const rational = number.Rational.fromInt64(rational_num, @intCast(rational_den)) catch |err| {
                @branchHint(.cold);
                    const component: []const u8 = switch (err) {
                        error.DenominatorZero => "denominator (division by zero)",
                        error.NumeratorTooLarge => "numerator",
                        error.DenominatorTooLarge => "denominator",
                    };
                diagnostics.addDiagnostic(vm.allocator(),.{ .arithmetic_overflow = .{
                        .operation = "addition",
                        .component = component,
                        .proc = Val.initNativeProc(&add),
                        .hint = "Consider using inexact arithmetic with floats",
                    } });
                return Vm.Error.UncaughtException;
            };
            return Val.initRational(rational);
        }
        return Val.initInt(int_sum);
    }
});

pub const sub = NativeProc.withRawArgs(struct {
    pub const name = "-";
    pub const docstring =
        \\(- z1 z2 ...)
        \\
        \\Returns the difference of the arguments, left-associative.
        \\With one argument, returns the negation.
        \\(- 3 4)     =>  -1
        \\(- 3 4 5)   =>  -6
        \\(- 3)       =>  -3
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        if (args.len == 0) {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(),.{ .wrong_arg_count = .{
                    .expected = 1,
                    .got = 0,
                    .proc = Val.initNativeProc(&sub),
                } });
            return Vm.Error.UncaughtException;
        }

        // Validate all arguments are numbers first
        try validateNumberArgs(vm, args, diagnostics, Val.initNativeProc(&sub));

        // Single argument: return negation
        if (args.len == 1) {
            switch (args[0].asNumber().?) { // Safe because we validated above
                .float => |x| return Val.initFloat(-x),
                .rational => |r| return Val.initRational(number.Rational.init(-r.numerator, r.denominator)),
                .int => |x| return Val.initInt(-x),
            }
        }

        // Get the first value and check if we have floats/rationals
        var has_float = false;
        var has_rational = false;
        var int_result: i64 = 0;
        var rational_num: i64 = 0;
        var rational_den: i64 = 1;
        var float_result: f64 = 0.0;
        const first_num = args[0].asNumber().?; // Safe because we validated above
        switch (first_num) {
            .int => |x| int_result = x,
            .rational => |r| {
                rational_num = r.numerator;
                rational_den = r.denominator;
                has_rational = true;
            },
            .float => |x| {
                float_result = x;
                has_float = true;
            },
        }

        // Multiple arguments: subtract sequentially
        for (args[1..]) |v| {
            const num = v.asNumber().?; // Safe because we validated above
            switch (num) {
                .int => |x| int_result -= x,
                .rational => |r| {
                    // Subtract rational: rational_num/rational_den - r.numerator/r.denominator
                    rational_num = rational_num * @as(i64, r.denominator) - @as(i64, r.numerator) * rational_den;
                    rational_den *= r.denominator;
                    has_rational = true;
                },
                .float => |x| {
                    float_result -= x;
                    has_float = true;
                },
            }
        }

        if (has_float) {
            float_result += @floatFromInt(int_result);
            if (has_rational) {
                float_result += @as(f64, @floatFromInt(rational_num)) / @as(f64, @floatFromInt(rational_den));
            }
            return Val.initFloat(float_result);
        }
        if (has_rational) {
            // Subtract int_result from rational: rational_num/rational_den - int_result
            rational_num -= int_result * rational_den;
            const rational = number.Rational.fromInt64(rational_num, @intCast(@abs(rational_den))) catch |err| {
                @branchHint(.cold);
                    const component: []const u8 = switch (err) {
                        error.DenominatorZero => "denominator (division by zero)",
                        error.NumeratorTooLarge => "numerator",
                        error.DenominatorTooLarge => "denominator",
                    };
                diagnostics.addDiagnostic(vm.allocator(),.{ .arithmetic_overflow = .{
                        .operation = "subtraction",
                        .component = component,
                        .proc = Val.initNativeProc(&sub),
                        .hint = "Consider using inexact arithmetic with floats",
                    } });
                return Vm.Error.UncaughtException;
            };
            return Val.initRational(rational);
        }
        return Val.initInt(int_result);
    }
});

pub const mul = NativeProc.withRawArgs(struct {
    pub const name = "*";
    pub const docstring =
        \\(* z1 ...)
        \\
        \\Returns the product of the arguments.
        \\(* 3 4)   =>  12
        \\(* 3)     =>  3
        \\(*)       =>  1
    ;

    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        // Validate all arguments are numbers first
        try validateNumberArgs(vm, args, diagnostics, Val.initNativeProc(&mul));

        var int_product: i64 = 1;
        var rational_num: i64 = 1;
        var rational_den: i64 = 1;
        var float_product: f64 = 1.0;
        var has_rational = false;
        var has_float = false;
        for (args) |v| {
            const num = v.asNumber().?; // Safe because we validated above
            switch (num) {
                .int => |x| int_product *= x,
                .rational => |r| {
                    // Multiply rational: (rational_num/rational_den) * (r.numerator/r.denominator)
                    // = (rational_num * r.numerator) / (rational_den * r.denominator)
                    rational_num *= r.numerator;
                    rational_den *= r.denominator;
                    has_rational = true;
                },
                .float => |x| {
                    float_product *= x;
                    has_float = true;
                },
            }
        }
        if (has_float) {
            float_product *= @floatFromInt(int_product);
            if (has_rational) {
                float_product *= @as(f64, @floatFromInt(rational_num)) / @as(f64, @floatFromInt(rational_den));
            }
            return Val.initFloat(float_product);
        }
        if (has_rational) {
            rational_num *= int_product;
            const rational = number.Rational.fromInt64(rational_num, @intCast(rational_den)) catch |err| {
                @branchHint(.cold);
                    const component: []const u8 = switch (err) {
                        error.DenominatorZero => "denominator (division by zero)",
                        error.NumeratorTooLarge => "numerator",
                        error.DenominatorTooLarge => "denominator",
                    };
                diagnostics.addDiagnostic(vm.allocator(),.{ .arithmetic_overflow = .{
                        .operation = "multiplication",
                        .component = component,
                        .proc = Val.initNativeProc(&mul),
                        .hint = "Consider using inexact arithmetic with floats",
                    } });
                return Vm.Error.UncaughtException;
            };
            return Val.initRational(rational);
        }
        return Val.initInt(int_product);
    }
});

pub const div = NativeProc.withRawArgs(struct {
    pub const name = "/";
    pub const docstring =
        \\(/ z1 z2 ...)
        \\
        \\Returns the quotient of the arguments, left-associative.
        \\With one argument, returns the reciprocal.
        \\(/ 12 3)     =>  4
        \\(/ 12 3 2)   =>  2
        \\(/ 4)        =>  1/4
        \\(/ 1 2)      =>  1/2
    ;

    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        if (args.len == 0) {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(),.{ .wrong_arg_count = .{
                    .expected = 1,
                    .got = 0,
                    .proc = Val.initNativeProc(&div),
                } });
            return Vm.Error.UncaughtException;
        }

        // Validate all arguments are numbers first
        try validateNumberArgs(vm, args, diagnostics, Val.initNativeProc(&div));

        // Single argument: return reciprocal
        if (args.len == 1) {
            const num = args[0].asNumber().?; // Safe because we validated above
            switch (num) {
                .int => |x| {
                    if (x == 0) {
                        @branchHint(.cold);
                                diagnostics.addDiagnostic(vm.allocator(),.{ .arithmetic_overflow = .{
                                .operation = "division",
                                .component = "denominator (division by zero)",
                                .proc = Val.initNativeProc(&div),
                                .hint = null,
                            } });
                        return Vm.Error.UncaughtException;
                    }
                    if (x == 1) return Val.initInt(1);
                    if (x == -1) return Val.initInt(-1);
                    // Return 1/x as a rational
                    const rational = number.Rational.fromInt64(1, @intCast(@abs(x))) catch |err| {
                        @branchHint(.cold);
                                    const component: []const u8 = switch (err) {
                                error.DenominatorZero => "denominator (division by zero)",
                                error.NumeratorTooLarge => "numerator",
                                error.DenominatorTooLarge => "denominator",
                            };
                        diagnostics.addDiagnostic(vm.allocator(),.{ .arithmetic_overflow = .{
                                .operation = "division",
                                .component = component,
                                .proc = Val.initNativeProc(&div),
                                .hint = "Consider using inexact arithmetic with floats",
                            } });
                        return Vm.Error.UncaughtException;
                    };
                    return Val.initRational(if (x < 0) number.Rational.init(-rational.numerator, rational.denominator) else rational);
                },
                .rational => |r| {
                    // Reciprocal: flip numerator and denominator
                    if (r.numerator == 0) {
                        @branchHint(.cold);
                                diagnostics.addDiagnostic(vm.allocator(),.{ .arithmetic_overflow = .{
                                .operation = "division",
                                .component = "denominator (division by zero)",
                                .proc = Val.initNativeProc(&div),
                                .hint = null,
                            } });
                        return Vm.Error.UncaughtException;
                    }
                    const rational = number.Rational.fromInt64(@as(i64, r.denominator), @intCast(@abs(r.numerator))) catch |err| {
                        @branchHint(.cold);
                                    const component: []const u8 = switch (err) {
                                error.DenominatorZero => "denominator (division by zero)",
                                error.NumeratorTooLarge => "numerator",
                                error.DenominatorTooLarge => "denominator",
                            };
                        diagnostics.addDiagnostic(vm.allocator(),.{ .arithmetic_overflow = .{
                                .operation = "division",
                                .component = component,
                                .proc = Val.initNativeProc(&div),
                                .hint = "Consider using inexact arithmetic with floats",
                            } });
                        return Vm.Error.UncaughtException;
                    };
                    return Val.initRational(if (r.numerator < 0) number.Rational.init(-rational.numerator, rational.denominator) else rational);
                },
                .float => |x| {
                    if (x == 0.0) {
                        @branchHint(.cold);
                                diagnostics.addDiagnostic(vm.allocator(),.{ .arithmetic_overflow = .{
                                .operation = "division",
                                .component = "denominator (division by zero)",
                                .proc = Val.initNativeProc(&div),
                                .hint = null,
                            } });
                        return Vm.Error.UncaughtException;
                    }
                    return Val.initFloat(1.0 / x);
                },
            }
        }

        // Multiple arguments: divide sequentially
        // Track whether we have floats or rationals
        var has_float = false;
        var has_rational = false;

        // Initialize result with first argument
        const first_num = args[0].asNumber().?; // Safe because we validated above
        var int_result: i64 = 0;
        var rational_num: i64 = 1;
        var rational_den: i64 = 1;
        var float_result: f64 = 0.0;

        switch (first_num) {
            .int => |x| int_result = x,
            .rational => |r| {
                rational_num = r.numerator;
                rational_den = r.denominator;
                has_rational = true;
            },
            .float => |x| {
                float_result = x;
                has_float = true;
            },
        }

        // Divide by remaining arguments
        for (args[1..]) |v| {
            const num = v.asNumber().?; // Safe because we validated above
            switch (num) {
                .int => |x| {
                    if (x == 0) {
                        @branchHint(.cold);
                                diagnostics.addDiagnostic(vm.allocator(),.{ .arithmetic_overflow = .{
                                .operation = "division",
                                .component = "denominator (division by zero)",
                                .proc = Val.initNativeProc(&div),
                                .hint = null,
                            } });
                        return Vm.Error.UncaughtException;
                    }
                    if (!has_float and !has_rational) {
                        // Convert int division to rational
                        rational_num = int_result;
                        rational_den = x;
                        has_rational = true;
                        int_result = 0;
                    } else if (has_rational) {
                        // Divide rational by int: (rational_num/rational_den) / x = rational_num / (rational_den * x)
                        rational_den *= x;
                    } else {
                        // has_float
                        float_result /= @floatFromInt(x);
                    }
                },
                .rational => |r| {
                    if (r.numerator == 0) {
                        @branchHint(.cold);
                                diagnostics.addDiagnostic(vm.allocator(),.{ .arithmetic_overflow = .{
                                .operation = "division",
                                .component = "denominator (division by zero)",
                                .proc = Val.initNativeProc(&div),
                                .hint = null,
                            } });
                        return Vm.Error.UncaughtException;
                    }
                    if (has_float) {
                        float_result /= (@as(f64, @floatFromInt(r.numerator)) / @as(f64, @floatFromInt(r.denominator)));
                    } else {
                        // Convert to rational and divide: (a/b) / (c/d) = (a*d) / (b*c)
                        if (!has_rational) {
                            rational_num = int_result;
                            rational_den = 1;
                            has_rational = true;
                        }
                        rational_num *= r.denominator;
                        rational_den *= r.numerator;
                    }
                },
                .float => |x| {
                    if (x == 0.0) {
                        @branchHint(.cold);
                                diagnostics.addDiagnostic(vm.allocator(),.{ .arithmetic_overflow = .{
                                .operation = "division",
                                .component = "denominator (division by zero)",
                                .proc = Val.initNativeProc(&div),
                                .hint = null,
                            } });
                        return Vm.Error.UncaughtException;
                    }
                    if (!has_float) {
                        // Convert to float
                        if (has_rational) {
                            float_result = @as(f64, @floatFromInt(rational_num)) / @as(f64, @floatFromInt(rational_den));
                        } else {
                            float_result = @floatFromInt(int_result);
                        }
                        has_float = true;
                    }
                    float_result /= x;
                },
            }
        }

        if (has_float) {
            return Val.initFloat(float_result);
        }
        if (has_rational) {
            // Handle negative denominators
            const final_num = if (rational_den < 0) -rational_num else rational_num;
            const final_den = @abs(rational_den);

            const rational = number.Rational.fromInt64(final_num, @intCast(final_den)) catch |err| {
                @branchHint(.cold);
                    const component: []const u8 = switch (err) {
                        error.DenominatorZero => "denominator (division by zero)",
                        error.NumeratorTooLarge => "numerator",
                        error.DenominatorTooLarge => "denominator",
                    };
                diagnostics.addDiagnostic(vm.allocator(),.{ .arithmetic_overflow = .{
                        .operation = "division",
                        .component = component,
                        .proc = Val.initNativeProc(&div),
                        .hint = "Consider using inexact arithmetic with floats",
                    } });
                return Vm.Error.UncaughtException;
            };
            return Val.initRational(rational);
        }
        return Val.initInt(int_result);
    }
});

pub const lt = NativeProc.withRawArgs(struct {
    pub const name = "<";
    pub const docstring =
        \\(< x1 x2 x3 ...)
        \\
        \\Returns #t if the arguments are monotonically increasing.
        \\(< 1 2 3)  =>  #t
        \\(< 1 1 2)  =>  #f
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        return checkOrdered(vm, args, diagnostics, Val.initNativeProc(&lt), isLessThan);
    }
});

pub const lte = NativeProc.withRawArgs(struct {
    pub const name = "<=";
    pub const docstring =
        \\(<= x1 x2 x3 ...)
        \\
        \\Returns #t if the arguments are monotonically non-decreasing.
        \\(<= 1 2 2 3)  =>  #t
        \\(<= 1 2 1)    =>  #f
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        return checkOrdered(vm, args, diagnostics, Val.initNativeProc(&lte), isLessThanOrEqual);
    }
});

pub const gt = NativeProc.withRawArgs(struct {
    pub const name = ">";
    pub const docstring =
        \\(> x1 x2 x3 ...)
        \\
        \\Returns #t if the arguments are monotonically decreasing.
        \\(> 3 2 1)  =>  #t
        \\(> 3 3 1)  =>  #f
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        return checkOrdered(vm, args, diagnostics, Val.initNativeProc(&gt), isGreaterThan);
    }
});

pub const gte = NativeProc.withRawArgs(struct {
    pub const name = ">=";
    pub const docstring =
        \\(>= x1 x2 x3 ...)
        \\
        \\Returns #t if the arguments are monotonically non-increasing.
        \\(>= 3 2 2 1)  =>  #t
        \\(>= 3 2 3)    =>  #f
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        return checkOrdered(vm, args, diagnostics, Val.initNativeProc(&gte), isGreaterThanOrEqual);
    }
});

pub const eq = NativeProc.withRawArgs(struct {
    pub const name = "=";
    pub const docstring =
        \\(= z1 z2 z3 ...)
        \\
        \\Returns #t if all arguments are numerically equal.
        \\(= 1 1 1)    =>  #t
        \\(= 1 1 2)    =>  #f
        \\(= 5.0 5)    =>  #t
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        return checkOrdered(vm, args, diagnostics, Val.initNativeProc(&eq), isEqual);
    }
});

pub const number_p = NativeProc.with1Arg(struct {
    pub const name = "number?";
    pub const docstring =
        \\(number? obj)
        \\
        \\Returns #t if obj is a number, #f otherwise.
        \\(number? 5)      =>  #t
        \\(number? 5.0)    =>  #t
        \\(number? "5")    =>  #f
    ;
    pub inline fn impl(_: *Vm, _: *ErrorDetails, arg: Val) Vm.Error!Val {
        return Val.initBool(arg.asNumber() != null);
    }
});

pub const integer_p = NativeProc.with1Arg(struct {
    pub const name = "integer?";
    pub const docstring =
        \\(integer? obj)
        \\
        \\Returns #t if obj is an integer (exact or inexact), #f otherwise.
        \\(integer? 5)     =>  #t
        \\(integer? 5.0)   =>  #t
        \\(integer? 5.5)   =>  #f
    ;
    pub inline fn impl(_: *Vm, _: *ErrorDetails, arg: Val) Vm.Error!Val {
        const num = arg.asNumber() orelse return Val.initBool(false);
        const is_int = switch (num) {
            .int => true,
            .float => |f| @round(f) == f,
            .rational => |r| @rem(r.numerator, @as(i32, @intCast(r.denominator))) == 0,
        };
        return Val.initBool(is_int);
    }
});

pub const exact_integer_p = NativeProc.with1Arg(struct {
    pub const name = "exact-integer?";
    pub const docstring =
        \\(exact-integer? obj)
        \\
        \\Returns #t if obj is an exact integer, #f otherwise.
        \\(exact-integer? 32)    =>  #t
        \\(exact-integer? 32.0)  =>  #f
        \\(exact-integer? 32/5)  =>  #f
    ;
    pub inline fn impl(_: *Vm, _: *ErrorDetails, arg: Val) Vm.Error!Val {
        return Val.initBool(arg.data == .int);
    }
});

pub const rational_p = NativeProc.with1Arg(struct {
    pub const name = "rational?";
    pub const docstring =
        \\(rational? obj)
        \\
        \\Returns #t if obj is a rational number, #f otherwise.
        \\(rational? 1/2)  =>  #t
        \\(rational? 5)    =>  #f
        \\(rational? 5.5)  =>  #f
    ;
    pub inline fn impl(_: *Vm, _: *ErrorDetails, arg: Val) Vm.Error!Val {
        return Val.initBool(arg.data == .rational);
    }
});

pub const exact_p = NativeProc.with1Arg(struct {
    pub const name = "exact?";
    pub const docstring =
        \\(exact? z)
        \\
        \\Returns #t if z is an exact number (integer or rational), #f otherwise.
        \\(exact? 5)      =>  #t
        \\(exact? 1/2)    =>  #t
        \\(exact? 5.5)    =>  #f
    ;
    pub inline fn impl(_: *Vm, _: *ErrorDetails, arg: Val) Vm.Error!Val {
        return Val.initBool(arg.data == .int or arg.data == .rational);
    }
});

pub const inexact_p = NativeProc.with1Arg(struct {
    pub const name = "inexact?";
    pub const docstring =
        \\(inexact? z)
        \\
        \\Returns #t if z is an inexact number (float), #f otherwise.
        \\(inexact? 5.5)  =>  #t
        \\(inexact? 5)    =>  #f
        \\(inexact? 1/2)  =>  #f
    ;
    pub inline fn impl(_: *Vm, _: *ErrorDetails, arg: Val) Vm.Error!Val {
        return Val.initBool(arg.data == .float);
    }
});

/// Implements banker's rounding (round half to even)
fn roundToEven(x: f64) f64 {
    const floor_val = @floor(x);
    const ceil_val = @ceil(x);
    const diff = x - floor_val;

    // If exactly halfway
    if (diff == 0.5) {
        // Round to even: check if floor is even
        const floor_int: i64 = @intFromFloat(floor_val);
        if (@mod(floor_int, 2) == 0) {
            return floor_val;
        } else {
            return ceil_val;
        }
    } else if (diff > 0.5) {
        return ceil_val;
    } else {
        return floor_val;
    }
}

pub const round = NativeProc.with1Arg(struct {
    pub const name = "round";
    pub const docstring =
        \\(round x)
        \\
        \\Returns the closest integer to x, rounding to even when x is halfway between two integers.
        \\(round 3.2)   =>  3
        \\(round 3.8)   =>  4
        \\(round 2.5)   =>  2
        \\(round 3.5)   =>  4
        \\(round -2.5)  =>  -2
        \\(round 5)     =>  5
        \\(round 1/2)   =>  0
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const num = arg.asNumber() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(),.{ .wrong_arg_type = .{
                .expected = "number",
                .got = arg,
                .proc = Val.initNativeProc(&round),
                .arg_name = "x",
                .arg_position = 0,
            } });
            return Vm.Error.UncaughtException;
        };

        switch (num) {
            .int => |x| return Val.initInt(x),
            .rational, .float => {
                const float_val = num.asFloat();
                const rounded = roundToEven(float_val);
                return Val.initInt(@intFromFloat(rounded));
            },
        }
    }
});

pub const finite_p = NativeProc.with1Arg(struct {
    pub const name = "finite?";
    pub const docstring =
        \\(finite? z)
        \\
        \\Returns #t if z is a finite number (not infinite or NaN), #f otherwise.
        \\All exact numbers are finite.
        \\(finite? 5)      =>  #t
        \\(finite? 1/2)    =>  #t
        \\(finite? 5.5)    =>  #t
        \\(finite? +inf.0) =>  #f
        \\(finite? -inf.0) =>  #f
        \\(finite? +nan.0) =>  #f
    ;
    pub inline fn impl(_: *Vm, _: *ErrorDetails, arg: Val) Vm.Error!Val {
        const num = arg.asNumber() orelse return Val.initBool(false);
        const is_finite = switch (num) {
            .int => true,
            .rational => true,
            .float => |f| std.math.isFinite(f),
        };
        return Val.initBool(is_finite);
    }
});

pub const zero_p = NativeProc.with1Arg(struct {
    pub const name = "zero?";
    pub const docstring =
        \\(zero? z)
        \\
        \\Returns #t if z is zero, #f otherwise.
        \\(zero? 0)    =>  #t
        \\(zero? 0.0)  =>  #t
        \\(zero? 1)    =>  #f
        \\(zero? -1)   =>  #f
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const num = arg.asNumber() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(),.{ .wrong_arg_type = .{
                .expected = "number",
                .got = arg,
                .proc = Val.initNativeProc(&zero_p),
                .arg_name = "z",
                .arg_position = 0,
            } });
            return Vm.Error.UncaughtException;
        };

        const is_zero = switch (num) {
            .int => |x| x == 0,
            .rational => |r| r.numerator == 0,
            .float => |f| f == 0.0,
        };
        return Val.initBool(is_zero);
    }
});

pub const positive_p = NativeProc.with1Arg(struct {
    pub const name = "positive?";
    pub const docstring =
        \\(positive? x)
        \\
        \\Returns #t if x is greater than zero, #f otherwise.
        \\(positive? 1)    =>  #t
        \\(positive? 0)    =>  #f
        \\(positive? -1)   =>  #f
        \\(positive? 0.5)  =>  #t
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const num = arg.asNumber() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(),.{ .wrong_arg_type = .{
                .expected = "number",
                .got = arg,
                .proc = Val.initNativeProc(&positive_p),
                .arg_name = "x",
                .arg_position = 0,
            } });
            return Vm.Error.UncaughtException;
        };

        const is_positive = switch (num) {
            .int => |x| x > 0,
            .rational => |r| r.numerator > 0,
            .float => |f| f > 0.0,
        };
        return Val.initBool(is_positive);
    }
});

pub const negative_p = NativeProc.with1Arg(struct {
    pub const name = "negative?";
    pub const docstring =
        \\(negative? x)
        \\
        \\Returns #t if x is less than zero, #f otherwise.
        \\(negative? -1)   =>  #t
        \\(negative? 0)    =>  #f
        \\(negative? 1)    =>  #f
        \\(negative? -0.5) =>  #t
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const num = arg.asNumber() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(),.{ .wrong_arg_type = .{
                .expected = "number",
                .got = arg,
                .proc = Val.initNativeProc(&negative_p),
                .arg_name = "x",
                .arg_position = 0,
            } });
            return Vm.Error.UncaughtException;
        };

        const is_negative = switch (num) {
            .int => |x| x < 0,
            .rational => |r| r.numerator < 0,
            .float => |f| f < 0.0,
        };
        return Val.initBool(is_negative);
    }
});

pub const odd_p = NativeProc.with1Arg(struct {
    pub const name = "odd?";
    pub const docstring =
        \\(odd? n)
        \\
        \\Returns #t if n is an odd integer, #f otherwise.
        \\(odd? 3)   =>  #t
        \\(odd? 2)   =>  #f
        \\(odd? -1)  =>  #t
        \\(odd? 0)   =>  #f
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const num = arg.asNumber() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(),.{ .wrong_arg_type = .{
                .expected = "number",
                .got = arg,
                .proc = Val.initNativeProc(&odd_p),
                .arg_name = "n",
                .arg_position = 0,
            } });
            return Vm.Error.UncaughtException;
        };

        const is_odd = switch (num) {
            .int => |x| @mod(x, 2) != 0,
            .rational => |r| blk: {
                // Only odd if it's an integer (denominator divides numerator evenly)
                // and the resulting integer is odd
                if (@rem(r.numerator, @as(i32, @intCast(r.denominator))) != 0) {
                    break :blk false; // Not an integer
                }
                const int_val = @divExact(r.numerator, @as(i32, @intCast(r.denominator)));
                break :blk @mod(int_val, 2) != 0;
            },
            .float => |f| blk: {
                // Only odd if it's an exact integer and odd
                if (@round(f) != f) break :blk false;
                const int_val: i64 = @intFromFloat(f);
                break :blk @mod(int_val, 2) != 0;
            },
        };
        return Val.initBool(is_odd);
    }
});

pub const even_p = NativeProc.with1Arg(struct {
    pub const name = "even?";
    pub const docstring =
        \\(even? n)
        \\
        \\Returns #t if n is an even integer, #f otherwise.
        \\(even? 2)   =>  #t
        \\(even? 3)   =>  #f
        \\(even? -2)  =>  #t
        \\(even? 0)   =>  #t
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const num = arg.asNumber() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(),.{ .wrong_arg_type = .{
                .expected = "number",
                .got = arg,
                .proc = Val.initNativeProc(&even_p),
                .arg_name = "n",
                .arg_position = 0,
            } });
            return Vm.Error.UncaughtException;
        };

        const is_even = switch (num) {
            .int => |x| @mod(x, 2) == 0,
            .rational => |r| blk: {
                // Only even if it's an integer (denominator divides numerator evenly)
                // and the resulting integer is even
                if (@rem(r.numerator, @as(i32, @intCast(r.denominator))) != 0) {
                    break :blk false; // Not an integer
                }
                const int_val = @divExact(r.numerator, @as(i32, @intCast(r.denominator)));
                break :blk @mod(int_val, 2) == 0;
            },
            .float => |f| blk: {
                // Only even if it's an exact integer and even
                if (@round(f) != f) break :blk false;
                const int_val: i64 = @intFromFloat(f);
                break :blk @mod(int_val, 2) == 0;
            },
        };
        return Val.initBool(is_even);
    }
});

pub const max = NativeProc.withRawArgs(struct {
    pub const name = "max";
    pub const docstring =
        \\(max x1 x2 ...)
        \\
        \\Returns the maximum of the arguments.
        \\(max 3 4)     =>  4
        \\(max 3 4 5)   =>  5
        \\(max 3)       =>  3
        \\(max 3.5 2)   =>  3.5
    ;

    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        if (args.len == 0) {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(),.{ .wrong_arg_count = .{
                    .expected = 1,
                    .got = 0,
                    .proc = Val.initNativeProc(&max),
                } });
            return Vm.Error.UncaughtException;
        }

        // Validate all arguments are numbers first
        try validateNumberArgs(vm, args, diagnostics, Val.initNativeProc(&max));

        // Check if any argument is a float (inexact)
        var has_float = false;
        for (args) |v| {
            if (v.asNumber().? == .float) {
                has_float = true;
                break;
            }
        }

        var max_val = args[0];
        for (args[1..]) |v| {
            if (isGreaterThan(v.asNumber().?, max_val.asNumber().?)) {
                max_val = v;
            }
        }

        // If any argument was a float, convert result to float
        if (has_float and max_val.asNumber().? != .float) {
            return Val.initFloat(max_val.asNumber().?.asFloat());
        }
        return max_val;
    }
});

pub const min = NativeProc.withRawArgs(struct {
    pub const name = "min";
    pub const docstring =
        \\(min x1 x2 ...)
        \\
        \\Returns the minimum of the arguments.
        \\(min 3 4)     =>  3
        \\(min 3 4 5)   =>  3
        \\(min 5)       =>  5
        \\(min 3.5 2)   =>  2
    ;

    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        if (args.len == 0) {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(),.{ .wrong_arg_count = .{
                    .expected = 1,
                    .got = 0,
                    .proc = Val.initNativeProc(&min),
                } });
            return Vm.Error.UncaughtException;
        }

        // Validate all arguments are numbers first
        try validateNumberArgs(vm, args, diagnostics, Val.initNativeProc(&min));

        // Check if any argument is a float (inexact)
        var has_float = false;
        for (args) |v| {
            if (v.asNumber().? == .float) {
                has_float = true;
                break;
            }
        }

        var min_val = args[0];
        for (args[1..]) |v| {
            if (isLessThan(v.asNumber().?, min_val.asNumber().?)) {
                min_val = v;
            }
        }

        // If any argument was a float, convert result to float
        if (has_float and min_val.asNumber().? != .float) {
            return Val.initFloat(min_val.asNumber().?.asFloat());
        }
        return min_val;
    }
});

pub const abs = NativeProc.with1Arg(struct {
    pub const name = "abs";
    pub const docstring =
        \\(abs x)
        \\
        \\Returns the absolute value of x.
        \\(abs -7)    =>  7
        \\(abs 7)     =>  7
        \\(abs 0)     =>  0
        \\(abs -3.5)  =>  3.5
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const num = arg.asNumber() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(),.{ .wrong_arg_type = .{
                .expected = "number",
                .got = arg,
                .proc = Val.initNativeProc(&abs),
                .arg_name = "x",
                .arg_position = 0,
            } });
            return Vm.Error.UncaughtException;
        };

        return switch (num) {
            .int => |x| Val.initInt(@intCast(@abs(x))),
            .rational => |r| Val.initRational(number.Rational.init(@intCast(@abs(r.numerator)), r.denominator)),
            .float => |f| Val.initFloat(@abs(f)),
        };
    }
});

test "+ on ints sums ints" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("10", "(+ 1 2 3 4)");
}

test "empty + returns 0" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0", "(+)");
}

test "+ on non-ints returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(+ #t)");
}

test "+ on floats sums floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("6.5", "(+ 1.5 2.0 3.0)");
}

test "+ on mixed ints and floats returns float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("10.5", "(+ 1 2.5 3 4)");
}

test "- on two ints subtracts them" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("5", "(- 10 5)");
    try vm.expectEval("-5", "(- 5 10)");
}

test "- on multiple ints subtracts sequentially" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("5", "(- 10 3 2)");
    try vm.expectEval("0", "(- 10 5 5)");
}

test "- with single arg returns negation" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("-5", "(- 5)");
    try vm.expectEval("5", "(- -5)");
    try vm.expectEval("0", "(- 0)");
}

test "- with no args returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(-)");
}

test "- on non-ints returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(- #t)");
    try vm.expectError(Vm.Error.UncaughtException, "(- 5 #f)");
}

test "- on floats subtracts floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("5.5", "(- 10.5 5.0)");
}

test "- with single float negates it" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("-5.5", "(- 5.5)");
}

test "- on mixed ints and floats returns float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0.5", "(- 10 5 4.5)");
}

test "<= on ordered ints returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<= 1 2 3)");
}

test "<= on non-ordered ints returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(<= 3 2 1)");
}

test "<= on equal ints returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<= 1 1 2)");
}

test "<= with less than 2 args returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<= )");
    try vm.expectEval("#t", "(<= 1)");
}

test "<= on non-ints returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(<= #t)");
}

test "<= on floats returns correct result" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<= 1.5 2.5 3.5)");
    try vm.expectEval("#f", "(<= 3.5 2.5 1.5)");
    try vm.expectEval("#t", "(<= 2.5 2.5)");
}

test "<= on mixed ints and floats returns correct result" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(<= 1 2.5 3)");
    try vm.expectEval("#f", "(<= 3 2.5 1)");
    try vm.expectEval("#t", "(<= 1.5 2 2.5)");
}

test "< on strictly ordered ints returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(< 1 2 3)");
}

test "< on non-ordered ints returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(< 3 2 1)");
}

test "< on equal ints returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(< 1 1 2)");
    try vm.expectEval("#f", "(< 1 2 2)");
}

test "< with less than 2 args returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(< )");
    try vm.expectEval("#t", "(< 1)");
}

test "< on non-ints returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(< #t)");
}

test "< on floats returns correct result" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(< 1.5 2.5 3.5)");
    try vm.expectEval("#f", "(< 3.5 2.5 1.5)");
    try vm.expectEval("#f", "(< 2.5 2.5)");
}

test "< on mixed ints and floats returns correct result" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(< 1 2.5 3)");
    try vm.expectEval("#f", "(< 3 2.5 1)");
    try vm.expectEval("#t", "(< 1.5 2 2.5)");
}

test "> on strictly ordered descending ints returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(> 3 2 1)");
}

test "> on non-ordered ints returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(> 1 2 3)");
}

test "> on equal ints returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(> 2 1 1)");
    try vm.expectEval("#f", "(> 2 2 1)");
}

test "> with less than 2 args returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(> )");
    try vm.expectEval("#t", "(> 1)");
}

test "> on non-ints returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(> #t)");
}

test "> on floats returns correct result" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(> 3.5 2.5 1.5)");
    try vm.expectEval("#f", "(> 1.5 2.5 3.5)");
    try vm.expectEval("#f", "(> 2.5 2.5)");
}

test "> on mixed ints and floats returns correct result" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(> 3 2.5 1)");
    try vm.expectEval("#f", "(> 1 2.5 3)");
    try vm.expectEval("#t", "(> 2.5 2 1.5)");
}

test ">= on ordered descending ints returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(>= 3 2 1)");
}

test ">= on non-ordered ints returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(>= 1 2 3)");
}

test ">= on equal ints returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(>= 2 2 1)");
    try vm.expectEval("#t", "(>= 3 2 2)");
}

test ">= with less than 2 args returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(>= )");
    try vm.expectEval("#t", "(>= 1)");
}

test ">= on non-ints returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(>= #t)");
}

test ">= on floats returns correct result" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(>= 3.5 2.5 1.5)");
    try vm.expectEval("#f", "(>= 1.5 2.5 3.5)");
    try vm.expectEval("#t", "(>= 2.5 2.5)");
}

test ">= on mixed ints and floats returns correct result" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(>= 3 2.5 1)");
    try vm.expectEval("#f", "(>= 1 2.5 3)");
    try vm.expectEval("#t", "(>= 2.5 2 1.5)");
}

test "= on equal ints returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(= 5 5)");
    try vm.expectEval("#t", "(= 1 1 1)");
    try vm.expectEval("#t", "(= 42 42 42 42)");
}

test "= on non-equal ints returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(= 1 2)");
    try vm.expectEval("#f", "(= 1 1 2)");
    try vm.expectEval("#f", "(= 1 2 1)");
}

test "= with less than 2 args returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(= )");
    try vm.expectEval("#t", "(= 1)");
}

test "= on non-ints returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(= #t)");
}

test "= on floats returns correct result" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(= 5.5 5.5)");
    try vm.expectEval("#t", "(= 2.5 2.5 2.5)");
    try vm.expectEval("#f", "(= 5.5 5.6)");
    try vm.expectEval("#f", "(= 2.5 2.5 2.6)");
}

test "= on mixed ints and floats returns correct result" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(= 5 5.0)");
    try vm.expectEval("#t", "(= 2.0 2 2.0)");
    try vm.expectEval("#f", "(= 5 5.1)");
    try vm.expectEval("#f", "(= 2.0 2 2.1)");
}

test "number? with integer returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(number? 0)");
    try vm.expectEval("#t", "(number? 42)");
    try vm.expectEval("#t", "(number? -5)");
}

test "number? with non-number returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(number? #t)");
    try vm.expectEval("#f", "(number? #f)");
    try vm.expectEval("#f", "(number? 'symbol)");
    try vm.expectEval("#f", "(number? \"string\")");
    try vm.expectEval("#f", "(number? '())");
}

test "number? with no arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(number?)");
}

test "number? with multiple arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(number? 1 2)");
}

test "number? with float returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(number? 0.0)");
    try vm.expectEval("#t", "(number? 42.5)");
    try vm.expectEval("#t", "(number? -5.25)");
}

test "integer? with integer returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(integer? 0)");
    try vm.expectEval("#t", "(integer? 42)");
    try vm.expectEval("#t", "(integer? -5)");
}

test "integer? with non-integer returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(integer? #t)");
    try vm.expectEval("#f", "(integer? #f)");
    try vm.expectEval("#f", "(integer? 'symbol)");
    try vm.expectEval("#f", "(integer? \"string\")");
    try vm.expectEval("#f", "(integer? '())");
}

test "integer? with no arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(integer?)");
}

test "integer? with multiple arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(integer? 1 2)");
}

test "integer? with inexact integer returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(integer? 0.0)");
    try vm.expectEval("#t", "(integer? 42.0)");
    try vm.expectEval("#t", "(integer? -5.0)");
    try vm.expectEval("#t", "(integer? 1.0)");
}

test "integer? with non-integer float returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(integer? 0.5)");
    try vm.expectEval("#f", "(integer? 42.5)");
    try vm.expectEval("#f", "(integer? -5.25)");
}

test "integer? with rational that is whole number returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(integer? 10/2)");
    try vm.expectEval("#t", "(integer? 15/3)");
    try vm.expectEval("#t", "(integer? -8/4)");
}

test "integer? with non-integer rational returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(integer? 1/2)");
    try vm.expectEval("#f", "(integer? 5/3)");
    try vm.expectEval("#f", "(integer? -7/4)");
}

test "exact-integer? with exact integers returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(exact-integer? 0)");
    try vm.expectEval("#t", "(exact-integer? 32)");
    try vm.expectEval("#t", "(exact-integer? -42)");
}

test "exact-integer? with inexact integers returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(exact-integer? 0.0)");
    try vm.expectEval("#f", "(exact-integer? 32.0)");
    try vm.expectEval("#f", "(exact-integer? -5.0)");
}

test "exact-integer? with rationals returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(exact-integer? 1/2)");
    try vm.expectEval("#f", "(exact-integer? 32/5)");
    try vm.expectEval("#f", "(exact-integer? 7/3)");
}

test "exact-integer? with non-integer floats returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(exact-integer? 5.5)");
    try vm.expectEval("#f", "(exact-integer? -2.5)");
}

test "exact-integer? with non-numbers returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(exact-integer? #t)");
    try vm.expectEval("#f", "(exact-integer? 'symbol)");
    try vm.expectEval("#f", "(exact-integer? \"string\")");
}

test "exact-integer? with no arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(exact-integer?)");
}

test "exact-integer? with multiple arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(exact-integer? 1 2)");
}

test "+ on rationals adds rationals" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3/4", "(+ 1/2 1/4)");
    try vm.expectEval("1", "(+ 1/3 1/3 1/3)"); // Reduces to 1/1, which becomes integer 1
}

test "- on rationals subtracts rationals" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1/2", "(- 3/4 1/4)");
    try vm.expectEval("1/2", "(- -1/2)");
}

test "< on rationals returns correct result" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(< 1/2 3/4)");
    try vm.expectEval("#f", "(< 3/4 1/2)");
}

test "= on rationals returns correct result" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(= 1/2 2/4)");
    try vm.expectEval("#f", "(= 1/2 1/3)");
}

test "+ on mixed ints and rationals returns rational" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3/2", "(+ 1 1/2)");
    try vm.expectEval("7/2", "(+ 1/2 3)");
}

test "rational arithmetic that produces integers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1", "(+ 1/2 1/2)");
    try vm.expectEval("3", "(+ 1/3 1/3 1/3 2)");
    try vm.expectEval("0", "(- 1/4 1/4)");
}

test "+ on mixed rationals and floats returns float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1.5", "(+ 0.5 1)");
    try vm.expectEval("2.25", "(+ 1/4 2.0)");
    try vm.expectEval("3.5", "(+ 1/2 3.0)");
}

test "- on mixed rationals and floats returns float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1.5", "(- 2.0 1/2)");
    try vm.expectEval("-0.25", "(- 1/4 0.5)");
    try vm.expectEval("2.75", "(- 3.0 1/4)");
}

test "rational? with rational returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(rational? 1/2)");
    try vm.expectEval("#t", "(rational? 3/4)");
    try vm.expectEval("#t", "(rational? -1/2)");
}

test "rational? with non-rational returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(rational? 5)");
    try vm.expectEval("#f", "(rational? 5.5)");
    try vm.expectEval("#f", "(rational? #t)");
    try vm.expectEval("#f", "(rational? 'symbol)");
    try vm.expectEval("#f", "(rational? \"string\")");
}

test "rational? with rationals that reduce to integers returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Even though these reduce to integers at parse time,
    // they were originally written as rationals
    try vm.expectEval("#f", "(rational? 4/2)"); // This becomes integer 2
    try vm.expectEval("#f", "(rational? 6/3)"); // This becomes integer 2
}

test "rational? with no arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(rational?)");
}

test "rational? with multiple arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(rational? 1/2 1/3)");
}

test "exact? with integers returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(exact? 0)");
    try vm.expectEval("#t", "(exact? 42)");
    try vm.expectEval("#t", "(exact? -5)");
}

test "exact? with rationals returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(exact? 1/2)");
    try vm.expectEval("#t", "(exact? 3/4)");
    try vm.expectEval("#t", "(exact? -1/2)");
}

test "exact? with floats returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(exact? 0.0)");
    try vm.expectEval("#f", "(exact? 5.5)");
    try vm.expectEval("#f", "(exact? -2.5)");
}

test "exact? with non-numbers returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(exact? #t)");
    try vm.expectEval("#f", "(exact? 'symbol)");
    try vm.expectEval("#f", "(exact? \"string\")");
}

test "exact? with no arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(exact?)");
}

test "exact? with multiple arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(exact? 1 2)");
}

test "inexact? with floats returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(inexact? 0.0)");
    try vm.expectEval("#t", "(inexact? 5.5)");
    try vm.expectEval("#t", "(inexact? -2.5)");
}

test "inexact? with integers returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(inexact? 0)");
    try vm.expectEval("#f", "(inexact? 42)");
    try vm.expectEval("#f", "(inexact? -5)");
}

test "inexact? with rationals returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(inexact? 1/2)");
    try vm.expectEval("#f", "(inexact? 3/4)");
    try vm.expectEval("#f", "(inexact? -1/2)");
}

test "inexact? with non-numbers returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(inexact? #t)");
    try vm.expectEval("#f", "(inexact? 'symbol)");
    try vm.expectEval("#f", "(inexact? \"string\")");
}

test "inexact? with no arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(inexact?)");
}

test "inexact? with multiple arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(inexact? 1.0 2.0)");
}

test "round with integers returns unchanged" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("5", "(round 5)");
    try vm.expectEval("0", "(round 0)");
    try vm.expectEval("-10", "(round -10)");
}

test "round with positive floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3", "(round 3.2)");
    try vm.expectEval("4", "(round 3.8)");
    try vm.expectEval("1", "(round 1.0)");
    try vm.expectEval("0", "(round 0.3)");
}

test "round with negative floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("-3", "(round -3.2)");
    try vm.expectEval("-4", "(round -3.8)");
    try vm.expectEval("-1", "(round -1.0)");
    try vm.expectEval("0", "(round -0.3)");
}

test "round with banker's rounding (round half to even)" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("2", "(round 2.5)");
    try vm.expectEval("4", "(round 3.5)");
    try vm.expectEval("4", "(round 4.5)");
    try vm.expectEval("6", "(round 5.5)");
    try vm.expectEval("-2", "(round -2.5)");
    try vm.expectEval("-4", "(round -3.5)");
}

test "round with rationals" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0", "(round 1/2)"); // 0.5 -> 0 (round to even)
    try vm.expectEval("2", "(round 3/2)"); // 1.5 -> 2 (round to even)
    try vm.expectEval("2", "(round 5/2)"); // 2.5 -> 2 (round to even)
    try vm.expectEval("4", "(round 7/2)"); // 3.5 -> 4 (round to even)
    try vm.expectEval("1", "(round 4/3)"); // 1.333... -> 1
    try vm.expectEval("-2", "(round -3/2)"); // -1.5 -> -2 (round to even)
}

test "round with no arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(round)");
}

test "round with multiple arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(round 1 2)");
}

test "round with non-number returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(round #t)");
    try vm.expectError(Vm.Error.UncaughtException, "(round \"string\")");
    try vm.expectError(Vm.Error.UncaughtException, "(round 'symbol)");
}

test "= with large integers preserves precision" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // These are beyond f64 mantissa precision (2^53)
    try vm.expectEval("#f", "(= 9007199254740993 9007199254740992)");
    try vm.expectEval("#t", "(= 9007199254740993 9007199254740993)");
    try vm.expectEval("#f", "(= -9007199254740993 -9007199254740992)");
}

test "< with large integers preserves precision" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // These are beyond f64 mantissa precision (2^53)
    try vm.expectEval("#t", "(< 9007199254740992 9007199254740993)");
    try vm.expectEval("#f", "(< 9007199254740993 9007199254740992)");
    try vm.expectEval("#t", "(< -9007199254740993 -9007199254740992)");
}

test "> with large integers preserves precision" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // These are beyond f64 mantissa precision (2^53)
    try vm.expectEval("#f", "(> 9007199254740992 9007199254740993)");
    try vm.expectEval("#t", "(> 9007199254740993 9007199254740992)");
    try vm.expectEval("#f", "(> -9007199254740993 -9007199254740992)");
}

test "= with rationals uses exact comparison" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Exact rational comparisons
    try vm.expectEval("#t", "(= 1/3 1/3)");
    try vm.expectEval("#f", "(= 1/3 1/4)");
    try vm.expectEval("#t", "(= 2/6 1/3)"); // Reduced forms
    try vm.expectEval("#f", "(= 1/3 1/2)");
}

test "< with rationals uses exact comparison" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Exact rational comparisons
    try vm.expectEval("#t", "(< 1/4 1/3)");
    try vm.expectEval("#f", "(< 1/3 1/4)");
    try vm.expectEval("#t", "(< 1/3 1/2)");
    try vm.expectEval("#f", "(< 1/2 1/3)");
    try vm.expectEval("#t", "(< -1/2 -1/3)");
}

test "> with rationals uses exact comparison" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Exact rational comparisons
    try vm.expectEval("#f", "(> 1/4 1/3)");
    try vm.expectEval("#t", "(> 1/3 1/4)");
    try vm.expectEval("#f", "(> 1/3 1/2)");
    try vm.expectEval("#t", "(> 1/2 1/3)");
    try vm.expectEval("#f", "(> -1/2 -1/3)");
}

test "= with mixed int and rational uses exact comparison" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Int vs rational comparisons (exact)
    try vm.expectEval("#t", "(= 2 4/2)");
    try vm.expectEval("#f", "(= 2 5/2)");
    try vm.expectEval("#t", "(= 10/5 2)");
    try vm.expectEval("#f", "(= 1/2 1)");
}

test "< with mixed int and rational uses exact comparison" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Int vs rational comparisons (exact)
    try vm.expectEval("#t", "(< 1 3/2)");
    try vm.expectEval("#f", "(< 2 3/2)");
    try vm.expectEval("#t", "(< 1/2 1)");
    try vm.expectEval("#f", "(< 3/2 1)");
}

test "= with exact float integers uses exact comparison" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Exact integer floats should compare exactly with ints
    try vm.expectEval("#t", "(= 5 5.0)");
    try vm.expectEval("#t", "(= 5.0 5)");
    try vm.expectEval("#f", "(= 5 5.5)");
    try vm.expectEval("#t", "(= -10 -10.0)");
}

test "finite? with integers returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(finite? 0)");
    try vm.expectEval("#t", "(finite? 42)");
    try vm.expectEval("#t", "(finite? -5)");
}

test "finite? with rationals returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(finite? 1/2)");
    try vm.expectEval("#t", "(finite? 3/4)");
    try vm.expectEval("#t", "(finite? -1/2)");
}

test "finite? with finite floats returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(finite? 0.0)");
    try vm.expectEval("#t", "(finite? 5.5)");
    try vm.expectEval("#t", "(finite? -2.5)");
}

test "finite? with non-numbers returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(finite? #t)");
    try vm.expectEval("#f", "(finite? 'symbol)");
    try vm.expectEval("#f", "(finite? \"string\")");
}

test "finite? with no arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(finite?)");
}

test "finite? with multiple arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(finite? 1.0 2.0)");
}

test "finite? with infinity returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(finite? +inf.0)");
    try vm.expectEval("#f", "(finite? -inf.0)");
}

test "finite? with NaN returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(finite? +nan.0)");
}

test "< with exact float integers uses exact comparison" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Exact integer floats should compare exactly with ints
    try vm.expectEval("#t", "(< 5 6.0)");
    try vm.expectEval("#f", "(< 6 6.0)");
    try vm.expectEval("#t", "(< 5.0 6)");
    try vm.expectEval("#f", "(< 6.0 6)");
}

test "* on ints multiplies ints" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("24", "(* 2 3 4)");
    try vm.expectEval("120", "(* 5 4 3 2 1)");
}

test "empty * returns 1" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1", "(*)");
}

test "* with single arg returns that arg" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("5", "(* 5)");
    try vm.expectEval("-3", "(* -3)");
}

test "* on non-numbers returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(* #t)");
    try vm.expectError(Vm.Error.UncaughtException, "(* 5 #f)");
}

test "* on floats multiplies floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("6.0", "(* 2.0 3.0)");
    try vm.expectEval("7.5", "(* 1.5 5.0)");
}

test "* on mixed ints and floats returns float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("30.0", "(* 2 3.0 5)");
    try vm.expectEval("15.0", "(* 2.5 3 2)");
}

test "* on rationals multiplies rationals" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1/8", "(* 1/2 1/4)");
    try vm.expectEval("1/27", "(* 1/3 1/3 1/3)");
}

test "* on mixed ints and rationals returns rational" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3/2", "(* 3 1/2)");
    try vm.expectEval("2", "(* 1/2 4)");
}

test "* on mixed rationals and floats returns float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1.0", "(* 2.0 1/2)");
    try vm.expectEval("0.75", "(* 1/4 3.0)");
}

test "* with zero returns zero" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0", "(* 0 5)");
    try vm.expectEval("0", "(* 5 0)");
    try vm.expectEval("0", "(* 2 3 0 5)");
}

test "/ on two ints returns rational" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("4", "(/ 12 3)");
    try vm.expectEval("1/2", "(/ 1 2)");
    try vm.expectEval("10/3", "(/ 10 3)");
    try vm.expectEval("-4", "(/ -8 2)");
    try vm.expectEval("-1/2", "(/ 1 -2)");
}

test "/ on multiple ints divides sequentially" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("2", "(/ 12 3 2)");
    try vm.expectEval("1/6", "(/ 1 2 3)");
    try vm.expectEval("1", "(/ 24 4 3 2)");
}

test "/ with single arg returns reciprocal" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1/4", "(/ 4)");
    try vm.expectEval("1/2", "(/ 2)");
    try vm.expectEval("1", "(/ 1)");
    try vm.expectEval("-1", "(/ -1)");
    try vm.expectEval("-1/5", "(/ -5)");
}

test "/ with no args returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(/)");
}

test "/ with zero divisor returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(/ 5 0)");
    try vm.expectError(Vm.Error.UncaughtException, "(/ 0)");
    try vm.expectError(Vm.Error.UncaughtException, "(/ 10 2 0)");
}

test "/ on non-numbers returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(/ #t)");
    try vm.expectError(Vm.Error.UncaughtException, "(/ 5 #f)");
}

test "/ on floats divides floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3.0", "(/ 6.0 2.0)");
    try vm.expectEval("2.5", "(/ 5.0 2.0)");
}

test "/ with single float returns reciprocal" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0.5", "(/ 2.0)");
    try vm.expectEval("0.25", "(/ 4.0)");
}

test "/ on mixed ints and floats returns float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("4.0", "(/ 10 2.5)");
    try vm.expectEval("2.5", "(/ 5.0 2)");
    try vm.expectEval("2.0", "(/ 10.0 2 2.5)");
}

test "/ on rationals divides rationals" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("2", "(/ 1/2 1/4)");
    try vm.expectEval("1/2", "(/ 1/3 2/3)");
    try vm.expectEval("3/2", "(/ 3/4 1/2)");
}

test "/ with single rational returns reciprocal" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("2", "(/ 1/2)");
    try vm.expectEval("4/3", "(/ 3/4)");
    try vm.expectEval("-3/2", "(/ -2/3)");
}

test "/ on mixed ints and rationals returns rational" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1/4", "(/ 1/2 2)");
    try vm.expectEval("4", "(/ 2 1/2)");
    try vm.expectEval("1/6", "(/ 1 2 3)");
}

test "/ on mixed rationals and floats returns float" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("4.0", "(/ 2.0 1/2)");
    try vm.expectEval("0.125", "(/ 1/4 2.0)");
    try vm.expectEval("1.5", "(/ 3.0 2)");
}

test "/ division that produces integers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("2", "(/ 8 4)");
    try vm.expectEval("5", "(/ 10 2)");
    try vm.expectEval("1", "(/ 3 3)");
}

test "zero? with integers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(zero? 0)");
    try vm.expectEval("#f", "(zero? 1)");
    try vm.expectEval("#f", "(zero? -1)");
    try vm.expectEval("#f", "(zero? 42)");
}

test "zero? with floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(zero? 0.0)");
    try vm.expectEval("#f", "(zero? 1.0)");
    try vm.expectEval("#f", "(zero? -1.0)");
    try vm.expectEval("#f", "(zero? 0.5)");
}

test "zero? with rationals" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(zero? 1/2)");
    try vm.expectEval("#f", "(zero? -1/2)");
}

test "zero? with non-numbers returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(zero? #t)");
    try vm.expectError(Vm.Error.UncaughtException, "(zero? \"string\")");
}

test "positive? with integers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(positive? 1)");
    try vm.expectEval("#t", "(positive? 42)");
    try vm.expectEval("#f", "(positive? 0)");
    try vm.expectEval("#f", "(positive? -1)");
    try vm.expectEval("#f", "(positive? -42)");
}

test "positive? with floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(positive? 1.5)");
    try vm.expectEval("#t", "(positive? 0.5)");
    try vm.expectEval("#f", "(positive? 0.0)");
    try vm.expectEval("#f", "(positive? -0.5)");
    try vm.expectEval("#f", "(positive? -1.5)");
}

test "positive? with rationals" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(positive? 1/2)");
    try vm.expectEval("#t", "(positive? 3/4)");
    try vm.expectEval("#f", "(positive? -1/2)");
    try vm.expectEval("#f", "(positive? -3/4)");
}

test "positive? with non-numbers returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(positive? #t)");
}

test "negative? with integers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(negative? -1)");
    try vm.expectEval("#t", "(negative? -42)");
    try vm.expectEval("#f", "(negative? 0)");
    try vm.expectEval("#f", "(negative? 1)");
    try vm.expectEval("#f", "(negative? 42)");
}

test "negative? with floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(negative? -1.5)");
    try vm.expectEval("#t", "(negative? -0.5)");
    try vm.expectEval("#f", "(negative? 0.0)");
    try vm.expectEval("#f", "(negative? 0.5)");
    try vm.expectEval("#f", "(negative? 1.5)");
}

test "negative? with rationals" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(negative? -1/2)");
    try vm.expectEval("#t", "(negative? -3/4)");
    try vm.expectEval("#f", "(negative? 1/2)");
    try vm.expectEval("#f", "(negative? 3/4)");
}

test "negative? with non-numbers returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(negative? #t)");
}

test "odd? with integers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(odd? 1)");
    try vm.expectEval("#t", "(odd? 3)");
    try vm.expectEval("#t", "(odd? -1)");
    try vm.expectEval("#t", "(odd? -3)");
    try vm.expectEval("#f", "(odd? 0)");
    try vm.expectEval("#f", "(odd? 2)");
    try vm.expectEval("#f", "(odd? -2)");
    try vm.expectEval("#f", "(odd? 42)");
}

test "odd? with floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(odd? 1.0)");
    try vm.expectEval("#t", "(odd? 3.0)");
    try vm.expectEval("#t", "(odd? -1.0)");
    try vm.expectEval("#f", "(odd? 0.0)");
    try vm.expectEval("#f", "(odd? 2.0)");
    try vm.expectEval("#f", "(odd? -2.0)");
    try vm.expectEval("#f", "(odd? 1.5)");
    try vm.expectEval("#f", "(odd? 2.5)");
}

test "odd? with rationals" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(odd? 3/1)");
    try vm.expectEval("#f", "(odd? 4/2)");
    try vm.expectEval("#f", "(odd? 1/2)");
    try vm.expectEval("#f", "(odd? 3/2)");
}

test "odd? with non-numbers returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(odd? #t)");
}

test "even? with integers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(even? 0)");
    try vm.expectEval("#t", "(even? 2)");
    try vm.expectEval("#t", "(even? -2)");
    try vm.expectEval("#t", "(even? 42)");
    try vm.expectEval("#f", "(even? 1)");
    try vm.expectEval("#f", "(even? 3)");
    try vm.expectEval("#f", "(even? -1)");
    try vm.expectEval("#f", "(even? -3)");
}

test "even? with floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(even? 0.0)");
    try vm.expectEval("#t", "(even? 2.0)");
    try vm.expectEval("#t", "(even? -2.0)");
    try vm.expectEval("#f", "(even? 1.0)");
    try vm.expectEval("#f", "(even? 3.0)");
    try vm.expectEval("#f", "(even? -1.0)");
    try vm.expectEval("#f", "(even? 1.5)");
    try vm.expectEval("#f", "(even? 2.5)");
}

test "even? with rationals" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(even? 4/2)");
    try vm.expectEval("#f", "(even? 3/1)");
    try vm.expectEval("#f", "(even? 1/2)");
    try vm.expectEval("#f", "(even? 3/2)");
}

test "even? with non-numbers returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(even? #t)");
}

test "max with integers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("4", "(max 3 4)");
    try vm.expectEval("5", "(max 3 4 5)");
    try vm.expectEval("5", "(max 5 3 4)");
    try vm.expectEval("3", "(max 3)");
    try vm.expectEval("10", "(max -5 0 10)");
    try vm.expectEval("0", "(max -10 -5 0)");
}

test "max with floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("4.5", "(max 3.5 4.5)");
    try vm.expectEval("5.5", "(max 3.5 4.5 5.5)");
    try vm.expectEval("3.5", "(max 3.5)");
    try vm.expectEval("0.5", "(max -1.5 0.5 -0.5)");
}

test "max with mixed ints and floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("4.0", "(max 3.5 4)");
    try vm.expectEval("4.0", "(max 3.9 4)");
    try vm.expectEval("5.0", "(max 3 4.5 5)");
    try vm.expectEval("5.5", "(max 3 4 5.5)");
}

test "max with rationals" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3/4", "(max 1/2 3/4)");
    try vm.expectEval("3/4", "(max 1/4 1/2 3/4)");
    try vm.expectEval("2", "(max 1/2 2)");
}

test "max with mixed types" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("2.5", "(max 1/2 2 2.5)");
    try vm.expectEval("3.0", "(max 1/2 2.5 3)");
}

test "max with no arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(max)");
}

test "max with non-numbers returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(max #t)");
    try vm.expectError(Vm.Error.UncaughtException, "(max 3 #f)");
}

test "min with integers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3", "(min 3 4)");
    try vm.expectEval("3", "(min 3 4 5)");
    try vm.expectEval("3", "(min 5 3 4)");
    try vm.expectEval("5", "(min 5)");
    try vm.expectEval("-5", "(min -5 0 10)");
    try vm.expectEval("-10", "(min -10 -5 0)");
}

test "min with floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3.5", "(min 3.5 4.5)");
    try vm.expectEval("3.5", "(min 3.5 4.5 5.5)");
    try vm.expectEval("3.5", "(min 3.5)");
    try vm.expectEval("-1.5", "(min -1.5 0.5 -0.5)");
}

test "min with mixed ints and floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3.5", "(min 3.5 4)");
    try vm.expectEval("3.9", "(min 3.9 4)");
    try vm.expectEval("3.0", "(min 3 4.5 5)");
    try vm.expectEval("3.0", "(min 3 4 5.5)");
}

test "min with rationals" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1/2", "(min 1/2 3/4)");
    try vm.expectEval("1/4", "(min 1/4 1/2 3/4)");
    try vm.expectEval("1/2", "(min 1/2 2)");
}

test "min with mixed types" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0.5", "(min 1/2 2 2.5)");
    try vm.expectEval("0.5", "(min 1/2 2.5 3)");
}

test "min with no arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(min)");
}

test "min with non-numbers returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(min #t)");
    try vm.expectError(Vm.Error.UncaughtException, "(min 3 #f)");
}

test "abs with positive integers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("7", "(abs 7)");
    try vm.expectEval("0", "(abs 0)");
    try vm.expectEval("42", "(abs 42)");
}

test "abs with negative integers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("7", "(abs -7)");
    try vm.expectEval("42", "(abs -42)");
    try vm.expectEval("1", "(abs -1)");
}

test "abs with positive floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3.5", "(abs 3.5)");
    try vm.expectEval("0.0", "(abs 0.0)");
    try vm.expectEval("42.5", "(abs 42.5)");
}

test "abs with negative floats" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3.5", "(abs -3.5)");
    try vm.expectEval("42.5", "(abs -42.5)");
    try vm.expectEval("0.5", "(abs -0.5)");
}

test "abs with positive rationals" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1/2", "(abs 1/2)");
    try vm.expectEval("3/4", "(abs 3/4)");
}

test "abs with negative rationals" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1/2", "(abs -1/2)");
    try vm.expectEval("3/4", "(abs -3/4)");
}

test "abs with non-numbers returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(abs #t)");
    try vm.expectError(Vm.Error.UncaughtException, "(abs \"string\")");
}

test "abs with no arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(abs)");
}

test "abs with multiple arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(abs 1 2)");
}
