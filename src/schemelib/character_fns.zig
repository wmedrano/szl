const std = @import("std");
const testing = std.testing;

const ErrorDetails = @import("../types/ErrorDetails.zig");
const NativeProc = @import("../types/NativeProc.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

/// Returns true if a == b
fn isEqual(a: u21, b: u21) bool {
    return a == b;
}

/// Returns true if a < b
fn isLessThan(a: u21, b: u21) bool {
    return a < b;
}

/// Returns true if a <= b
fn isLessThanOrEqual(a: u21, b: u21) bool {
    return a <= b;
}

/// Returns true if a > b
fn isGreaterThan(a: u21, b: u21) bool {
    return a > b;
}

/// Returns true if a >= b
fn isGreaterThanOrEqual(a: u21, b: u21) bool {
    return a >= b;
}

/// Returns true if a == b (case-insensitive)
fn isEqualCaseInsensitive(a: u21, b: u21) bool {
    const a_lower = if (a < 128) std.ascii.toLower(@intCast(a)) else a;
    const b_lower = if (b < 128) std.ascii.toLower(@intCast(b)) else b;
    return a_lower == b_lower;
}

/// Returns true if a < b (case-insensitive)
fn isLessThanCaseInsensitive(a: u21, b: u21) bool {
    const a_lower = if (a < 128) std.ascii.toLower(@intCast(a)) else a;
    const b_lower = if (b < 128) std.ascii.toLower(@intCast(b)) else b;
    return a_lower < b_lower;
}

/// Returns true if a <= b (case-insensitive)
fn isLessThanOrEqualCaseInsensitive(a: u21, b: u21) bool {
    const a_lower = if (a < 128) std.ascii.toLower(@intCast(a)) else a;
    const b_lower = if (b < 128) std.ascii.toLower(@intCast(b)) else b;
    return a_lower <= b_lower;
}

/// Returns true if a > b (case-insensitive)
fn isGreaterThanCaseInsensitive(a: u21, b: u21) bool {
    const a_lower = if (a < 128) std.ascii.toLower(@intCast(a)) else a;
    const b_lower = if (b < 128) std.ascii.toLower(@intCast(b)) else b;
    return a_lower > b_lower;
}

/// Returns true if a >= b (case-insensitive)
fn isGreaterThanOrEqualCaseInsensitive(a: u21, b: u21) bool {
    const a_lower = if (a < 128) std.ascii.toLower(@intCast(a)) else a;
    const b_lower = if (b < 128) std.ascii.toLower(@intCast(b)) else b;
    return a_lower >= b_lower;
}

/// Generic comparison helper that checks if all arguments are ordered according to compareFn
fn checkOrdered(vm: *Vm, args: []const Val, comptime compare_fn: fn (u21, u21) bool, proc: *const NativeProc, diagnostics: *ErrorDetails) Vm.Error!Val {
    const is_ordered = switch (args.len) {
        0 => true,
        1 => blk: {
            // Validate single argument is a character
            _ = args[0].asChar() orelse {
                @branchHint(.cold);
                diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "char",
                    .got = args[0],
                    .proc = Val.initNativeProc(proc),
                    .arg_name = null,
                    .arg_position = 0,
                } });
                return Vm.Error.UncaughtException;
            };
            break :blk true;
        },
        else => blk: {
            var prev = args[0].asChar() orelse {
                @branchHint(.cold);
                diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "char",
                    .got = args[0],
                    .proc = Val.initNativeProc(proc),
                    .arg_name = null,
                    .arg_position = 0,
                } });
                return Vm.Error.UncaughtException;
            };
            for (args[1..], 1..) |v, idx| {
                const curr = v.asChar() orelse {
                    @branchHint(.cold);
                    diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                        .expected = "char",
                        .got = v,
                        .proc = Val.initNativeProc(proc),
                        .arg_name = null,
                        .arg_position = @intCast(idx),
                    } });
                    return Vm.Error.UncaughtException;
                };
                if (!compare_fn(prev, curr)) break :blk false;
                prev = curr;
            }
            break :blk true;
        },
    };
    return Val.initBool(is_ordered);
}

pub const eq = NativeProc.withRawArgs(struct {
    pub const name = "char=?";
    pub const docstring =
        \\(char=? char1 char2 ...)
        \\
        \\Returns #t if all characters are equal.
        \\(char=? #\a #\a)  =>  #t
        \\(char=? #\a #\b)  =>  #f
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        return checkOrdered(vm, args, isEqual, &eq, diagnostics);
    }
});

pub const lt = NativeProc.withRawArgs(struct {
    pub const name = "char<?";
    pub const docstring =
        \\(char<? char1 char2 ...)
        \\
        \\Returns #t if characters are monotonically increasing.
        \\(char<? #\a #\b #\c)  =>  #t
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        return checkOrdered(vm, args, isLessThan, &lt, diagnostics);
    }
});

pub const lte = NativeProc.withRawArgs(struct {
    pub const name = "char<=?";
    pub const docstring =
        \\(char<=? char1 char2 ...)
        \\
        \\Returns #t if characters are monotonically non-decreasing.
        \\(char<=? #\a #\a #\b)  =>  #t
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        return checkOrdered(vm, args, isLessThanOrEqual, &lte, diagnostics);
    }
});

pub const gt = NativeProc.withRawArgs(struct {
    pub const name = "char>?";
    pub const docstring =
        \\(char>? char1 char2 ...)
        \\
        \\Returns #t if characters are monotonically decreasing.
        \\(char>? #\c #\b #\a)  =>  #t
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        return checkOrdered(vm, args, isGreaterThan, &gt, diagnostics);
    }
});

pub const gte = NativeProc.withRawArgs(struct {
    pub const name = "char>=?";
    pub const docstring =
        \\(char>=? char1 char2 ...)
        \\
        \\Returns #t if characters are monotonically non-increasing.
        \\(char>=? #\c #\b #\b)  =>  #t
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        return checkOrdered(vm, args, isGreaterThanOrEqual, &gte, diagnostics);
    }
});

pub const ci_eq = NativeProc.withRawArgs(struct {
    pub const name = "char-ci=?";
    pub const docstring =
        \\(char-ci=? char1 char2 ...)
        \\
        \\Returns #t if all characters are equal (case-insensitive).
        \\(char-ci=? #\a #\A)  =>  #t
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        return checkOrdered(vm, args, isEqualCaseInsensitive, &ci_eq, diagnostics);
    }
});

pub const ci_lt = NativeProc.withRawArgs(struct {
    pub const name = "char-ci<?";
    pub const docstring =
        \\(char-ci<? char1 char2 ...)
        \\
        \\Returns #t if characters are monotonically increasing (case-insensitive).
        \\(char-ci<? #\a #\B)  =>  #t
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        return checkOrdered(vm, args, isLessThanCaseInsensitive, &ci_lt, diagnostics);
    }
});

pub const ci_lte = NativeProc.withRawArgs(struct {
    pub const name = "char-ci<=?";
    pub const docstring =
        \\(char-ci<=? char1 char2 ...)
        \\
        \\Returns #t if characters are monotonically non-decreasing (case-insensitive).
        \\(char-ci<=? #\a #\A #\B)  =>  #t
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        return checkOrdered(vm, args, isLessThanOrEqualCaseInsensitive, &ci_lte, diagnostics);
    }
});

pub const ci_gt = NativeProc.withRawArgs(struct {
    pub const name = "char-ci>?";
    pub const docstring =
        \\(char-ci>? char1 char2 ...)
        \\
        \\Returns #t if characters are monotonically decreasing (case-insensitive).
        \\(char-ci>? #\C #\b #\a)  =>  #t
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        return checkOrdered(vm, args, isGreaterThanCaseInsensitive, &ci_gt, diagnostics);
    }
});

pub const ci_gte = NativeProc.withRawArgs(struct {
    pub const name = "char-ci>=?";
    pub const docstring =
        \\(char-ci>=? char1 char2 ...)
        \\
        \\Returns #t if characters are monotonically non-increasing (case-insensitive).
        \\(char-ci>=? #\C #\B #\b)  =>  #t
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []const Val) Vm.Error!Val {
        return checkOrdered(vm, args, isGreaterThanOrEqualCaseInsensitive, &ci_gte, diagnostics);
    }
});

pub const char_p = NativeProc.with1Arg(struct {
    pub const name = "char?";
    pub const docstring =
        \\(char? obj)
        \\
        \\Returns #t if obj is a character, #f otherwise.
        \\(char? #\a)  =>  #t
        \\(char? 97)   =>  #f
    ;
    pub inline fn impl(_: *Vm, _: *ErrorDetails, arg: Val) Vm.Error!Val {
        return Val.initBool(arg.asChar() != null);
    }
});

pub const char_alphabetic_p = NativeProc.with1Arg(struct {
    pub const name = "char-alphabetic?";
    pub const docstring =
        \\(char-alphabetic? char)
        \\
        \\Returns #t if char is an alphabetic character.
        \\(char-alphabetic? #\a)      =>  #t
        \\(char-alphabetic? #\0)      =>  #f
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const c = arg.asChar() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "char",
                    .got = arg,
                    .proc = Val.initNativeProc(&char_alphabetic_p),
                    .arg_name = "char",
                    .arg_position = 0,
                } });
            return Vm.Error.UncaughtException;
        };
        const is_alpha = if (c < 128)
            std.ascii.isAlphabetic(@intCast(c))
        else
            // For Unicode, consider letters as non-control characters in letter ranges
            // This is a simplified implementation
            (c >= 0xC0 and c <= 0x2AF) or (c >= 0x370 and c <= 0x52F);
        return Val.initBool(is_alpha);
    }
});

pub const char_numeric_p = NativeProc.with1Arg(struct {
    pub const name = "char-numeric?";
    pub const docstring =
        \\(char-numeric? char)
        \\
        \\Returns #t if char is a numeric digit character.
        \\(char-numeric? #\5)    =>  #t
        \\(char-numeric? #\a)    =>  #f
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const c = arg.asChar() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "char",
                    .got = arg,
                    .proc = Val.initNativeProc(&char_numeric_p),
                    .arg_name = "char",
                    .arg_position = 0,
                } });
            return Vm.Error.UncaughtException;
        };
        const is_numeric = if (c < 128)
            std.ascii.isDigit(@intCast(c))
        else
            false; // Simplified: only ASCII digits for now
        return Val.initBool(is_numeric);
    }
});

pub const char_whitespace_p = NativeProc.with1Arg(struct {
    pub const name = "char-whitespace?";
    pub const docstring =
        \\(char-whitespace? char)
        \\
        \\Returns #t if char is a whitespace character.
        \\(char-whitespace? #\space)    =>  #t
        \\(char-whitespace? #\a)        =>  #f
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const c = arg.asChar() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "char",
                    .got = arg,
                    .proc = Val.initNativeProc(&char_whitespace_p),
                    .arg_name = "char",
                    .arg_position = 0,
                } });
            return Vm.Error.UncaughtException;
        };

        const is_whitespace = if (c < 128)
            std.ascii.isWhitespace(@intCast(c))
        else
            // Unicode whitespace characters
            c == 0x00A0 or // NO-BREAK SPACE
                c == 0x1680 or // OGHAM SPACE MARK
                (c >= 0x2000 and c <= 0x200A) or // Various spaces
                c == 0x202F or // NARROW NO-BREAK SPACE
                c == 0x205F or // MEDIUM MATHEMATICAL SPACE
                c == 0x3000; // IDEOGRAPHIC SPACE

        return Val.initBool(is_whitespace);
    }
});

pub const char_upper_case_p = NativeProc.with1Arg(struct {
    pub const name = "char-upper-case?";
    pub const docstring =
        \\(char-upper-case? char)
        \\
        \\Returns #t if char is an uppercase letter.
        \\(char-upper-case? #\A)    =>  #t
        \\(char-upper-case? #\a)    =>  #f
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const c = arg.asChar() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "char",
                    .got = arg,
                    .proc = Val.initNativeProc(&char_upper_case_p),
                    .arg_name = "char",
                    .arg_position = 0,
                } });
            return Vm.Error.UncaughtException;
        };

        const is_upper = if (c < 128)
            std.ascii.isUpper(@intCast(c))
        else
            false; // Simplified: only ASCII for case

        return Val.initBool(is_upper);
    }
});

pub const char_lower_case_p = NativeProc.with1Arg(struct {
    pub const name = "char-lower-case?";
    pub const docstring =
        \\(char-lower-case? char)
        \\
        \\Returns #t if char is a lowercase letter.
        \\(char-lower-case? #\a)    =>  #t
        \\(char-lower-case? #\A)    =>  #f
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const c = arg.asChar() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "char",
                    .got = arg,
                    .proc = Val.initNativeProc(&char_lower_case_p),
                    .arg_name = "char",
                    .arg_position = 0,
                } });
            return Vm.Error.UncaughtException;
        };

        const is_lower = if (c < 128)
            std.ascii.isLower(@intCast(c))
        else
            false; // Simplified: only ASCII for case

        return Val.initBool(is_lower);
    }
});
pub const digit_value = NativeProc.with1Arg(struct {
    pub const name = "digit-value";
    pub const docstring =
        \\(digit-value char)
        \\
        \\Returns the numeric value of a digit character, or #f if not a digit.
        \\(digit-value #\5)    =>  5
        \\(digit-value #\a)    =>  #f
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const c = arg.asChar() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "char",
                    .got = arg,
                    .proc = Val.initNativeProc(&digit_value),
                    .arg_name = "char",
                    .arg_position = 0,
                } });
            return Vm.Error.UncaughtException;
        };

        if (c >= '0' and c <= '9') {
            const value: i64 = @intCast(c - '0');
            return Val.initInt(value);
        }
        return Val.initBool(false); // R7RS returns #f for non-digits
    }
});

pub const char_to_integer = NativeProc.with1Arg(struct {
    pub const name = "char->integer";
    pub const docstring =
        \\(char->integer char)
        \\
        \\Returns the Unicode code point of char as an integer.
        \\(char->integer #\a)    =>  97
        \\(char->integer #\A)    =>  65
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const c = arg.asChar() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "char",
                    .got = arg,
                    .proc = Val.initNativeProc(&char_to_integer),
                    .arg_name = "char",
                    .arg_position = 0,
                } });
            return Vm.Error.UncaughtException;
        };
        const code: i64 = @intCast(c);
        return Val.initInt(code);
    }
});

pub const integer_to_char = NativeProc.with1Arg(struct {
    pub const name = "integer->char";
    pub const docstring =
        \\(integer->char n)
        \\
        \\Returns the character with Unicode code point n.
        \\(integer->char 97)    =>  #\a
        \\(integer->char 65)    =>  #\A
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const n = arg.asInt() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "integer",
                    .got = arg,
                    .proc = Val.initNativeProc(&integer_to_char),
                    .arg_name = "n",
                    .arg_position = 0,
                } });
            return Vm.Error.UncaughtException;
        };

        // Validate Unicode range
        if (n < 0 or n > 0x10FFFF) {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .other = "integer->char: argument must be in Unicode range [0, 0x10FFFF]" });
            return Vm.Error.UncaughtException;
        }
        const c: u21 = @intCast(n);
        return Val.initChar(c);
    }
});

pub const char_upcase = NativeProc.with1Arg(struct {
    pub const name = "char-upcase";
    pub const docstring =
        \\(char-upcase char)
        \\
        \\Returns the uppercase version of char.
        \\(char-upcase #\a)    =>  #\A
        \\(char-upcase #\A)    =>  #\A
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const c = arg.asChar() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "char",
                    .got = arg,
                    .proc = Val.initNativeProc(&char_upcase),
                    .arg_name = "char",
                    .arg_position = 0,
                } });
            return Vm.Error.UncaughtException;
        };

        const upper = if (c < 128)
            std.ascii.toUpper(@intCast(c))
        else
            c; // No case conversion for non-ASCII
        return Val.initChar(upper);
    }
});

pub const char_downcase = NativeProc.with1Arg(struct {
    pub const name = "char-downcase";
    pub const docstring =
        \\(char-downcase char)
        \\
        \\Returns the lowercase version of char.
        \\(char-downcase #\A)    =>  #\a
        \\(char-downcase #\a)    =>  #\a
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const c = arg.asChar() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "char",
                    .got = arg,
                    .proc = Val.initNativeProc(&char_downcase),
                    .arg_name = "char",
                    .arg_position = 0,
                } });
            return Vm.Error.UncaughtException;
        };

        const lower = if (c < 128)
            std.ascii.toLower(@intCast(c))
        else
            c; // No case conversion for non-ASCII
        return Val.initChar(lower);
    }
});

pub const char_foldcase = NativeProc.with1Arg(struct {
    pub const name = "char-foldcase";
    pub const docstring =
        \\(char-foldcase char)
        \\
        \\Returns the case-folded version of char (for case-insensitive comparison).
        \\(char-foldcase #\A)    =>  #\a
        \\(char-foldcase #\a)    =>  #\a
    ;
    pub inline fn impl(vm: *Vm, diagnostics: *ErrorDetails, arg: Val) Vm.Error!Val {
        const c = arg.asChar() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "char",
                    .got = arg,
                    .proc = Val.initNativeProc(&char_foldcase),
                    .arg_name = "char",
                    .arg_position = 0,
                } });
            return Vm.Error.UncaughtException;
        };

        // For ASCII, foldcase is the same as downcase
        const folded = if (c < 128)
            std.ascii.toLower(@intCast(c))
        else
            c; // No case folding for non-ASCII in this simple implementation
        return Val.initChar(folded);
    }
});
test "char? with character returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char? #\\a)");
    try vm.expectEval("#t", "(char? #\\Z)");
    try vm.expectEval("#t", "(char? #\\space)");
}

test "char? with non-character returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(char? 42)");
    try vm.expectEval("#f", "(char? #t)");
    try vm.expectEval("#f", "(char? \"a\")");
}

test "char? with wrong argument count returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(char?)");
    try vm.expectError(Vm.Error.UncaughtException, "(char? #\\a #\\b)");
}

test "char=? on equal characters returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char=? #\\a #\\a)");
    try vm.expectEval("#t", "(char=? #\\Z #\\Z #\\Z)");
}

test "char=? on non-equal characters returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(char=? #\\a #\\b)");
    try vm.expectEval("#f", "(char=? #\\A #\\a)");
    try vm.expectEval("#f", "(char=? #\\a #\\a #\\b)");
}

test "char=? with less than 2 args returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char=?)");
    try vm.expectEval("#t", "(char=? #\\a)");
}

test "char<? on ordered characters returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char<? #\\a #\\b #\\c)");
    try vm.expectEval("#t", "(char<? #\\A #\\Z)");
}

test "char<? on non-ordered characters returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(char<? #\\c #\\b #\\a)");
    try vm.expectEval("#f", "(char<? #\\a #\\a)");
}

test "char<=? on ordered characters returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char<=? #\\a #\\b #\\c)");
    try vm.expectEval("#t", "(char<=? #\\a #\\a #\\b)");
}

test "char>? on descending characters returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char>? #\\c #\\b #\\a)");
    try vm.expectEval("#t", "(char>? #\\Z #\\A)");
}

test "char>? on non-descending characters returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(char>? #\\a #\\b)");
    try vm.expectEval("#f", "(char>? #\\a #\\a)");
}

test "char>=? on descending characters returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char>=? #\\c #\\b #\\a)");
    try vm.expectEval("#t", "(char>=? #\\b #\\b #\\a)");
}

test "char-ci=? case-insensitive equal returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char-ci=? #\\a #\\A)");
    try vm.expectEval("#t", "(char-ci=? #\\Z #\\z)");
    try vm.expectEval("#t", "(char-ci=? #\\a #\\A #\\a)");
}

test "char-ci=? case-insensitive non-equal returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(char-ci=? #\\a #\\B)");
    try vm.expectEval("#f", "(char-ci=? #\\A #\\b #\\c)");
}

test "char-ci<? case-insensitive comparison" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char-ci<? #\\a #\\B)");
    try vm.expectEval("#t", "(char-ci<? #\\A #\\b #\\C)");
    try vm.expectEval("#f", "(char-ci<? #\\b #\\A)");
}

test "char-alphabetic? returns true for letters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char-alphabetic? #\\a)");
    try vm.expectEval("#t", "(char-alphabetic? #\\Z)");
}

test "char-alphabetic? returns false for non-letters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(char-alphabetic? #\\0)");
    try vm.expectEval("#f", "(char-alphabetic? #\\space)");
    try vm.expectEval("#f", "(char-alphabetic? #\\!)");
}

test "char-numeric? returns true for digits" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char-numeric? #\\0)");
    try vm.expectEval("#t", "(char-numeric? #\\5)");
    try vm.expectEval("#t", "(char-numeric? #\\9)");
}

test "char-numeric? returns false for non-digits" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(char-numeric? #\\a)");
    try vm.expectEval("#f", "(char-numeric? #\\space)");
}

test "char-whitespace? returns true for whitespace" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char-whitespace? #\\space)");
    try vm.expectEval("#t", "(char-whitespace? #\\tab)");
    try vm.expectEval("#t", "(char-whitespace? #\\newline)");
}

test "char-whitespace? returns false for non-whitespace" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(char-whitespace? #\\a)");
    try vm.expectEval("#f", "(char-whitespace? #\\0)");
}

test "char-upper-case? returns true for uppercase" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char-upper-case? #\\A)");
    try vm.expectEval("#t", "(char-upper-case? #\\Z)");
}

test "char-upper-case? returns false for non-uppercase" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(char-upper-case? #\\a)");
    try vm.expectEval("#f", "(char-upper-case? #\\0)");
}

test "char-lower-case? returns true for lowercase" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char-lower-case? #\\a)");
    try vm.expectEval("#t", "(char-lower-case? #\\z)");
}

test "char-lower-case? returns false for non-lowercase" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(char-lower-case? #\\A)");
    try vm.expectEval("#f", "(char-lower-case? #\\0)");
}

test "digit-value returns correct value for digits" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0", "(digit-value #\\0)");
    try vm.expectEval("5", "(digit-value #\\5)");
    try vm.expectEval("9", "(digit-value #\\9)");
}

test "digit-value returns false for non-digits" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(digit-value #\\a)");
    try vm.expectEval("#f", "(digit-value #\\space)");
}

test "char->integer returns Unicode code point" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("97", "(char->integer #\\a)");
    try vm.expectEval("65", "(char->integer #\\A)");
    try vm.expectEval("32", "(char->integer #\\space)");
}

test "integer->char creates character from code point" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#\\a", "(integer->char 97)");
    try vm.expectEval("#\\A", "(integer->char 65)");
    try vm.expectEval("#\\space", "(integer->char 32)");
}

test "integer->char rejects invalid code points" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(integer->char -1)");
    try vm.expectError(Vm.Error.UncaughtException, "(integer->char 1114112)");
}

test "char->integer and integer->char are inverses" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#\\a", "(integer->char (char->integer #\\a))");
    try vm.expectEval("97", "(char->integer (integer->char 97))");
}

test "char-upcase converts lowercase to uppercase" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#\\A", "(char-upcase #\\a)");
    try vm.expectEval("#\\Z", "(char-upcase #\\z)");
    try vm.expectEval("#\\A", "(char-upcase #\\A)");
}

test "char-upcase preserves non-letters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#\\0", "(char-upcase #\\0)");
    try vm.expectEval("#\\space", "(char-upcase #\\space)");
}

test "char-downcase converts uppercase to lowercase" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#\\a", "(char-downcase #\\A)");
    try vm.expectEval("#\\z", "(char-downcase #\\Z)");
    try vm.expectEval("#\\a", "(char-downcase #\\a)");
}

test "char-downcase preserves non-letters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#\\0", "(char-downcase #\\0)");
    try vm.expectEval("#\\space", "(char-downcase #\\space)");
}

test "char-foldcase performs case folding" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#\\a", "(char-foldcase #\\A)");
    try vm.expectEval("#\\a", "(char-foldcase #\\a)");
    try vm.expectEval("#\\z", "(char-foldcase #\\Z)");
}
