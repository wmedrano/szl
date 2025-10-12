const std = @import("std");
const testing = std.testing;

const Instruction = @import("../instruction.zig").Instruction;
const Module = @import("../types/Module.zig");
const NativeProc = @import("../types/NativeProc.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Number = Val.Number;
const Vm = @import("../Vm.zig");

/// Returns true if a < b
fn isLessThan(a: Number, b: Number) bool {
    const a_val: f64 = switch (a) {
        .int => |x| @floatFromInt(x),
        .float => |x| x,
    };
    const b_val: f64 = switch (b) {
        .int => |x| @floatFromInt(x),
        .float => |x| x,
    };
    return a_val < b_val;
}

/// Returns true if a <= b
fn isLessThanOrEqual(a: Number, b: Number) bool {
    const a_val: f64 = switch (a) {
        .int => |x| @floatFromInt(x),
        .float => |x| x,
    };
    const b_val: f64 = switch (b) {
        .int => |x| @floatFromInt(x),
        .float => |x| x,
    };
    return a_val <= b_val;
}

/// Returns true if a > b
fn isGreaterThan(a: Number, b: Number) bool {
    const a_val: f64 = switch (a) {
        .int => |x| @floatFromInt(x),
        .float => |x| x,
    };
    const b_val: f64 = switch (b) {
        .int => |x| @floatFromInt(x),
        .float => |x| x,
    };
    return a_val > b_val;
}

/// Returns true if a >= b
fn isGreaterThanOrEqual(a: Number, b: Number) bool {
    const a_val: f64 = switch (a) {
        .int => |x| @floatFromInt(x),
        .float => |x| x,
    };
    const b_val: f64 = switch (b) {
        .int => |x| @floatFromInt(x),
        .float => |x| x,
    };
    return a_val >= b_val;
}

/// Returns true if a == b
fn isEqual(a: Number, b: Number) bool {
    const a_val: f64 = switch (a) {
        .int => |x| @floatFromInt(x),
        .float => |x| x,
    };
    const b_val: f64 = switch (b) {
        .int => |x| @floatFromInt(x),
        .float => |x| x,
    };
    return a_val == b_val;
}

/// Generic comparison helper that checks if all arguments are ordered according to compareFn
fn checkOrdered(args: []const Val, comptime compare_fn: fn (Number, Number) bool) NativeProc.Result {
    const is_ordered = switch (args.len) {
        0 => true,
        1 => blk: {
            // Validate single argument is a number
            _ = args[0].asNumber() orelse return .{ .err = error.NotImplemented };
            break :blk true;
        },
        else => blk: {
            var prev = args[0].asNumber() orelse return .{ .err = error.NotImplemented };
            for (args[1..]) |v| {
                const curr = v.asNumber() orelse return .{ .err = error.NotImplemented };
                if (!compare_fn(prev, curr)) break :blk false;
                prev = curr;
            }
            break :blk true;
        },
    };
    return .{ .val = Val.initBool(is_ordered) };
}

pub const add = NativeProc.withRawArgs(struct {
    pub const name = "+";
    pub inline fn impl(_: *Vm, args: []const Val) NativeProc.Result {
        var int_sum: i64 = 0;
        var float_sum: f64 = 0.0;
        var has_float = false;
        // TODO: Raise an exception.
        for (args) |v| {
            const num = v.asNumber() orelse return NativeProc.Result{ .err = Vm.Error.NotImplemented };
            switch (num) {
                .int => |x| int_sum += x,
                .float => |x| {
                    float_sum += x;
                    has_float = true;
                },
            }
        }
        if (has_float) {
            float_sum += @floatFromInt(int_sum);
            return NativeProc.Result{ .val = Val.initFloat(float_sum) };
        }
        return NativeProc.Result{ .val = Val.initInt(int_sum) };
    }
});

pub const sub = NativeProc.withRawArgs(struct {
    pub const name = "-";
    pub inline fn impl(_: *Vm, args: []const Val) NativeProc.Result {
        switch (args.len) {
            0 => return .{ .err = error.NotImplemented },
            1 => switch (args[0].asNumber() orelse return .{ .err = error.NotImplemented }) {
                .float => |x| return .{ .val = Val.initFloat(-x) },
                .int => |x| return .{ .val = Val.initInt(-x) },
            },
            else => {},
        }

        // Get the first value and check if we have floats
        var has_float = false;
        var int_result: i64 = 0;
        var float_result: f64 = 0.0;
        const first_num = args[0].asNumber() orelse return .{ .err = error.NotImplemented };
        switch (first_num) {
            .int => |x| int_result = x,
            .float => |x| {
                float_result = x;
                has_float = true;
            },
        }

        // Multiple arguments: subtract sequentially
        for (args[1..]) |v| {
            const num = v.asNumber() orelse return .{ .err = error.NotImplemented };
            switch (num) {
                .int => |x| int_result -= x,
                .float => |x| {
                    float_result -= x;
                    has_float = true;
                },
            }
        }

        if (has_float) {
            float_result += @floatFromInt(int_result);
            return .{ .val = Val.initFloat(float_result) };
        }
        return .{ .val = Val.initInt(int_result) };
    }
});

pub const lt = NativeProc.withRawArgs(struct {
    pub const name = "<";
    pub inline fn impl(_: *Vm, args: []const Val) NativeProc.Result {
        return checkOrdered(args, isLessThan);
    }
});

pub const lte = NativeProc.withRawArgs(struct {
    pub const name = "<=";
    pub inline fn impl(_: *Vm, args: []const Val) NativeProc.Result {
        return checkOrdered(args, isLessThanOrEqual);
    }
});

pub const gt = NativeProc.withRawArgs(struct {
    pub const name = ">";
    pub inline fn impl(_: *Vm, args: []const Val) NativeProc.Result {
        return checkOrdered(args, isGreaterThan);
    }
});

pub const gte = NativeProc.withRawArgs(struct {
    pub const name = ">=";
    pub inline fn impl(_: *Vm, args: []const Val) NativeProc.Result {
        return checkOrdered(args, isGreaterThanOrEqual);
    }
});

pub const eq = NativeProc.withRawArgs(struct {
    pub const name = "=";
    pub inline fn impl(_: *Vm, args: []const Val) NativeProc.Result {
        return checkOrdered(args, isEqual);
    }
});

pub const number_p = NativeProc.withRawArgs(struct {
    pub const name = "number?";
    pub inline fn impl(_: *Vm, args: []const Val) NativeProc.Result {
        return switch (args.len) {
            1 => NativeProc.Result{ .val = Val.initBool(args[0].asNumber() != null) },
            else => NativeProc.Result{ .err = Vm.Error.UncaughtException },
        };
    }
});

pub const integer_p = NativeProc.withRawArgs(struct {
    pub const name = "integer?";
    pub inline fn impl(_: *Vm, args: []const Val) NativeProc.Result {
        return switch (args.len) {
            1 => NativeProc.Result{ .val = Val.initBool(args[0].data == .int) },
            else => NativeProc.Result{ .err = Vm.Error.UncaughtException },
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

    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(+ #t)", null));
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

    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(-)", null));
}

test "- on non-ints returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(- #t)", null));
    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(- 5 #f)", null));
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

    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(<= #t)", null));
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

    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(< #t)", null));
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

    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(> #t)", null));
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

    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(>= #t)", null));
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

    try testing.expectError(Vm.Error.NotImplemented, vm.evalStr("(= #t)", null));
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

    try testing.expectError(Vm.Error.UncaughtException, vm.evalStr("(number?)", null));
}

test "number? with multiple arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.UncaughtException, vm.evalStr("(number? 1 2)", null));
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

    try testing.expectError(Vm.Error.UncaughtException, vm.evalStr("(integer?)", null));
}

test "integer? with multiple arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.UncaughtException, vm.evalStr("(integer? 1 2)", null));
}

test "integer? with float returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(integer? 0.0)");
    try vm.expectEval("#f", "(integer? 42.5)");
    try vm.expectEval("#f", "(integer? -5.25)");
    try vm.expectEval("#f", "(integer? 1.0)");
}
