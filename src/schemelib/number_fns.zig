const std = @import("std");
const testing = std.testing;

const Instruction = @import("../instruction.zig").Instruction;
const Module = @import("../types/Module.zig");
const NativeProc = @import("../types/NativeProc.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

pub const add = NativeProc.withRawArgs(struct {
    pub const name = "+";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        const inspector = vm.inspector();
        var sum: i64 = 0;
        // TODO: Raise an exception.
        for (args) |v| sum += inspector.asInt(v) catch return .{ .err = error.NotImplemented };
        return .{ .val = Val.initInt(sum) };
    }
});

pub const sub = NativeProc.withRawArgs(struct {
    pub const name = "-";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        const inspector = vm.inspector();
        // TODO: Raise an exception.
        const result: i64 = switch (args.len) {
            0 => return .{ .err = error.NotImplemented },
            1 => blk: {
                const n = inspector.asInt(args[0]) catch return .{ .err = error.NotImplemented };
                break :blk -n;
            },
            else => blk: {
                var diff = inspector.asInt(args[0]) catch return .{ .err = error.NotImplemented };
                for (args[1..]) |v| diff -= inspector.asInt(v) catch return .{ .err = error.NotImplemented };
                break :blk diff;
            },
        };
        return .{ .val = Val.initInt(result) };
    }
});

pub const lt = NativeProc.withRawArgs(struct {
    pub const name = "<";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        const inspector = vm.inspector();

        // Check if strictly ordered by comparing adjacent pairs.
        // TODO: Raise an exception instead of NotImplemented.
        const is_ordered = switch (args.len) {
            0 => true,
            1 => blk: {
                // Validate single argument is an integer
                _ = inspector.asInt(args[0]) catch return .{ .err = error.NotImplemented };
                break :blk true;
            },
            else => blk: {
                var prev = inspector.asInt(args[0]) catch return .{ .err = error.NotImplemented };
                for (args[1..]) |v| {
                    const curr = inspector.asInt(v) catch return .{ .err = error.NotImplemented };
                    if (prev >= curr) break :blk false;
                    prev = curr;
                }
                break :blk true;
            },
        };
        return .{ .val = Val.initBool(is_ordered) };
    }
});

pub const lte = NativeProc.withRawArgs(struct {
    pub const name = "<=";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        const inspector = vm.inspector();

        // Check if ordered by comparing adjacent pairs.
        // TODO: Raise an exception instead of NotImplemented.
        const is_ordered = switch (args.len) {
            0 => true,
            1 => blk: {
                // Validate single argument is an integer
                _ = inspector.asInt(args[0]) catch return .{ .err = error.NotImplemented };
                break :blk true;
            },
            else => blk: {
                var prev = inspector.asInt(args[0]) catch return .{ .err = error.NotImplemented };
                for (args[1..]) |v| {
                    const curr = inspector.asInt(v) catch return .{ .err = error.NotImplemented };
                    if (prev > curr) break :blk false;
                    prev = curr;
                }
                break :blk true;
            },
        };
        return .{ .val = Val.initBool(is_ordered) };
    }
});

pub const gt = NativeProc.withRawArgs(struct {
    pub const name = ">";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        const inspector = vm.inspector();

        // Check if strictly ordered in descending order by comparing adjacent pairs.
        // TODO: Raise an exception instead of NotImplemented.
        const is_ordered = switch (args.len) {
            0 => true,
            1 => blk: {
                // Validate single argument is an integer
                _ = inspector.asInt(args[0]) catch return .{ .err = error.NotImplemented };
                break :blk true;
            },
            else => blk: {
                var prev = inspector.asInt(args[0]) catch return .{ .err = error.NotImplemented };
                for (args[1..]) |v| {
                    const curr = inspector.asInt(v) catch return .{ .err = error.NotImplemented };
                    if (prev <= curr) break :blk false;
                    prev = curr;
                }
                break :blk true;
            },
        };
        return .{ .val = Val.initBool(is_ordered) };
    }
});

pub const number_p = NativeProc.withRawArgs(struct {
    pub const name = "number?";
    pub inline fn impl(_: *Vm, args: []const Val) NativeProc.Result {
        return switch (args.len) {
            1 => NativeProc.Result{ .val = Val.initBool(args[0].data == .int) },
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
