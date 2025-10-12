const std = @import("std");
const testing = std.testing;

const NativeProc = @import("../types/NativeProc.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

pub const not = NativeProc.withRawArgs(struct {
    pub const name = "not";
    pub const docstring =
        \\(not obj)
        \\
        \\Returns #t if obj is false, #f otherwise.
        \\(not #t)    =>  #f
        \\(not #f)    =>  #t
        \\(not 'foo)  =>  #f
    ;
    pub inline fn impl(_: *Vm, args: []const Val) NativeProc.Result {
        return switch (args.len) {
            1 => NativeProc.Result{ .val = Val.initBool(!args[0].isTruthy()) },
            else => NativeProc.Result{ .err = Vm.Error.UncaughtException },
        };
    }
});

pub const boolean_p = NativeProc.withRawArgs(struct {
    pub const name = "boolean?";
    pub const docstring =
        \\(boolean? obj)
        \\
        \\Returns #t if obj is a boolean, #f otherwise.
        \\(boolean? #t)    =>  #t
        \\(boolean? #f)    =>  #t
        \\(boolean? 0)     =>  #f
    ;
    pub inline fn impl(_: *Vm, args: []const Val) NativeProc.Result {
        return switch (args.len) {
            1 => NativeProc.Result{ .val = Val.initBool(args[0].asBool() != null) },
            else => NativeProc.Result{ .err = Vm.Error.UncaughtException },
        };
    }
});

test "not with true returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(not #t)");
}

test "not with false returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(not #f)");
}

test "not with truthy value returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(not 1)");
    try vm.expectEval("#f", "(not 'symbol)");
    try vm.expectEval("#f", "(not \"string\")");
}

test "not with no arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.UncaughtException, vm.evalStr("(not)", null));
}

test "not with multiple arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.UncaughtException, vm.evalStr("(not #t #f)", null));
}

test "boolean? with true returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(boolean? #t)");
}

test "boolean? with false returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(boolean? #f)");
}

test "boolean? with non-boolean values returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(boolean? 0)");
    try vm.expectEval("#f", "(boolean? 1)");
    try vm.expectEval("#f", "(boolean? 'symbol)");
    try vm.expectEval("#f", "(boolean? \"string\")");
    try vm.expectEval("#f", "(boolean? '())");
}

test "boolean? with no arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.UncaughtException, vm.evalStr("(boolean?)", null));
}

test "boolean? with multiple arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.UncaughtException, vm.evalStr("(boolean? #t #f)", null));
}
