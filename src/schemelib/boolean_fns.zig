const std = @import("std");
const testing = std.testing;

const ErrorDetails = @import("../types/ErrorDetails.zig");
const NativeProc = @import("../types/NativeProc.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

pub const not = NativeProc.with1Arg(struct {
    pub const name = "not";
    pub const docstring =
        \\(not obj)
        \\
        \\Returns #t if obj is false, #f otherwise.
        \\(not #t)    =>  #f
        \\(not #f)    =>  #t
        \\(not 'foo)  =>  #f
    ;
    pub inline fn impl(_: *Vm, _: *ErrorDetails, arg: Val) Vm.Error!Val {
        return Val.initBool(!arg.isTruthy());
    }
});

pub const boolean_p = NativeProc.with1Arg(struct {
    pub const name = "boolean?";
    pub const docstring =
        \\(boolean? obj)
        \\
        \\Returns #t if obj is a boolean, #f otherwise.
        \\(boolean? #t)    =>  #t
        \\(boolean? #f)    =>  #t
        \\(boolean? 0)     =>  #f
    ;
    pub inline fn impl(_: *Vm, _: *ErrorDetails, arg: Val) Vm.Error!Val {
        return Val.initBool(arg.asBool() != null);
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

    try vm.expectError(Vm.Error.UncaughtException, "(not)");
}

test "not with multiple arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(not #t #f)");
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

    try vm.expectError(Vm.Error.UncaughtException, "(boolean?)");
}

test "boolean? with multiple arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(boolean? #t #f)");
}
