const std = @import("std");
const testing = std.testing;
const NativeProc = @import("../types/NativeProc.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

pub const string_length = NativeProc.withRawArgs(struct {
    pub const name = "string-length";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 1) return .{ .err = error.NotImplemented };
        const inspector = vm.inspector();
        const string = inspector.asString(args[0]) catch return .{ .err = error.WrongType };
        const len: i64 = @intCast(string.asSlice().len);
        return .{ .val = Val.initInt(len) };
    }
});

test "string-length on empty string returns 0" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0", "(string-length \"\")");
}

test "string-length on normal string returns length" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("5", "(string-length \"hello\")");
    try vm.expectEval("11", "(string-length \"hello world\")");
}

test "string-length on string with escape sequences returns processed length" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("6", "(string-length \"hello\\n\")");
    try vm.expectEval("6", "(string-length \"hello\\t\")");
}

test "string-length on non-string returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.WrongType, vm.evalStr("(string-length 42)", null));
    try testing.expectError(Vm.Error.WrongType, vm.evalStr("(string-length #t)", null));
}
