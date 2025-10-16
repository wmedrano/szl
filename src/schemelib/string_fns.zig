const std = @import("std");
const testing = std.testing;

const Diagnostics = @import("../Diagnostics.zig");
const NativeProc = @import("../types/NativeProc.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

pub const string_length = NativeProc.with1Arg(struct {
    pub const name = "string-length";
    pub const docstring =
        \\(string-length string)
        \\
        \\Returns the number of characters in string.
        \\(string-length "")           =>  0
        \\(string-length "hello")      =>  5
    ;
    pub inline fn impl(vm: *Vm, diagnostics: ?*Diagnostics, arg: Val) Vm.Error!Val {
        const inspector = vm.inspector();
        const string = inspector.asString(arg) catch {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_type = .{
                    .expected = "string",
                    .got = arg,
                    .proc = Val.initNativeProc(&string_length),
                    .arg_name = "string",
                    .arg_position = 0,
                } });
            }
            return Vm.Error.UncaughtException;
        };
        const len: i64 = @intCast(string.asSlice().len);
        return Val.initInt(len);
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

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(string-length 42)", null, null),
    );
    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(string-length #t)", null, null),
    );
}
