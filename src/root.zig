//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const testing = std.testing;

pub const Val = @import("Val.zig");
pub const Vm = @import("Vm.zig");

test {
    testing.refAllDecls(@This());
}

test "define" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try vm.expectEval("*unspecified*", "(define x 28)");
    try vm.expectEval("28", "x");
}

test "quote" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try vm.expectEval("a", "(quote a)");
    try vm.expectEval("(+ 1 2)", "(quote (+ 1 2))");
}

test "if" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("yes", "(if (> 3 2) 'yes 'no)");
    try vm.expectEval("no", "(if (> 2 3) 'yes 'no)");
    try vm.expectEval("1", "(if (> 3 2) (- 3 2) (+ 3 2))");
}
