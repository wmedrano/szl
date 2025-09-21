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
    try testing.expectFmt(
        "()",
        "{f}",
        .{vm.pretty(try vm.evalStr("(define x 28)"))},
    );
    try testing.expectFmt(
        "28",
        "{f}",
        .{vm.pretty(try vm.evalStr("x"))},
    );
}

test "quote" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try testing.expectFmt(
        "a",
        "{f}",
        .{vm.pretty(try vm.evalStr("(quote a)"))},
    );
    try testing.expectFmt(
        "(+ 1 2)",
        "{f}",
        .{vm.pretty(try vm.evalStr("(quote (+ 1 2))"))},
    );
}

test "if" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectFmt(
        "yes",
        "{f}",
        .{vm.pretty(try vm.evalStr("(if (> 3 2) 'yes 'no)"))},
    );
    try testing.expectFmt(
        "no",
        "{f}",
        .{vm.pretty(try vm.evalStr("(if (> 2 3) 'yes 'no)"))},
    );
    try testing.expectFmt(
        "1",
        "{f}",
        .{vm.pretty(try vm.evalStr("(if (> 3 2) (- 3 2) (+ 3 2))"))},
    );
}
