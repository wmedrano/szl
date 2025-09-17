//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const testing = std.testing;

pub const Val = @import("Val.zig");
pub const Vm = @import("Vm.zig");

test {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(Vm);
}
