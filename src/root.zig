//! By convention, root.zig is the root source file when making a library.
const std = @import("std");

pub const Val = @import("Vm.zig").Val;
pub const Vm = @import("Vm.zig");
