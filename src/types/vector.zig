const std = @import("std");

const Val = @import("Val.zig");

pub const Vector = std.ArrayList(Val);
pub const ByteVector = std.ArrayList(u8);
