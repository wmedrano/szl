const std = @import("std");

const Slot = @This();

index: u32,

pub fn idx(self: Slot) usize {
    return @intCast(self.index);
}
