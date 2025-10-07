const std = @import("std");

const Val = @import("Val.zig");

const Vector = @This();

data: std.ArrayList(Val) = .{},

pub fn deinit(self: *Vector, allocator: std.mem.Allocator) void {
    self.data.deinit(allocator);
}

pub fn asSlice(self: *Vector) []Val {
    return self.data.items;
}
