const std = @import("std");

const String = @This();

data: std.ArrayList(u8),

pub fn init(allocator: std.mem.Allocator, s: []const u8) error{OutOfMemory}!String {
    var data = std.ArrayList(u8){};
    try data.appendSlice(allocator, s);
    return String{ .data = data };
}

pub fn deinit(self: *String, allocator: std.mem.Allocator) void {
    self.data.deinit(allocator);
}

pub fn asSlice(self: *const String) []const u8 {
    return self.data.items;
}
