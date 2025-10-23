const std = @import("std");

const Val = @import("Val.zig");

const Port = @This();

inner: Inner = .null,

pub const Inner = union(enum) {
    null,
    stdin,
    stdout,
    stderr,
};

pub fn deinit(self: *Port, allocator: std.mem.Allocator) void {
    _ = self;
    _ = allocator;
}

pub fn isBinary(_: Port) bool {
    return false;
}

pub fn isTextual(_: Port) bool {
    return true;
}
