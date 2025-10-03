const std = @import("std");

const Reader = @import("Reader.zig");

const Vm = @This();

options: Options,

pub const Options = struct {
    allocator: std.mem.Allocator,
};

pub const Val = union(enum) {
    empty_list,
    int: i64,

    pub fn format(self: Val, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            .empty_list => try writer.writeAll("()"),
            .int => |n| try writer.print("{}", .{n}),
        }
    }
};

pub const Error = error{ OutOfMemory, NotImplemented, ReadError };

pub fn init(options: Options) error{OutOfMemory}!Vm {
    return Vm{ .options = options };
}

pub fn deinit(_: *Vm) void {}

pub fn allocator(self: Vm) std.mem.Allocator {
    return self.options.allocator;
}

pub fn read(self: *Vm, source: []const u8) Reader {
    return Reader.init(self, source);
}
