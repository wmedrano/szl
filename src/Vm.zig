const std = @import("std");

const Vm = @This();

options: Options,

pub const Options = struct {
    allocator: std.mem.Allocator,
};

pub const Val = union(enum) {
    empty_list,

    pub fn format(self: Val, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            .empty_list => try writer.writeAll("()"),
        }
    }
};

pub const Error = error{ OutOfMemory, NotImplemented };

pub fn init(options: Options) error{OutOfMemory}!Vm {
    return Vm{ .options = options };
}

pub fn deinit(_: *Vm) void {}

pub fn read(_: *Vm, _: []const u8) Error!Val {
    return Error.NotImplemented;
}
