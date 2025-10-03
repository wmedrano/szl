const std = @import("std");

const Builder = @import("Builder.zig");
const Reader = @import("Reader.zig");
const Val = @import("Val.zig");

const Vm = @This();

options: Options,
objects: std.ArrayList(Val) = .{},

pub const Options = struct {
    allocator: std.mem.Allocator,
};

pub const Error = error{ OutOfMemory, NotImplemented, ReadError };

pub fn init(options: Options) error{OutOfMemory}!Vm {
    return Vm{ .options = options };
}

pub fn deinit(self: *Vm) void {
    for (self.objects.items) |val| {
        switch (val.data) {
            .empty_list, .int => {},
            .pair => |cons| self.allocator().destroy(cons),
        }
    }
    self.objects.deinit(self.options.allocator);
}

pub fn allocator(self: Vm) std.mem.Allocator {
    return self.options.allocator;
}

pub fn builder(self: *Vm) Builder {
    return Builder.init(self);
}

pub fn read(self: *Vm, source: []const u8) Reader {
    return Reader.init(self, source);
}
