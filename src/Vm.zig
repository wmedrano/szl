const std = @import("std");
const testing = std.testing;

const Builder = @import("Builder.zig");
const Reader = @import("Reader.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");

const Vm = @This();

options: Options,
objects: std.ArrayList(Val) = .{},
symbols: std.StringHashMapUnmanaged(Symbol) = .{},

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
            .empty_list, .int, .symbol => {},
            .pair => |cons| self.allocator().destroy(cons),
        }
    }
    self.objects.deinit(self.options.allocator);
    var it = self.symbols.keyIterator();
    while (it.next()) |key| {
        self.allocator().free(key.*);
    }
    self.symbols.deinit(self.allocator());
}

pub fn allocator(self: Vm) std.mem.Allocator {
    return self.options.allocator;
}

pub fn builder(self: *Vm) Builder {
    return Builder.init(self);
}

test builder {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const items = [_]Val{ b.makeInt(1), b.makeInt(2), b.makeInt(3) };
    try testing.expectFmt("(1 2 3)", "{f}", .{try b.makeList(&items)});
}

pub fn read(self: *Vm, source: []const u8) Reader {
    return Reader.init(self, source);
}

test read {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = vm.read("() (1 2 3)");
    try testing.expectFmt("()", "{f}", .{(try reader.readNext()).?});
    try testing.expectFmt("(1 2 3)", "{f}", .{(try reader.readNext()).?});
    try testing.expectEqual(null, try reader.readNext());
}
