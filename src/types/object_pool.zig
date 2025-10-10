const std = @import("std");
const testing = std.testing;

pub fn Handle(comptime T: type) type {
    return struct {
        const Object = T;
        id: u32,
    };
}

pub fn ObjectPool(comptime T: type) type {
    return struct {
        const Self = @This();
        objects: std.ArrayList(T) = .{},

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.objects.deinit(allocator);
        }

        pub fn put(self: *Self, allocator: std.mem.Allocator, obj: T) !Handle(T) {
            const handle = Handle(T){ .id = @intCast(self.objects.items.len) };
            try self.objects.append(allocator, obj);
            return handle;
        }

        pub fn get(self: Self, handle: Handle(T)) ?*T {
            const idx: usize = @intCast(handle.id);
            if (idx >= self.objects.items.len) return null;
            return &self.objects.items[idx];
        }

        pub const ObjectIterator = struct {
            pool: *const Self,
            index: usize = 0,

            pub fn next(self: *ObjectIterator) ?struct { handle: Handle(T), value: *T } {
                if (self.index >= self.pool.objects.items.len) return null;
                const obj = &self.pool.objects.items[self.index];
                const handle = Handle(T){ .id = @intCast(self.index) };
                self.index += 1;
                return .{ .handle = handle, .value = obj };
            }
        };

        pub fn iterator(self: *const Self) ObjectIterator {
            return ObjectIterator{ .pool = self };
        }

        pub fn applyAll(self: Self, closure: anytype) void {
            for (self.objects.items) |*obj| {
                closure.apply(obj);
            }
        }
    };
}
