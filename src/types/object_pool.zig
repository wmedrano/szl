const std = @import("std");
const testing = std.testing;

pub fn Handle(comptime T: type) type {
    return packed struct {
        const Object = T;
        id: u32,
    };
}

pub fn ObjectPool(comptime T: type) type {
    return struct {
        const Self = @This();
        objects: std.ArrayList(T) = .{},
        alive: std.bit_set.DynamicBitSetUnmanaged = .{},
        freelist: std.ArrayList(Handle(T)) = .{},

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.objects.deinit(allocator);
            self.alive.deinit(allocator);
            self.freelist.deinit(allocator);
        }

        pub fn put(self: *Self, allocator: std.mem.Allocator, obj: T) !Handle(T) {
            if (self.freelist.pop()) |handle| {
                const index: usize = @intCast(handle.id);
                self.alive.set(index);
                self.objects.items[index] = obj;
                return handle;
            }
            const handle = Handle(T){ .id = @intCast(self.objects.items.len) };
            try self.objects.append(allocator, obj);
            try self.alive.resize(allocator, self.objects.items.len, true);
            return handle;
        }

        pub fn get(self: Self, handle: Handle(T)) ?*T {
            const idx: usize = @intCast(handle.id);
            if (idx >= self.objects.items.len) return null;
            if (!self.alive.isSet(idx)) return null;
            return &self.objects.items[idx];
        }

        pub const ObjectIterator = struct {
            pool: *const Self,
            index: usize = 0,

            pub fn next(self: *ObjectIterator) ?struct { handle: Handle(T), value: *T } {
                while (self.index < self.pool.objects.items.len) {
                    const index = self.index;
                    self.index += 1;
                    if (self.pool.alive.isSet(index)) {
                        const obj = &self.pool.objects.items[index];
                        const handle = Handle(T){ .id = @intCast(index) };
                        return .{ .handle = handle, .value = obj };
                    }
                }
                return null;
            }
        };

        pub fn iterator(self: *const Self) ObjectIterator {
            return ObjectIterator{ .pool = self };
        }

        pub fn applyAll(self: Self, closure: anytype) void {
            for (0..self.objects.items.len, self.objects.items) |idx, *obj| {
                if (self.alive.isSet(idx)) closure.apply(obj);
            }
        }

        pub fn removeAll(self: *Self, allocator: std.mem.Allocator, pred: anytype) error{OutOfMemory}!void {
            for (0..self.objects.items.len, self.objects.items) |idx, *obj| {
                if (!self.alive.isSet(idx)) continue;
                const handle = Handle(T){ .id = @intCast(idx) };
                const remove = pred.remove(handle, obj);
                if (!remove) continue;
                self.alive.unset(idx);
                try self.freelist.append(allocator, handle);
            }
        }
    };
}
