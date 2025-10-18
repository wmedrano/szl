const std = @import("std");
const testing = std.testing;

pub fn SmallArrayList(T: type, inline_size: usize) type {
    return struct {
        const Self = @This();

        len: usize = 0,
        data: union(enum) {
            small: [inline_size]T,
            heap: struct { ptr: [*]T, capacity: usize },
        } = .{
            .small = undefined,
        },

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            switch (self.data) {
                .small => {},
                .heap => |heap| {
                    allocator.free(heap.ptr[0..heap.capacity]);
                },
            }
            self.* = undefined;
        }

        pub fn asSlice(self: *Self) []T {
            return switch (self.data) {
                .small => |*small| small[0..self.len],
                .heap => |heap| heap.ptr[0..self.len],
            };
        }

        pub fn asSliceConst(self: *const Self) []const T {
            return switch (self.data) {
                .small => |*small| small[0..self.len],
                .heap => |heap| heap.ptr[0..self.len],
            };
        }

        pub fn append(self: *Self, allocator: std.mem.Allocator, item: T) error{OutOfMemory}!void {
            switch (self.data) {
                .small => |*small| {
                    if (self.len < inline_size) {
                        // Still have room in small array
                        small[self.len] = item;
                        self.len += 1;
                    } else {
                        // Need to grow to heap
                        const new_capacity = inline_size * 2;
                        const new_ptr = try allocator.alloc(T, new_capacity);
                        @memcpy(new_ptr[0..inline_size], small);
                        new_ptr[self.len] = item;
                        self.len += 1;
                        self.data = .{ .heap = .{ .ptr = new_ptr.ptr, .capacity = new_capacity } };
                    }
                },
                .heap => |*heap| {
                    if (self.len < heap.capacity) {
                        // Have room in existing heap allocation
                        heap.ptr[self.len] = item;
                        self.len += 1;
                    } else {
                        // Need to grow heap allocation
                        const new_capacity = heap.capacity * 2;
                        const old_slice = heap.ptr[0..heap.capacity];
                        const new_ptr = try allocator.alloc(T, new_capacity);
                        @memcpy(new_ptr[0..self.len], old_slice[0..self.len]);
                        allocator.free(old_slice);
                        new_ptr[self.len] = item;
                        self.len += 1;
                        heap.* = .{ .ptr = new_ptr.ptr, .capacity = new_capacity };
                    }
                },
            }
        }
    };
}

test "SmallArrayList - basic operations" {
    const IntList = SmallArrayList(i32, 4);
    var list: IntList = .{};
    defer list.deinit(testing.allocator);

    // Test initial state
    try testing.expectEqual(0, list.len);
    try testing.expectEqual(0, list.asSlice().len);

    // Test append within small array
    try list.append(testing.allocator, 10);
    try testing.expectEqual(1, list.len);
    try testing.expectEqual(10, list.asSlice()[0]);

    try list.append(testing.allocator, 20);
    try list.append(testing.allocator, 30);
    try testing.expectEqual(3, list.len);
    try testing.expectEqualSlices(i32, &[_]i32{ 10, 20, 30 }, list.asSlice());
}

test "SmallArrayList - transition to heap" {
    const IntList = SmallArrayList(i32, 2);
    var list: IntList = .{};
    defer list.deinit(testing.allocator);

    // Fill small array
    try list.append(testing.allocator, 1);
    try list.append(testing.allocator, 2);
    try testing.expect(list.data == .small);

    // Trigger transition to heap
    try list.append(testing.allocator, 3);
    try testing.expect(list.data == .heap);
    try testing.expectEqual(3, list.len);
    try testing.expectEqualSlices(i32, &[_]i32{ 1, 2, 3 }, list.asSlice());

    // Continue appending on heap
    try list.append(testing.allocator, 4);
    try testing.expectEqual(4, list.len);
    try testing.expectEqualSlices(i32, &[_]i32{ 1, 2, 3, 4 }, list.asSlice());
}

test "SmallArrayList - heap growth" {
    const IntList = SmallArrayList(i32, 2);
    var list: IntList = .{};
    defer list.deinit(testing.allocator);

    // Add enough items to trigger multiple heap reallocations
    for (0..10) |i| {
        try list.append(testing.allocator, @intCast(i));
    }

    try testing.expectEqual(10, list.len);
    for (0..10) |i| {
        try testing.expectEqual(@as(i32, @intCast(i)), list.asSlice()[i]);
    }
}

test "SmallArrayList - mutable slice" {
    const IntList = SmallArrayList(i32, 4);
    var list: IntList = .{};
    defer list.deinit(testing.allocator);

    try list.append(testing.allocator, 10);
    try list.append(testing.allocator, 20);
    try list.append(testing.allocator, 30);

    // Modify via slice
    const slice = list.asSlice();
    slice[1] = 99;

    try testing.expectEqualSlices(i32, &[_]i32{ 10, 99, 30 }, list.asSlice());
}
