//! Object pool implementation for memory management.
//!
//! This module provides a generic object pool that can store and manage
//! objects of any type with automatic handle-based access and recycling
//! of freed slots for optimal memory usage.

const std = @import("std");
const testing = std.testing;

/// Creates a handle type for objects stored in the pool.
/// Handles are lightweight references that contain only an index.
///
/// Args:
///   T: The type of objects that will be referenced by this handle.
///
/// Returns:
///   A handle type that can reference objects of type T.
pub fn Handle(T: type) type {
    return struct {
        idx: usize,
        const _ = T;
    };
}

const Metadata = struct {
    is_alive: bool,
};

/// Creates an iterator type for traversing live objects in the pool.
///
/// Args:
///   T: The type of objects stored in the pool.
///
/// Returns:
///   An iterator type that can traverse objects of type T.
fn Iterator(T: type) type {
    return struct {
        pool: *const ObjectPool(T),
        index: usize,

        /// Returns the next live object in the pool, or null if no more objects exist.
        /// Automatically skips over deleted objects.
        ///
        /// Args:
        ///   self: Pointer to the iterator instance.
        ///
        /// Returns:
        ///   The next live object, or null if iteration is complete.
        pub fn next(self: *Iterator(T)) ?T {
            while (self.index < self.pool.objects.items.len) {
                const idx = self.index;
                self.index += 1;
                if (self.pool.metadata.items[idx].is_alive) {
                    return self.pool.objects.items[idx];
                }
            }
            return null;
        }
    };
}

/// Creates an object pool type for managing objects of type T.
/// The pool provides efficient allocation, deallocation, and iteration
/// over objects while recycling freed slots to minimize memory fragmentation.
///
/// Args:
///   T: The type of objects to store in the pool.
///
/// Returns:
///   An ObjectPool type configured for objects of type T.
pub fn ObjectPool(T: type) type {
    return struct {
        objects: std.ArrayListUnmanaged(T) = .{},
        metadata: std.ArrayListUnmanaged(Metadata) = .{},
        free: std.ArrayListUnmanaged(Handle(T)) = .{},

        /// Initializes a new empty object pool.
        ///
        /// Returns:
        ///   A new ObjectPool instance ready for use.
        pub fn init() ObjectPool(T) {
            return ObjectPool(T){};
        }

        /// Releases all memory associated with the object pool.
        /// After calling this function, the pool should not be used.
        ///
        /// Args:
        ///   self: Pointer to the pool to deinitialize.
        ///   allocator: The allocator used to free memory.
        pub fn deinit(self: *ObjectPool(T), allocator: std.mem.Allocator) void {
            self.objects.deinit(allocator);
            self.metadata.deinit(allocator);
            self.free.deinit(allocator);
        }

        /// Adds an object to the pool and returns a handle for accessing it.
        /// Reuses freed slots when available, otherwise allocates new space.
        ///
        /// Args:
        ///   self: Pointer to the object pool.
        ///   allocator: Allocator to use for memory allocation if needed.
        ///   obj: The object to store in the pool.
        ///
        /// Returns:
        ///   A handle that can be used to access the stored object.
        ///
        /// Errors:
        ///   OutOfMemory: If allocation fails when expanding the pool.
        pub fn put(self: *ObjectPool(T), allocator: std.mem.Allocator, obj: T) !Handle(T) {
            if (self.free.pop()) |id| {
                self.objects.items[id.idx] = obj;
                self.metadata.items[id.idx] = Metadata{ .is_alive = true };
                return id;
            }
            const id = Handle(T){ .idx = self.objects.items.len };
            try self.objects.append(allocator, obj);
            try self.metadata.append(allocator, Metadata{ .is_alive = true });
            return id;
        }

        /// Checks if a handle references a live object in the pool.
        ///
        /// Args:
        ///   self: The object pool to check.
        ///   handle: The handle to validate.
        ///
        /// Returns:
        ///   True if the handle references a live object, false otherwise.
        fn is_alive(self: ObjectPool(T), handle: Handle(T)) bool {
            return handle.idx < self.objects.items.len and self.metadata.items[handle.idx].is_alive;
        }

        /// Retrieves an object from the pool using its handle.
        ///
        /// Args:
        ///   self: The object pool to retrieve from.
        ///   handle: The handle referencing the desired object.
        ///
        /// Returns:
        ///   The object if the handle is valid and references a live object,
        ///   null otherwise.
        pub fn get(self: ObjectPool(T), handle: Handle(T)) ?T {
            if (self.is_alive(handle))
                return self.objects.items[handle.idx]
            else
                return null;
        }

        /// Marks an object as deleted and makes its slot available for reuse.
        /// The object's memory is not immediately freed, but the slot can be
        /// recycled for future allocations.
        ///
        /// Args:
        ///   self: Pointer to the object pool.
        ///   allocator: Allocator to use for managing the free list.
        ///   handle: Handle to the object to delete.
        ///
        /// Errors:
        ///   OutOfMemory: If expanding the free list fails.
        pub fn delete(self: *ObjectPool(T), allocator: std.mem.Allocator, handle: Handle(T)) !void {
            if (!self.is_alive(handle)) return;
            self.metadata.items[handle.idx].is_alive = false;
            try self.free.append(allocator, handle);
        }

        /// Creates an iterator for traversing all live objects in the pool.
        /// The iterator automatically skips deleted objects.
        ///
        /// Args:
        ///   self: Pointer to the object pool to iterate over.
        ///
        /// Returns:
        ///   An iterator positioned at the beginning of the pool.
        pub fn iterator(self: *const ObjectPool(T)) Iterator(T) {
            return Iterator(T){
                .pool = self,
                .index = 0,
            };
        }
    };
}

test "get on created object returns object" {
    var pool = ObjectPool(i32).init();
    defer pool.deinit(testing.allocator);

    const handle = try pool.put(testing.allocator, 42);
    try testing.expectEqual(42, pool.get(handle));
}

test "get on deleted object returns null" {
    var pool = ObjectPool(i32).init();
    defer pool.deinit(testing.allocator);

    const handle = try pool.put(testing.allocator, 42);

    try pool.delete(testing.allocator, handle);
    try testing.expectEqual(null, pool.get(handle));
}

test "delete + put recycles handle id" {
    var pool = ObjectPool(i32).init();
    defer pool.deinit(testing.allocator);

    const handle1 = try pool.put(testing.allocator, 42);

    try pool.delete(testing.allocator, handle1);
    const handle2 = try pool.put(testing.allocator, 200);

    try testing.expectEqual(handle1.idx, handle2.idx);
    try testing.expectEqual(200, pool.get(handle2));
}
