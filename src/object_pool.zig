//! Object pool implementation for memory management with garbage collection support.
//!
//! This module provides a generic object pool that can store and manage
//! objects of any type with automatic handle-based access and recycling
//! of freed slots for optimal memory usage. The pool supports tri-color
//! garbage collection marking to enable automatic memory reclamation.

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

        /// Checks if two handles reference the same object.
        ///
        /// Args:
        ///   self: The first handle to compare.
        ///   other: The second handle to compare.
        ///
        /// Returns:
        ///   True if both handles reference the same object, false otherwise.
        pub fn eq(self: @This(), other: @This()) bool {
            return std.meta.eql(self, other);
        }
    };
}

/// Color enumeration used for tri-color garbage collection marking.
/// Objects are marked as red or blue during garbage collection cycles,
/// while tombstoned objects are marked for deletion.
pub const Color = enum {
    red,
    blue,
    tombstoned,

    /// Returns the opposite color for garbage collection cycles.
    /// Red becomes blue, blue becomes red, tombstoned remains tombstoned.
    ///
    /// Args:
    ///   self: The current color.
    ///
    /// Returns:
    ///   The opposite color for GC marking.
    pub fn other(self: Color) Color {
        switch (self) {
            .red => return .blue,
            .blue => return .red,
            .tombstoned => return .tombstoned,
        }
    }
};

/// Metadata associated with each object in the pool.
/// Contains garbage collection information for object lifecycle management.
const Metadata = struct {
    color: Color,

    /// Checks if this object is alive and not a re-usable placeholder.
    ///
    /// Args:
    ///   self: The metadata instance to check.
    ///
    /// Returns:
    ///   True if the object is alive, false if tombstoned.
    pub fn isAlive(self: Metadata) bool {
        return self.color != Color.tombstoned;
    }
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

        /// Returns a pointer to the next live object in the pool, or null if no more objects exist.
        /// Automatically skips over deleted objects.
        ///
        /// Args:
        ///   self: Pointer to the iterator instance.
        ///
        /// Returns:
        ///   A pointer to the next live object, or null if iteration is complete.
        pub fn next(self: *Iterator(T)) ?*T {
            while (self.index < self.pool.objects.items.len) {
                const idx = self.index;
                self.index += 1;
                if (self.pool.metadata.items[idx].isAlive())
                    return &self.pool.objects.items[idx];
            }
            return null;
        }
    };
}

/// Creates a sweep iterator type for garbage collection.
///
/// Args:
///   T: The type of objects stored in the pool.
///
/// Returns:
///   A sweep iterator type that can traverse and collect unreachable objects.
fn SweepIter(T: type) type {
    return struct {
        pool: *ObjectPool(T),
        allocator: std.mem.Allocator,
        target_color: Color,
        index: usize,

        /// Returns a pointer to the next object that should be collected, or null if sweep is complete.
        /// Objects that don't match the target color are marked as tombstoned and added to the free list.
        ///
        /// Args:
        ///   self: Pointer to the sweep iterator instance.
        ///
        /// Returns:
        ///   A pointer to the next object to be collected, or null if sweep is complete.
        ///
        /// Errors:
        ///   OutOfMemory: If expanding the free list fails.
        pub fn next(self: *SweepIter(T)) !?*T {
            while (self.index < self.pool.objects.items.len) {
                const idx = self.index;
                self.index += 1;
                const metadata = &self.pool.metadata.items[idx];
                if (metadata.isAlive() and metadata.color != self.target_color) {
                    metadata.color = Color.tombstoned;
                    const handle = Handle(T){ .idx = idx };
                    try self.pool.free.append(self.allocator, handle);
                    return &self.pool.objects.items[idx];
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
        /// The pool manages memory efficiently by recycling previously freed slots.
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
        pub fn put(self: *ObjectPool(T), allocator: std.mem.Allocator, obj: T, color: Color) !Handle(T) {
            if (self.free.pop()) |id| {
                self.objects.items[id.idx] = obj;
                self.metadata.items[id.idx] = Metadata{ .color = color };
                return id;
            }
            const id = Handle(T){ .idx = self.objects.items.len };
            try self.objects.append(allocator, obj);
            try self.metadata.append(allocator, Metadata{ .color = color });
            return id;
        }

        /// Checks if a handle references a live object in the pool.
        /// Validates both that the handle index is within bounds and that
        /// the object at that index is not tombstoned.
        ///
        /// Args:
        ///   self: The object pool to check.
        ///   handle: The handle to validate.
        ///
        /// Returns:
        ///   True if the handle references a live object, false otherwise.
        fn isAlive(self: ObjectPool(T), handle: Handle(T)) bool {
            return handle.idx < self.objects.items.len and self.metadata.items[handle.idx].isAlive();
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
            if (self.isAlive(handle))
                return self.objects.items[handle.idx]
            else
                return null;
        }

        /// Gets a mutable pointer to an object in the pool.
        ///
        /// Args:
        ///   self: Pointer to the object pool.
        ///   handle: Handle to the object to retrieve.
        ///
        /// Returns:
        ///   A mutable pointer to the object if the handle is valid, null otherwise.
        pub fn getMutable(self: *ObjectPool(T), handle: Handle(T)) ?*T {
            if (self.isAlive(handle))
                return &self.objects.items[handle.idx]
            else
                return null;
        }

        /// Sets the color of an object in the pool.
        ///
        /// Args:
        ///   self: Pointer to the object pool.
        ///   handle: Handle to the object to update.
        ///   color: The new color to set.
        ///
        /// Returns:
        ///   The object if the color was changed, null if the handle is invalid or color was already set.
        pub fn setColor(self: *ObjectPool(T), handle: Handle(T), color: Color) ?T {
            if (!self.isAlive(handle)) return null;
            if (self.metadata.items[handle.idx].color == color) return null;
            self.metadata.items[handle.idx].color = color;
            return self.objects.items[handle.idx];
        }

        /// Removes an object from the pool, making its handle invalid.
        /// The pool may reuse the freed slot for future allocations.
        ///
        /// Args:
        ///   self: Pointer to the object pool.
        ///   allocator: Allocator to use for managing the free list.
        ///   handle: Handle to the object to delete.
        ///
        /// Errors:
        ///   OutOfMemory: If expanding the free list fails.
        pub fn delete(self: *ObjectPool(T), allocator: std.mem.Allocator, handle: Handle(T)) !void {
            if (!self.isAlive(handle)) return;
            self.metadata.items[handle.idx].color = Color.tombstoned;
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

        /// Creates a sweep iterator for garbage collection.
        /// The iterator returns objects that don't match the target color and should be collected.
        ///
        /// Args:
        ///   self: Pointer to the object pool to sweep.
        ///   allocator: Allocator to use for managing the free list.
        ///   color: The target color indicating reachable objects.
        ///
        /// Returns:
        ///   A sweep iterator positioned at the beginning of the pool.
        pub fn sweep(self: *ObjectPool(T), allocator: std.mem.Allocator, color: Color) SweepIter(T) {
            return SweepIter(T){
                .pool = self,
                .allocator = allocator,
                .target_color = color,
                .index = 0,
            };
        }
    };
}

test "get on created object returns object" {
    var pool = ObjectPool(i32).init();
    defer pool.deinit(testing.allocator);

    const handle = try pool.put(testing.allocator, 42, Color.red);
    try testing.expectEqual(42, pool.get(handle));
}

test "get on deleted object returns null" {
    var pool = ObjectPool(i32).init();
    defer pool.deinit(testing.allocator);

    const handle = try pool.put(testing.allocator, 42, Color.red);

    try pool.delete(testing.allocator, handle);
    try testing.expectEqual(null, pool.get(handle));
}

test "delete + put recycles handle id" {
    var pool = ObjectPool(i32).init();
    defer pool.deinit(testing.allocator);

    const handle1 = try pool.put(testing.allocator, 42, Color.red);

    try pool.delete(testing.allocator, handle1);
    const handle2 = try pool.put(testing.allocator, 200, Color.red);

    try testing.expectEqual(handle1.idx, handle2.idx);
    try testing.expectEqual(200, pool.get(handle2));
}
