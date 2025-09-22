//! Vector implementation for the Scheme interpreter.
//!
//! This module provides the vector data structure used in Scheme. Vectors are
//! sequences of values that can be accessed by index and modified in place.
//! They are implemented using std.ArrayList(Val) for dynamic sizing and
//! efficient manipulation.

const std = @import("std");
const testing = std.testing;

const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const Vector = @This();

/// The vector data as a dynamic array of values.
/// Uses ArrayList for efficient access and modification.
data: std.ArrayList(Val),

/// Creates a new empty vector.
///
/// Returns:
///   A new empty Vector instance.
pub fn init() Vector {
    return Vector{
        .data = std.ArrayList(Val){},
    };
}

/// Creates a new vector from the given slice of values.
///
/// The values are copied into the vector's internal storage.
///
/// Args:
///   allocator: The memory allocator to use for the vector's internal storage.
///   values: The initial values to copy into the vector.
///
/// Returns:
///   A new Vector instance containing a copy of the provided values.
///
/// Errors:
///   Returns OutOfMemory if allocation fails.
pub fn initFromSlice(allocator: std.mem.Allocator, values: []const Val) !Vector {
    var data = std.ArrayList(Val){};
    try data.appendSlice(allocator, values);
    return Vector{ .data = data };
}

/// Creates a new vector with the given initial capacity.
///
/// Args:
///   allocator: The memory allocator to use for the vector's internal storage.
///   capacity: The initial capacity to allocate.
///
/// Returns:
///   A new Vector instance with the specified capacity.
///
/// Errors:
///   Returns OutOfMemory if allocation fails.
pub fn initWithCapacity(allocator: std.mem.Allocator, capacity: usize) !Vector {
    var data = std.ArrayList(Val){};
    try data.ensureTotalCapacity(allocator, capacity);
    return Vector{ .data = data };
}

/// Releases all memory used by the vector.
///
/// After calling this method, the vector should not be used.
///
/// Args:
///   self: The vector to deinitialize.
///   allocator: The allocator used to create the vector.
pub fn deinit(self: *Vector, allocator: std.mem.Allocator) void {
    self.data.deinit(allocator);
}

/// Returns the number of elements in the vector.
///
/// Args:
///   self: The vector to measure.
///
/// Returns:
///   The number of elements in the vector.
pub fn len(self: Vector) usize {
    return self.data.items.len;
}

/// Appends a value to the end of the vector.
///
/// Args:
///   self: The vector to append to.
///   allocator: The allocator to use for any necessary reallocation.
///   value: The value to append.
///
/// Errors:
///   Returns OutOfMemory if allocation fails.
pub fn append(self: *Vector, allocator: std.mem.Allocator, value: Val) !void {
    try self.data.append(allocator, value);
}

/// Gets the value at the specified index.
///
/// Args:
///   self: The vector to access.
///   index: The index of the element to retrieve.
///
/// Returns:
///   The value at the specified index, or error if index is out of bounds.
///
/// Errors:
///   Returns IndexOutOfBounds if the index is invalid.
pub fn get(self: Vector, index: usize) !Val {
    if (index >= self.data.items.len) {
        return error.IndexOutOfBounds;
    }
    return self.data.items[index];
}

/// Sets the value at the specified index.
///
/// Args:
///   self: The vector to modify.
///   index: The index of the element to set.
///   value: The new value to store at the index.
///
/// Errors:
///   Returns IndexOutOfBounds if the index is invalid.
pub fn set(self: *Vector, index: usize, value: Val) !void {
    if (index >= self.data.items.len) {
        return error.IndexOutOfBounds;
    }
    self.data.items[index] = value;
}

/// Returns a slice of all values in the vector.
///
/// Args:
///   self: The vector to access.
///
/// Returns:
///   A slice containing all values in the vector.
pub fn slice(self: Vector) []const Val {
    return self.data.items;
}

/// Returns a mutable slice of all values in the vector.
///
/// Args:
///   self: The vector to access.
///
/// Returns:
///   A mutable slice containing all values in the vector.
pub fn sliceMut(self: *Vector) []Val {
    return self.data.items;
}

test "Vector init creates empty vector" {
    const vector = Vector.init();
    try testing.expectEqual(0, vector.len());
}

test "Vector initFromSlice creates vector with values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const values = [_]Val{ Val.init(1), Val.init(2), Val.init(3) };
    var vector = try Vector.initFromSlice(testing.allocator, &values);
    defer vector.deinit(testing.allocator);

    try testing.expectEqual(3, vector.len());
    try testing.expectEqual(Val.init(1), try vector.get(0));
    try testing.expectEqual(Val.init(2), try vector.get(1));
    try testing.expectEqual(Val.init(3), try vector.get(2));
}

test "Vector initWithCapacity creates vector with capacity" {
    var vector = try Vector.initWithCapacity(testing.allocator, 10);
    defer vector.deinit(testing.allocator);

    try testing.expectEqual(0, vector.len());
    try testing.expect(vector.data.capacity >= 10);
}

test "Vector append adds values to vector" {
    var vector = Vector.init();
    defer vector.deinit(testing.allocator);

    try vector.append(testing.allocator, Val.init(42));
    try vector.append(testing.allocator, Val.init(true));

    try testing.expectEqual(2, vector.len());
    try testing.expectEqual(Val.init(42), try vector.get(0));
    try testing.expectEqual(Val.init(true), try vector.get(1));
}

test "Vector get returns correct values" {
    const values = [_]Val{ Val.init(10), Val.init(20), Val.init(30) };
    var vector = try Vector.initFromSlice(testing.allocator, &values);
    defer vector.deinit(testing.allocator);

    try testing.expectEqual(Val.init(10), try vector.get(0));
    try testing.expectEqual(Val.init(20), try vector.get(1));
    try testing.expectEqual(Val.init(30), try vector.get(2));
}

test "Vector get with invalid index returns error" {
    var vector = Vector.init();
    defer vector.deinit(testing.allocator);

    try testing.expectError(error.IndexOutOfBounds, vector.get(0));
    try testing.expectError(error.IndexOutOfBounds, vector.get(100));
}

test "Vector set modifies values" {
    const values = [_]Val{ Val.init(1), Val.init(2), Val.init(3) };
    var vector = try Vector.initFromSlice(testing.allocator, &values);
    defer vector.deinit(testing.allocator);

    try vector.set(1, Val.init(42));
    try testing.expectEqual(Val.init(1), try vector.get(0));
    try testing.expectEqual(Val.init(42), try vector.get(1));
    try testing.expectEqual(Val.init(3), try vector.get(2));
}

test "Vector set with invalid index returns error" {
    var vector = Vector.init();
    defer vector.deinit(testing.allocator);

    try testing.expectError(error.IndexOutOfBounds, vector.set(0, Val.init(42)));
    try testing.expectError(error.IndexOutOfBounds, vector.set(100, Val.init(42)));
}

test "Vector slice returns all values" {
    const values = [_]Val{ Val.init(1), Val.init(2), Val.init(3) };
    var vector = try Vector.initFromSlice(testing.allocator, &values);
    defer vector.deinit(testing.allocator);

    const slice_result = vector.slice();
    try testing.expectEqual(3, slice_result.len);
    try testing.expectEqual(Val.init(1), slice_result[0]);
    try testing.expectEqual(Val.init(2), slice_result[1]);
    try testing.expectEqual(Val.init(3), slice_result[2]);
}

test "Vector sliceMut allows modification" {
    const values = [_]Val{ Val.init(1), Val.init(2), Val.init(3) };
    var vector = try Vector.initFromSlice(testing.allocator, &values);
    defer vector.deinit(testing.allocator);

    const slice_mut = vector.sliceMut();
    slice_mut[1] = Val.init(42);

    try testing.expectEqual(Val.init(1), try vector.get(0));
    try testing.expectEqual(Val.init(42), try vector.get(1));
    try testing.expectEqual(Val.init(3), try vector.get(2));
}
