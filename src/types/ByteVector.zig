//! ByteVector implementation for the Scheme interpreter.
//!
//! This module provides the bytevector data structure used in Scheme. Bytevectors are
//! sequences of bytes that can be accessed by index and modified in place.
//! They are implemented using std.ArrayList(u8) for dynamic sizing and
//! efficient manipulation.

const std = @import("std");
const testing = std.testing;

const Vm = @import("../Vm.zig");

const ByteVector = @This();

/// The bytevector data as a dynamic array of bytes.
/// Uses ArrayList for efficient access and modification.
data: std.ArrayList(u8),

/// Creates a new empty bytevector.
///
/// Returns:
///   A new empty ByteVector instance.
pub fn init() ByteVector {
    return ByteVector{
        .data = std.ArrayList(u8){},
    };
}

/// Creates a new bytevector from the given slice of bytes.
///
/// The bytes are copied into the bytevector's internal storage.
///
/// Args:
///   allocator: The memory allocator to use for the bytevector's internal storage.
///   bytes: The initial bytes to copy into the bytevector.
///
/// Returns:
///   A new ByteVector instance containing a copy of the provided bytes.
///
/// Errors:
///   Returns OutOfMemory if allocation fails.
pub fn initFromSlice(allocator: std.mem.Allocator, bytes: []const u8) !ByteVector {
    var data = std.ArrayList(u8){};
    try data.appendSlice(allocator, bytes);
    return ByteVector{ .data = data };
}

/// Creates a new bytevector with the given initial capacity.
///
/// Args:
///   allocator: The memory allocator to use for the bytevector's internal storage.
///   capacity: The initial capacity to allocate.
///
/// Returns:
///   A new ByteVector instance with the specified capacity.
///
/// Errors:
///   Returns OutOfMemory if allocation fails.
pub fn initWithCapacity(allocator: std.mem.Allocator, capacity: usize) !ByteVector {
    var data = std.ArrayList(u8){};
    try data.ensureTotalCapacity(allocator, capacity);
    return ByteVector{ .data = data };
}

/// Releases all memory used by the bytevector.
///
/// After calling this method, the bytevector should not be used.
///
/// Args:
///   self: The bytevector to deinitialize.
///   allocator: The allocator used to create the bytevector.
pub fn deinit(self: *ByteVector, allocator: std.mem.Allocator) void {
    self.data.deinit(allocator);
}

/// Returns the number of bytes in the bytevector.
///
/// Args:
///   self: The bytevector to measure.
///
/// Returns:
///   The number of bytes in the bytevector.
pub fn len(self: ByteVector) usize {
    return self.data.items.len;
}

/// Appends a byte to the end of the bytevector.
///
/// Args:
///   self: The bytevector to append to.
///   allocator: The allocator to use for any necessary reallocation.
///   byte: The byte to append.
///
/// Errors:
///   Returns OutOfMemory if allocation fails.
pub fn append(self: *ByteVector, allocator: std.mem.Allocator, byte: u8) !void {
    try self.data.append(allocator, byte);
}

/// Gets the byte at the specified index.
///
/// Args:
///   self: The bytevector to access.
///   index: The index of the byte to retrieve.
///
/// Returns:
///   The byte at the specified index, or error if index is out of bounds.
///
/// Errors:
///   Returns IndexOutOfBounds if the index is invalid.
pub fn get(self: ByteVector, index: usize) !u8 {
    if (index >= self.data.items.len) {
        return error.IndexOutOfBounds;
    }
    return self.data.items[index];
}

/// Sets the byte at the specified index.
///
/// Args:
///   self: The bytevector to modify.
///   index: The index of the byte to set.
///   byte: The new byte to store at the index.
///
/// Errors:
///   Returns IndexOutOfBounds if the index is invalid.
pub fn set(self: *ByteVector, index: usize, byte: u8) !void {
    if (index >= self.data.items.len) {
        return error.IndexOutOfBounds;
    }
    self.data.items[index] = byte;
}

/// Returns a slice of all bytes in the bytevector.
///
/// Args:
///   self: The bytevector to access.
///
/// Returns:
///   A slice containing all bytes in the bytevector.
pub fn slice(self: ByteVector) []const u8 {
    return self.data.items;
}

/// Returns a mutable slice of all bytes in the bytevector.
///
/// Args:
///   self: The bytevector to access.
///
/// Returns:
///   A mutable slice containing all bytes in the bytevector.
pub fn sliceMut(self: *ByteVector) []u8 {
    return self.data.items;
}

test "ByteVector is small" {
    try testing.expectEqual(24, @sizeOf(ByteVector));
}

test "ByteVector init creates empty bytevector" {
    const bytevector = ByteVector.init();
    try testing.expectEqual(0, bytevector.len());
}

test "ByteVector initFromSlice creates bytevector with bytes" {
    const bytes = [_]u8{ 0x01, 0x02, 0x03 };
    var bytevector = try ByteVector.initFromSlice(testing.allocator, &bytes);
    defer bytevector.deinit(testing.allocator);

    try testing.expectEqual(3, bytevector.len());
    try testing.expectEqual(@as(u8, 0x01), try bytevector.get(0));
    try testing.expectEqual(@as(u8, 0x02), try bytevector.get(1));
    try testing.expectEqual(@as(u8, 0x03), try bytevector.get(2));
}

test "ByteVector initWithCapacity creates bytevector with capacity" {
    var bytevector = try ByteVector.initWithCapacity(testing.allocator, 10);
    defer bytevector.deinit(testing.allocator);

    try testing.expectEqual(0, bytevector.len());
    try testing.expect(bytevector.data.capacity >= 10);
}

test "ByteVector append adds bytes to bytevector" {
    var bytevector = ByteVector.init();
    defer bytevector.deinit(testing.allocator);

    try bytevector.append(testing.allocator, 0x42);
    try bytevector.append(testing.allocator, 0xFF);

    try testing.expectEqual(2, bytevector.len());
    try testing.expectEqual(@as(u8, 0x42), try bytevector.get(0));
    try testing.expectEqual(@as(u8, 0xFF), try bytevector.get(1));
}

test "ByteVector get returns correct bytes" {
    const bytes = [_]u8{ 0x10, 0x20, 0x30 };
    var bytevector = try ByteVector.initFromSlice(testing.allocator, &bytes);
    defer bytevector.deinit(testing.allocator);

    try testing.expectEqual(@as(u8, 0x10), try bytevector.get(0));
    try testing.expectEqual(@as(u8, 0x20), try bytevector.get(1));
    try testing.expectEqual(@as(u8, 0x30), try bytevector.get(2));
}

test "ByteVector get with invalid index returns error" {
    var bytevector = ByteVector.init();
    defer bytevector.deinit(testing.allocator);

    try testing.expectError(error.IndexOutOfBounds, bytevector.get(0));
    try testing.expectError(error.IndexOutOfBounds, bytevector.get(100));
}

test "ByteVector set modifies bytes" {
    const bytes = [_]u8{ 0x01, 0x02, 0x03 };
    var bytevector = try ByteVector.initFromSlice(testing.allocator, &bytes);
    defer bytevector.deinit(testing.allocator);

    try bytevector.set(1, 0x42);
    try testing.expectEqual(@as(u8, 0x01), try bytevector.get(0));
    try testing.expectEqual(@as(u8, 0x42), try bytevector.get(1));
    try testing.expectEqual(@as(u8, 0x03), try bytevector.get(2));
}

test "ByteVector set with invalid index returns error" {
    var bytevector = ByteVector.init();
    defer bytevector.deinit(testing.allocator);

    try testing.expectError(error.IndexOutOfBounds, bytevector.set(0, 0x42));
    try testing.expectError(error.IndexOutOfBounds, bytevector.set(100, 0x42));
}

test "ByteVector slice returns all bytes" {
    const bytes = [_]u8{ 0x01, 0x02, 0x03 };
    var bytevector = try ByteVector.initFromSlice(testing.allocator, &bytes);
    defer bytevector.deinit(testing.allocator);

    const slice_result = bytevector.slice();
    try testing.expectEqual(3, slice_result.len);
    try testing.expectEqual(@as(u8, 0x01), slice_result[0]);
    try testing.expectEqual(@as(u8, 0x02), slice_result[1]);
    try testing.expectEqual(@as(u8, 0x03), slice_result[2]);
}

test "ByteVector sliceMut allows modification" {
    const bytes = [_]u8{ 0x01, 0x02, 0x03 };
    var bytevector = try ByteVector.initFromSlice(testing.allocator, &bytes);
    defer bytevector.deinit(testing.allocator);

    const slice_mut = bytevector.sliceMut();
    slice_mut[1] = 0x42;

    try testing.expectEqual(@as(u8, 0x01), try bytevector.get(0));
    try testing.expectEqual(@as(u8, 0x42), try bytevector.get(1));
    try testing.expectEqual(@as(u8, 0x03), try bytevector.get(2));
}
