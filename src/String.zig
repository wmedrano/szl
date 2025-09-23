//! String representation for the Scheme interpreter.
//!
//! This module defines the string type used in the Scheme interpreter.
//! Strings are implemented using std.ArrayList(u8) for dynamic sizing
//! and efficient manipulation. They support UTF-8 content but are
//! fundamentally byte-oriented for compatibility with Scheme semantics.

const std = @import("std");
const testing = std.testing;

const String = @This();

/// The string data as either empty, static, or a dynamic array of bytes.
/// This allows for efficient handling of empty strings, static string literals, and resizing of non-empty content.
data: union(enum) {
    empty: void,
    static: []const u8,
    mutable: std.ArrayList(u8),
},

/// Creates a new empty string with the given allocator.
///
/// Args:
///   allocator: The memory allocator to use for the string's internal storage.
///
/// Returns:
///   A new empty String instance.
pub fn init() String {
    return String{
        .data = .{ .empty = {} },
    };
}

/// Creates a new string from the given byte slice.
///
/// The content is copied into the string's internal storage.
///
/// Args:
///   allocator: The memory allocator to use for the string's internal storage.
///   content: The initial content to copy into the string.
///
/// Returns:
///   A new String instance containing a copy of the provided content.
///
/// Errors:
///   Returns OutOfMemory if allocation fails.
pub fn initFromSlice(allocator: std.mem.Allocator, content: []const u8) !String {
    if (content.len == 0) {
        return String{ .data = .{ .empty = {} } };
    }
    var data = std.ArrayList(u8){};
    try data.appendSlice(allocator, content);
    return String{ .data = .{ .mutable = data } };
}

/// Creates a new string from a static byte slice.
///
/// The content is not copied, but referenced directly. This is efficient for string literals
/// and other static content that has a lifetime at least as long as the String instance.
///
/// Args:
///   content: The static content to reference.
///
/// Returns:
///   A new String instance referencing the provided static content.
pub fn initStatic(content: []const u8) String {
    if (content.len == 0) {
        return String{ .data = .{ .empty = {} } };
    }
    return String{ .data = .{ .static = content } };
}

/// Releases all memory used by the string.
///
/// After calling this method, the string should not be used.
///
/// Args:
///   self: The string to deinitialize.
pub fn deinit(self: *String, allocator: std.mem.Allocator) void {
    switch (self.data) {
        .empty => {},
        .static => {},
        .mutable => |*mutable| mutable.deinit(allocator),
    }
}

/// Returns the length of the string in bytes.
///
/// Args:
///   self: The string to measure.
///
/// Returns:
///   The number of bytes in the string.
pub fn len(self: String) usize {
    return switch (self.data) {
        .empty => 0,
        .static => |static| static.len,
        .mutable => |mutable| mutable.items.len,
    };
}

/// Returns a slice view of the string's content.
///
/// The returned slice is valid until the string is modified or deallocated.
///
/// Args:
///   self: The string to get a slice from.
///
/// Returns:
///   A slice containing the string's content.
pub fn slice(self: String) []const u8 {
    return switch (self.data) {
        .empty => "",
        .static => |static| static,
        .mutable => |mutable| mutable.items,
    };
}

/// Appends a byte slice to the end of the string.
///
/// Args:
///   self: The string to append to.
///   allocator: The memory allocator to use.
///   content: The content to append.
///
/// Errors:
///   Returns OutOfMemory if allocation fails.
pub fn appendSlice(self: *String, allocator: std.mem.Allocator, content: []const u8) !void {
    switch (self.data) {
        .empty => {
            if (content.len > 0) {
                var mutable = std.ArrayList(u8){};
                try mutable.appendSlice(allocator, content);
                self.data = .{ .mutable = mutable };
            }
        },
        .static => |static| {
            var mutable = std.ArrayList(u8){};
            try mutable.appendSlice(allocator, static);
            try mutable.appendSlice(allocator, content);
            self.data = .{ .mutable = mutable };
        },
        .mutable => |*mutable| try mutable.appendSlice(allocator, content),
    }
}

/// Appends a single byte to the end of the string.
///
/// Args:
///   self: The string to append to.
///   allocator: The memory allocator to use.
///   byte: The byte to append.
///
/// Errors:
///   Returns OutOfMemory if allocation fails.
pub fn appendByte(self: *String, allocator: std.mem.Allocator, byte: u8) !void {
    switch (self.data) {
        .empty => {
            var mutable = std.ArrayList(u8){};
            try mutable.append(allocator, byte);
            self.data = .{ .mutable = mutable };
        },
        .static => |static| {
            var mutable = std.ArrayList(u8){};
            try mutable.appendSlice(allocator, static);
            try mutable.append(allocator, byte);
            self.data = .{ .mutable = mutable };
        },
        .mutable => |*mutable| try mutable.append(allocator, byte),
    }
}

/// Clears all content from the string, making it empty.
///
/// The string's capacity is preserved for future use.
///
/// Args:
///   self: The string to clear.
pub fn clear(self: *String) void {
    switch (self.data) {
        .empty => {},
        .static => self.data = .{ .empty = {} },
        .mutable => |*mutable| mutable.clearRetainingCapacity(),
    }
}

/// Compares this string with another string for equality.
///
/// Args:
///   self: The first string to compare.
///   other: The second string to compare.
///
/// Returns:
///   true if the strings contain identical content, false otherwise.
pub fn eql(self: String, other: String) bool {
    return std.mem.eql(u8, self.slice(), other.slice());
}

/// Compares this string with a byte slice for equality.
///
/// Args:
///   self: The string to compare.
///   other: The byte slice to compare against.
///
/// Returns:
///   true if the string content matches the slice, false otherwise.
pub fn eqlSlice(self: String, other: []const u8) bool {
    return std.mem.eql(u8, self.slice(), other);
}

test "String is small" {
    try testing.expectEqual(32, @sizeOf(String));
}

test "String init creates empty string" {
    var string = String.init();
    defer string.deinit(testing.allocator);

    try testing.expectEqual(@as(usize, 0), string.len());
    try testing.expectEqualStrings("", string.slice());
}

test "String initFromSlice creates string with content" {
    var string = try String.initFromSlice(testing.allocator, "hello world");
    defer string.deinit(testing.allocator);

    try testing.expectEqual(@as(usize, 11), string.len());
    try testing.expectEqualStrings("hello world", string.slice());
}

test "String appendSlice adds content" {
    var string = String.init();
    defer string.deinit(testing.allocator);

    try string.appendSlice(testing.allocator, "hello");
    try testing.expectEqualStrings("hello", string.slice());

    try string.appendSlice(testing.allocator, " world");
    try testing.expectEqualStrings("hello world", string.slice());
}

test "String appendByte adds single character" {
    var string = String.init();
    defer string.deinit(testing.allocator);

    try string.appendByte(testing.allocator, 'a');
    try string.appendByte(testing.allocator, 'b');
    try string.appendByte(testing.allocator, 'c');

    try testing.expectEqualStrings("abc", string.slice());
}

test "String clear empties the string" {
    var string = try String.initFromSlice(testing.allocator, "content");
    defer string.deinit(testing.allocator);

    try testing.expectEqual(@as(usize, 7), string.len());

    string.clear();
    try testing.expectEqual(@as(usize, 0), string.len());
    try testing.expectEqualStrings("", string.slice());
}

test "String eql compares strings correctly" {
    var string1 = try String.initFromSlice(testing.allocator, "hello");
    defer string1.deinit(testing.allocator);

    var string2 = try String.initFromSlice(testing.allocator, "hello");
    defer string2.deinit(testing.allocator);

    var string3 = try String.initFromSlice(testing.allocator, "world");
    defer string3.deinit(testing.allocator);

    try testing.expectEqual(true, string1.eql(string2));
    try testing.expectEqual(false, string1.eql(string3));
}

test "String eqlSlice compares with byte slice correctly" {
    var string = try String.initFromSlice(testing.allocator, "hello");
    defer string.deinit(testing.allocator);

    try testing.expectEqual(true, string.eqlSlice("hello"));
    try testing.expectEqual(false, string.eqlSlice("world"));
    try testing.expectEqual(false, string.eqlSlice("hello world"));
}

test "String handles empty content" {
    var string = try String.initFromSlice(testing.allocator, "");
    defer string.deinit(testing.allocator);

    try testing.expectEqual(@as(usize, 0), string.len());
    try testing.expectEqualStrings("", string.slice());
    try testing.expectEqual(true, string.eqlSlice(""));
}

test "String handles UTF-8 content" {
    var string = try String.initFromSlice(testing.allocator, "héllo 世界");
    defer string.deinit(testing.allocator);

    // Note: length is in bytes, not Unicode code points
    // "héllo 世界" = 'h' + 'é'(2 bytes) + 'l' + 'l' + 'o' + ' ' + '世'(3 bytes) + '界'(3 bytes) = 13 bytes
    try testing.expectEqual(@as(usize, 13), string.len());
    try testing.expectEqualStrings("héllo 世界", string.slice());
}
