//! Symbol representation and interning system for the Scheme interpreter.
//!
//! This module provides efficient symbol management through string interning,
//! allowing symbols with identical content to share memory and be compared
//! by reference rather than string comparison. This is a critical optimization
//! for Lisp-family languages where symbol comparison is frequent.
const std = @import("std");
const testing = std.testing;

const Symbol = @This();

/// The string data representing this symbol.
data: []const u8,

/// Creates a new symbol with the provided string data.
/// The symbol will reference the provided string slice without copying it.
///
/// Args:
///   str: The string data for the symbol (not copied).
///
/// Returns:
///   A new Symbol instance referencing the provided string data.
pub fn init(str: []const u8) Symbol {
    return Symbol{ .data = str };
}

/// Compares two symbols for equality based on their string content.
/// Two symbols are considered equal if their underlying string data is identical,
/// regardless of memory location or other structural differences.
///
/// Args:
///   self: The first symbol to compare.
///   other: The second symbol to compare.
///
/// Returns:
///   true if the symbols have identical string content, false otherwise.
pub fn eql(self: Symbol, other: Symbol) bool {
    return std.mem.eql(u8, self.data, other.data);
}

/// Symbol interner that manages unique symbol instances for efficient comparison.
/// This implementation uses string interning to ensure that symbols with identical
/// string content share the same memory location and can be compared by reference.
pub const Interner = struct {
    arena: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    symbol_to_interned: std.StringHashMapUnmanaged(Interned) = .{},
    symbols: std.ArrayListUnmanaged(Symbol) = .{},

    /// Initializes a new symbol interner with the given allocator.
    /// The interner manages its own arena allocator for efficient memory management.
    ///
    /// Args:
    ///   allocator: The base allocator to use for internal data structures.
    ///
    /// Returns:
    ///   A new Interner instance ready for use.
    pub fn init(allocator: std.mem.Allocator) Interner {
        return Interner{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .allocator = allocator,
        };
    }

    /// Releases all memory associated with the interner.
    /// This includes the arena allocator and all internal data structures.
    /// After calling this function, the interner should not be used.
    ///
    /// Args:
    ///   self: Pointer to the interner to deinitialize.
    pub fn deinit(self: *Interner) void {
        self.arena.deinit();
        self.symbol_to_interned.deinit(self.allocator);
        self.symbols.deinit(self.allocator);
    }

    /// Interns a symbol, creating a copy of its data if it hasn't been seen before.
    /// If the symbol already exists in the interner, returns the existing interned version.
    /// The symbol's data will be duplicated and owned by the interner.
    ///
    /// Args:
    ///   self: Pointer to the interner.
    ///   symbol: The symbol to intern (data will be copied).
    ///
    /// Returns:
    ///   An Interned struct representing the unique symbol, or an error if allocation fails.
    ///
    /// Errors:
    ///   OutOfMemory: If memory allocation for the symbol data fails.
    pub fn intern(self: *Interner, symbol: Symbol) !Interned {
        if (self.symbol_to_interned.get(symbol.data)) |interned| return interned;
        const data = try self.arena.allocator().dupe(u8, symbol.data);
        const interned = Interned{ .id = @intCast(self.symbols.items.len) };
        try self.symbols.append(self.allocator, Symbol{ .data = data });
        try self.symbol_to_interned.put(self.allocator, data, interned);
        return interned;
    }

    /// Interns a symbol with static lifetime data, avoiding the need to copy.
    /// This is more efficient than intern() when the symbol data has static lifetime
    /// (e.g., string literals) and doesn't need to be duplicated.
    ///
    /// Args:
    ///   self: Pointer to the interner.
    ///   symbol: The symbol to intern (data will NOT be copied).
    ///
    /// Returns:
    ///   An Interned struct representing the unique symbol, or an error if allocation fails.
    ///
    /// Errors:
    ///   OutOfMemory: If memory allocation for internal data structures fails.
    ///
    /// Note:
    ///   The caller must ensure that symbol.data has a lifetime at least as long
    ///   as the interner itself.
    pub fn internStatic(self: *Interner, symbol: Symbol) !Interned {
        if (self.symbol_to_interned.get(symbol.data)) |interned| return interned;
        const interned = Interned{ .id = @intCast(self.symbols.items.len) };
        try self.symbols.append(self.allocator, symbol);
        try self.symbol_to_interned.put(self.allocator, symbol.data, interned);
        return interned;
    }

    /// Looks up an existing interned symbol without creating a new one.
    /// Returns the interned symbol if it already exists, or null if it hasn't been interned yet.
    ///
    /// Args:
    ///   self: Pointer to the interner.
    ///   symbol: The symbol to look up.
    ///
    /// Returns:
    ///   The existing interned symbol, or null if not found.
    pub fn lookup(self: Interner, symbol: Symbol) ?Interned {
        return self.symbol_to_interned.get(symbol.data);
    }

    /// Retrieves the string data for an interned symbol.
    /// Uses the symbol's ID to look up the corresponding string data.
    ///
    /// Args:
    ///   self: Pointer to the interner.
    ///   interned: The interned symbol to get the data for.
    ///
    /// Returns:
    ///   The string data associated with the interned symbol, or an error if the ID is invalid.
    ///
    /// Errors:
    ///   InvalidId: If the provided ID is out of bounds.
    pub fn get(self: Interner, interned: Interned) error{InvalidId}!Symbol {
        if (interned.id >= self.symbols.items.len) return error.InvalidId;
        return self.symbols.items[interned.id];
    }
};

/// Represents an interned symbol with a unique identifier.
/// Interned symbols can be compared efficiently by their ID rather than string content.
/// This is particularly useful in Scheme interpreters where symbol comparison is frequent.
pub const Interned = struct {
    /// Unique identifier for this interned symbol.
    /// Symbols with the same ID are guaranteed to have identical string content.
    id: u32,
};

test "internStatic uses passed in symbol" {
    var interner = Interner.init(testing.allocator);
    defer interner.deinit();

    const symbol = Symbol{ .data = "test-symbol" };
    const interned = try interner.internStatic(symbol);
    const retrieved = try interner.get(interned);

    try testing.expectEqual(symbol, retrieved);
}

test "intern creates a new symbol" {
    var interner = Interner.init(testing.allocator);
    defer interner.deinit();

    const symbol = Symbol{ .data = "test-symbol" };
    const interned = try interner.intern(symbol);
    const retrieved = try interner.get(interned);

    try testing.expect(!std.meta.eql(symbol, retrieved));
}

test "intern creates a string that is equivalent" {
    var interner = Interner.init(testing.allocator);
    defer interner.deinit();

    const symbol = Symbol{ .data = "test-symbol" };
    const interned = try interner.intern(symbol);
    const retrieved = try interner.get(interned);

    try testing.expect(symbol.eql(retrieved));
}

test "two symbols with same content are equivalent but different instances" {
    const data1 = try testing.allocator.dupe(u8, "test-symbol");
    defer testing.allocator.free(data1);
    const data2 = try testing.allocator.dupe(u8, "test-symbol");
    defer testing.allocator.free(data2);

    const symbol1 = Symbol{ .data = data1 };
    const symbol2 = Symbol{ .data = data2 };

    try testing.expect(symbol1.eql(symbol2));
    try testing.expect(data1.ptr != data2.ptr);
}
