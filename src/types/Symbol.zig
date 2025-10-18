const std = @import("std");

const Symbol = @This();

id: u32,

pub fn eq(self: Symbol, other: Symbol) bool {
    return self.id == other.id;
}

pub const Interner = struct {
    arena: std.heap.ArenaAllocator,
    string_to_symbol: std.StringHashMapUnmanaged(Symbol),
    symbol_to_string: std.ArrayListUnmanaged([]const u8),

    pub fn init(allocator: std.mem.Allocator) error{OutOfMemory}!Interner {
        var string_to_symbol = std.StringHashMapUnmanaged(Symbol){};
        const symbol_to_string = try std.ArrayListUnmanaged([]const u8).initCapacity(allocator, 1024);
        try string_to_symbol.ensureTotalCapacity(allocator, 1024);
        return .{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .string_to_symbol = string_to_symbol,
            .symbol_to_string = symbol_to_string,
        };
    }

    pub fn deinit(self: *Interner, allocator: std.mem.Allocator) void {
        self.string_to_symbol.deinit(allocator);
        self.symbol_to_string.deinit(allocator);
        self.arena.deinit();
    }

    pub fn intern(self: *Interner, allocator: std.mem.Allocator, str: []const u8) !Symbol {
        if (self.string_to_symbol.get(str)) |interned| {
            return interned;
        }
        const owned_string = try self.arena.allocator().dupe(u8, str);
        const id: u32 = @intCast(self.symbol_to_string.items.len);
        const interned = Symbol{ .id = id };
        try self.symbol_to_string.append(allocator, owned_string);
        try self.string_to_symbol.put(allocator, owned_string, interned);
        return interned;
    }

    pub fn internStatic(self: *Interner, allocator: std.mem.Allocator, str: []const u8) !Symbol {
        if (self.string_to_symbol.get(str)) |interned| {
            return interned;
        }
        const id: u32 = @intCast(self.symbol_to_string.items.len);
        const interned = Symbol{ .id = id };
        try self.symbol_to_string.append(allocator, str);
        try self.string_to_symbol.put(allocator, str, interned);
        return interned;
    }

    pub fn asString(self: Interner, interned: Symbol) ?[]const u8 {
        if (interned.id >= self.symbol_to_string.items.len) {
            return null;
        }
        return self.symbol_to_string.items[interned.id];
    }
};

test "Interner.init and deinit" {
    const allocator = std.testing.allocator;
    var interner = try Interner.init(allocator);
    defer interner.deinit(allocator);
}

test "Interner.intern deduplicates symbols" {
    const allocator = std.testing.allocator;
    var interner = try Interner.init(allocator);
    defer interner.deinit(allocator);

    const interned1 = try interner.intern(allocator, "test");
    const interned2 = try interner.intern(allocator, "test");
    try std.testing.expectEqual(interned1.id, interned2.id);
}

test "Interner.intern creates unique IDs for different symbols" {
    const allocator = std.testing.allocator;
    var interner = try Interner.init(allocator);
    defer interner.deinit(allocator);

    const interned1 = try interner.intern(allocator, "foo");
    const interned2 = try interner.intern(allocator, "bar");

    try std.testing.expect(interned1.id != interned2.id);
}

test "Interner.asSymbol retrieves correct symbol" {
    const allocator = std.testing.allocator;
    var interner = try Interner.init(allocator);
    defer interner.deinit(allocator);

    const interned = try interner.intern(allocator, "hello");

    try std.testing.expectEqualDeep(
        "hello",
        interner.asString(interned),
    );
}

test "Interner.asSymbol returns null for invalid ID" {
    const allocator = std.testing.allocator;
    var interner = try Interner.init(allocator);
    defer interner.deinit(allocator);

    const invalid = Symbol{ .id = 999 };
    try std.testing.expectEqual(null, interner.asString(invalid));
}

test "Interner.internStatic with static string" {
    const allocator = std.testing.allocator;
    var interner = try Interner.init(allocator);
    defer interner.deinit(allocator);

    const static_str = "static-symbol";
    const interned = try interner.internStatic(allocator, "static-symbol");

    try std.testing.expectEqualDeep(
        static_str,
        interner.asString(interned),
    );
}

test "Interner.internStatic deduplicates like intern" {
    const allocator = std.testing.allocator;
    var interner = try Interner.init(allocator);
    defer interner.deinit(allocator);

    const static_str = "duplicate";

    const interned1 = try interner.internStatic(allocator, static_str);
    const interned2 = try interner.internStatic(allocator, static_str);

    try std.testing.expectEqual(interned1.id, interned2.id);
}

test "Interner.intern uses arena for string storage" {
    const allocator = std.testing.allocator;
    var interner = try Interner.init(allocator);
    defer interner.deinit(allocator);

    // Create a temporary string that will go out of scope
    var buffer: [10]u8 = undefined;
    @memcpy(buffer[0..5], "temp!");
    const temp_str = buffer[0..5];

    const sym = temp_str;
    const interned = try interner.intern(allocator, sym);

    // Modify the original buffer to prove we copied
    @memcpy(buffer[0..5], "xxxxx");

    try std.testing.expectEqualDeep(
        "temp!",
        interner.asString(interned),
    );
}

test "Interner multiple symbols with sequential IDs" {
    const allocator = std.testing.allocator;
    var interner = try Interner.init(allocator);
    defer interner.deinit(allocator);

    const symbols = [_][]const u8{ "first", "second", "third" };

    for (symbols, 0..) |str, i| {
        const sym = str;
        const interned = try interner.intern(allocator, sym);
        try std.testing.expectEqual(i, interned.id);
    }
}
