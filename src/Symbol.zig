const std = @import("std");

const Symbol = @This();

string: []const u8,

pub fn init(str: []const u8) Symbol {
    return Symbol{ .string = str };
}

pub const Interned = struct {
    id: u32,
};

pub const Interner = struct {
    arena: std.heap.ArenaAllocator,
    string_to_interned: std.StringHashMapUnmanaged(Interned) = .{},
    interned_to_symbol: std.ArrayListUnmanaged(Symbol) = .{},

    pub fn init(allocator: std.mem.Allocator) Interner {
        return .{
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *Interner, allocator: std.mem.Allocator) void {
        self.string_to_interned.deinit(allocator);
        self.interned_to_symbol.deinit(allocator);
        self.arena.deinit();
    }

    pub fn intern(self: *Interner, allocator: std.mem.Allocator, symbol: Symbol) !Interned {
        if (self.string_to_interned.get(symbol.string)) |interned| {
            return interned;
        }
        const owned_string = try self.arena.allocator().dupe(u8, symbol.string);
        const id: u32 = @intCast(self.interned_to_symbol.items.len);
        const interned = Interned{ .id = id };
        try self.interned_to_symbol.append(allocator, Symbol{ .string = owned_string });
        try self.string_to_interned.put(allocator, owned_string, interned);
        return interned;
    }

    pub fn internStatic(self: *Interner, allocator: std.mem.Allocator, symbol: Symbol) !Interned {
        if (self.string_to_interned.get(symbol.string)) |interned| {
            return interned;
        }
        const id: u32 = @intCast(self.interned_to_symbol.items.len);
        const interned = Interned{ .id = id };
        try self.interned_to_symbol.append(allocator, symbol);
        try self.string_to_interned.put(allocator, symbol.string, interned);
        return interned;
    }

    pub fn asSymbol(self: Interner, interned: Interned) ?Symbol {
        if (interned.id >= self.interned_to_symbol.items.len) {
            return null;
        }
        return self.interned_to_symbol.items[interned.id];
    }
};

test "Symbol.init" {
    const sym = Symbol.init("hello");
    try std.testing.expectEqualStrings("hello", sym.string);
}

test "Interner.init and deinit" {
    const allocator = std.testing.allocator;
    var interner = Interner.init(allocator);
    defer interner.deinit(allocator);
}

test "Interner.intern deduplicates symbols" {
    const allocator = std.testing.allocator;
    var interner = Interner.init(allocator);
    defer interner.deinit(allocator);

    const sym1 = Symbol.init("test");
    const sym2 = Symbol.init("test");

    const interned1 = try interner.intern(allocator, sym1);
    const interned2 = try interner.intern(allocator, sym2);
    try std.testing.expectEqual(interned1.id, interned2.id);
}

test "Interner.intern creates unique IDs for different symbols" {
    const allocator = std.testing.allocator;
    var interner = Interner.init(allocator);
    defer interner.deinit(allocator);

    const interned1 = try interner.intern(allocator, Symbol.init("foo"));
    const interned2 = try interner.intern(allocator, Symbol.init("bar"));

    try std.testing.expect(interned1.id != interned2.id);
}

test "Interner.asSymbol retrieves correct symbol" {
    const allocator = std.testing.allocator;
    var interner = Interner.init(allocator);
    defer interner.deinit(allocator);

    const interned = try interner.intern(allocator, Symbol.init("hello"));

    try std.testing.expectEqualDeep(
        Symbol.init("hello"),
        interner.asSymbol(interned),
    );
}

test "Interner.asSymbol returns null for invalid ID" {
    const allocator = std.testing.allocator;
    var interner = Interner.init(allocator);
    defer interner.deinit(allocator);

    const invalid = Interned{ .id = 999 };
    try std.testing.expectEqual(null, interner.asSymbol(invalid));
}

test "Interner.internStatic with static string" {
    const allocator = std.testing.allocator;
    var interner = Interner.init(allocator);
    defer interner.deinit(allocator);

    const static_str = "static_symbol";
    const sym = Symbol.init(static_str);

    const interned = try interner.internStatic(allocator, sym);

    try std.testing.expectEqualDeep(
        Symbol.init(static_str),
        interner.asSymbol(interned),
    );
}

test "Interner.internStatic deduplicates like intern" {
    const allocator = std.testing.allocator;
    var interner = Interner.init(allocator);
    defer interner.deinit(allocator);

    const static_str = "duplicate";

    const interned1 = try interner.internStatic(allocator, Symbol.init(static_str));
    const interned2 = try interner.internStatic(allocator, Symbol.init(static_str));

    try std.testing.expectEqual(interned1.id, interned2.id);
}

test "Interner.intern uses arena for string storage" {
    const allocator = std.testing.allocator;
    var interner = Interner.init(allocator);
    defer interner.deinit(allocator);

    // Create a temporary string that will go out of scope
    var buffer: [10]u8 = undefined;
    @memcpy(buffer[0..5], "temp!");
    const temp_str = buffer[0..5];

    const sym = Symbol.init(temp_str);
    const interned = try interner.intern(allocator, sym);

    // Modify the original buffer to prove we copied
    @memcpy(buffer[0..5], "xxxxx");

    try std.testing.expectEqualDeep(
        Symbol.init("temp!"),
        interner.asSymbol(interned),
    );
}

test "Interner multiple symbols with sequential IDs" {
    const allocator = std.testing.allocator;
    var interner = Interner.init(allocator);
    defer interner.deinit(allocator);

    const symbols = [_][]const u8{ "first", "second", "third" };

    for (symbols, 0..) |str, i| {
        const sym = Symbol.init(str);
        const interned = try interner.intern(allocator, sym);
        try std.testing.expectEqual(i, interned.id);
    }
}
