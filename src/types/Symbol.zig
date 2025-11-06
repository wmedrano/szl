const std = @import("std");

const Symbol = @This();

data: u64,

pub fn init(comptime str: []const u8) Symbol {
    const short_len = 7;
    if (str.len > short_len) {
        @compileError("Symbol string too long for compile-time initialization: " ++ str);
    }
    var data = [8]u8{ 0, 0, 0, 0, 0, 0, 0, @as(u8, @intCast(str.len)) };
    @memcpy(data[0..str.len], str);
    return Symbol{ .data = std.mem.readInt(u64, &data, .little) };
}

pub fn eq(self: Symbol, other: Symbol) bool {
    return self.data == other.data;
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
        return self.internImpl(allocator, str, false);
    }

    pub fn internStatic(self: *Interner, allocator: std.mem.Allocator, str: []const u8) !Symbol {
        return self.internImpl(allocator, str, true);
    }

    fn internImpl(self: *Interner, allocator: std.mem.Allocator, str: []const u8, comptime is_static: bool) !Symbol {
        const short_len = 7;
        if (str.len <= short_len) {
            @branchHint(.likely);
            var data = [8]u8{ 0, 0, 0, 0, 0, 0, 0, @intCast(str.len) };
            @memcpy(data[0..str.len], str);
            return Symbol{ .data = std.mem.readInt(u64, &data, .little) };
        }
        if (self.string_to_symbol.get(str)) |interned| {
            return interned;
        }
        const owned_string = if (is_static)
            str
        else
            try self.arena.allocator().dupe(u8, str);
        const id: u64 = @intCast(self.symbol_to_string.items.len);
        const interned = Symbol{ .data = id };
        try self.symbol_to_string.append(allocator, owned_string);
        try self.string_to_symbol.put(allocator, owned_string, interned);
        return interned;
    }

    pub fn formatted(self: Interner, symbol: Symbol) struct {
        interner: Interner,
        symbol: Symbol,
        pub fn format(this: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            var buf: [8]u8 = undefined;
            std.mem.writeInt(u64, &buf, this.symbol.data, .little);
            const short_len = buf[7];
            switch (short_len) {
                // Not a short symbol so the string is interned.
                0 => {
                    const id = this.symbol.data;
                    if (id >= this.interner.symbol_to_string.items.len) {
                        return writer.print("#<symbol-{}>", .{id});
                    }
                    return writer.print("{s}", .{this.interner.symbol_to_string.items[id]});
                },
                // A short symbol, decode properly.
                else => return writer.print("{s}", .{buf[0..short_len]}),
            }
        }
    } {
        return .{ .interner = self, .symbol = symbol };
    }
};
