const std = @import("std");
const testing = std.testing;

const Vm = @import("../Vm.zig");
const Handle = @import("object_pool.zig").Handle;
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");

const Module = @This();

pub const Slot = packed struct {
    idx: u32,
};

namespace: []const Symbol = &.{},
slots: std.ArrayList(Val) = .{},
symbol_to_slot: std.AutoHashMapUnmanaged(Symbol, Slot) = .{},

pub fn deinit(self: *Module, allocator: std.mem.Allocator) void {
    allocator.free(self.namespace);
    self.slots.deinit(allocator);
    self.symbol_to_slot.deinit(allocator);
}

pub fn getOrCreateSlot(self: *Module, allocator: std.mem.Allocator, sym: Symbol, default_value: Val) !Slot {
    if (self.getSlot(sym)) |slot| return slot;
    const slot = Slot{ .idx = @intCast(self.slots.items.len) };
    try self.slots.append(allocator, default_value);
    try self.symbol_to_slot.put(allocator, sym, slot);
    return slot;
}

pub fn getSlot(self: *Module, sym: Symbol) ?Slot {
    return self.symbol_to_slot.get(sym);
}

pub fn get(self: Module, slot: Slot) ?Val {
    return self.slots.items[@intCast(slot.idx)];
}

pub fn getBySymbol(self: Module, sym: Symbol) ?Val {
    const slot = self.symbol_to_slot.get(sym) orelse return null;
    const idx: usize = @intCast(slot.idx);
    return self.slots.items[idx];
}

pub fn setBySymbol(self: *Module, allocator: std.mem.Allocator, sym: Symbol, val: Val) Vm.Error!void {
    if (self.symbol_to_slot.get(sym)) |slot| {
        const idx: usize = @intCast(slot.idx);
        if (idx < self.slots.items.len)
            self.slots.items[idx] = val
        else
            return Vm.Error.UndefinedBehavior;
    }
    const slot = Slot{ .idx = @intCast(self.slots.items.len) };
    try self.slots.append(allocator, val);
    try self.symbol_to_slot.put(allocator, sym, slot);
}

pub fn set(self: *Module, slot: Slot, val: Val) Vm.Error!void {
    const idx: usize = @intCast(slot.idx);
    if (idx < self.slots.items.len)
        self.slots.items[idx] = val
    else
        return Vm.Error.UndefinedBehavior;
}

pub fn import(self: *Module, allocator: std.mem.Allocator, other: Module) Vm.Error!void {
    try self.symbol_to_slot.ensureUnusedCapacity(allocator, other.symbol_to_slot.size);
    try self.slots.ensureUnusedCapacity(allocator, other.slots.items.len);
    var symbol_slots_iter = other.symbol_to_slot.iterator();
    while (symbol_slots_iter.next()) |symbol_slot| {
        const val = other.get(symbol_slot.value_ptr.*) orelse
            return Vm.Error.UndefinedBehavior;
        try self.setBySymbol(allocator, symbol_slot.key_ptr.*, val);
    }
}
