const std = @import("std");
const testing = std.testing;

const Vm = @import("../Vm.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");

const Module = @This();

namespace: []const Symbol.Interned = &.{},
slots: std.ArrayList(Val) = .{},
symbol_to_slot: std.AutoHashMapUnmanaged(Symbol.Interned, u32) = .{},

pub fn deinit(self: *Module, allocator: std.mem.Allocator) void {
    allocator.free(self.namespace);
    self.slots.deinit(allocator);
    self.symbol_to_slot.deinit(allocator);
}

pub fn get(self: Module, slot: u32) Vm.Error!Val {
    const idx: usize = @intCast(slot);
    if (idx >= self.slots.items.len) return Vm.Error.UndefinedBehavior;
    return self.slots.items[idx];
}

pub fn getBySymbol(self: Module, sym: Symbol.Interned) ?Val {
    const slot = self.symbol_to_slot.get(sym) orelse return null;
    const idx: usize = @intCast(slot);
    return self.slots.items[idx];
}

pub fn set(self: *Module, slot: u32, val: Val) Vm.Error!void {
    const idx: usize = @intCast(slot);
    if (idx < self.slots.items.len)
        self.slots.items[idx] = val
    else
        return Vm.Error.UndefinedBehavior;
}

pub fn setBySymbol(self: *Module, allocator: std.mem.Allocator, sym: Symbol.Interned, val: Val) Vm.Error!void {
    if (self.symbol_to_slot.get(sym)) |slot| {
        return self.set(slot, val);
    }
    const slot: u32 = @intCast(self.slots.items.len);
    try self.slots.append(allocator, val);
    try self.symbol_to_slot.put(allocator, sym, slot);
}
