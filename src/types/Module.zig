const std = @import("std");
const testing = std.testing;

const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("../Vm.zig");

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
    if (self.slots.items.len >= idx) return Vm.Error.UndefinedBehavior;
    self.slots.items[idx] = val;
}

pub fn setBySymbol(self: *Module, vm: *Vm, sym: Symbol.Interned, val: Val) Vm.Error!void {
    if (self.symbol_to_slot.get(sym)) |slot| {
        return self.set(self, slot, val);
    }
    const slot: u32 = @intCast(self.slots.items.len);
    try self.slots.append(vm.allocator(), val);
    try self.symbol_to_slot.put(vm.allocator(), sym, slot);
}
