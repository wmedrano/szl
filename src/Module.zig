const std = @import("std");
const testing = std.testing;

const Slot = @import("Slot.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const Module = @This();

namespace: []const Symbol,
slots: std.ArrayList(Val) = .{},
symbol_to_slot: std.AutoHashMapUnmanaged(Symbol, Slot) = .{},

pub fn deinit(self: *Module, allocator: std.mem.Allocator) void {
    allocator.free(self.namespace);
    self.slots.deinit(allocator);
    self.symbol_to_slot.deinit(allocator);
}

pub fn get(self: *Module, slot: Slot) Vm.Error!Val {
    const idx: usize = slot.idx();
    if (idx >= self.slots.items.len) return Vm.Error.UndefinedBehavior;
    return self.slots.items[idx];
}

pub fn getBySymbol(self: *Module, sym: Symbol) ?Val {
    const slot = self.symbol_to_slot.get(sym) orelse return null;
    return self.get(slot);
}

pub fn set(self: *Module, slot: Slot, val: Val) Vm.Error!void {
    const idx = slot.idx();
    if (self.slots.items.len >= idx) return Vm.Error.UndefinedBehavior;
    self.slots.items[idx] = val;
}

pub fn setBySymbol(self: *Module, vm: *Vm, sym: Symbol, val: Val) Vm.Error!void {
    if (self.symbol_to_slot(sym)) |slot| {
        return self.set(vm, slot, val);
    }
    const slot = Slot{ .index = self.slots.items.len };
    try self.slots.append(vm.allocator(), val);
    try self.symbol_to_slot.put(vm.allocator(), sym, slot);
}
