const std = @import("std");

const Cons = @import("Cons.zig");
const Module = @import("Module.zig");
const Slot = @import("Slot.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const Builder = @This();

vm: *Vm,

pub fn init(vm: *Vm) Builder {
    return Builder{ .vm = vm };
}

pub inline fn makeInt(_: Builder, value: i64) Val {
    return Val{ .data = .{ .int = value } };
}

pub inline fn makeEmptyList(_: Builder) Val {
    return Val{ .data = .{ .empty_list = {} } };
}

pub inline fn makeCons(self: Builder, car: Val, cdr: Val) Vm.Error!Val {
    const h = try self.vm.objects.cons.put(
        self.vm.allocator(),
        Cons{ .car = car, .cdr = cdr },
    );
    const val = Val{ .data = .{ .pair = h } };
    return val;
}

pub inline fn makeList(self: Builder, items: []const Val) Vm.Error!Val {
    var result = self.makeEmptyList();
    var index: usize = items.len;
    while (index > 0) {
        index -= 1;
        result = try self.makeCons(items[index], result);
    }
    return result;
}

// TODO: Take the cdr as an argument.
pub inline fn makeImproperList(self: Builder, items: []const Val) Vm.Error!Val {
    if (items.len < 2) return Vm.Error.ReadError;
    var result = items[items.len - 1];
    var index: usize = items.len - 1;
    while (index > 0) {
        index -= 1;
        result = try self.makeCons(items[index], result);
    }
    return result;
}

/// Creates a symbol value, copying the symbol's string data.
/// The input symbol's lifetime does not need to extend beyond this function call,
/// as the string data is copied and managed by the VM's allocator.
pub inline fn makeSymbol(self: Builder, symbol: Symbol) Vm.Error!Val {
    const interned = try self.vm.objects.symbols.intern(self.vm.allocator(), symbol);
    return self.makeSymbolFromInterned(interned);
}

pub inline fn makeSymbolFromInterned(_: Builder, interned: Symbol.Interned) Val {
    return Val{ .data = .{ .symbol = interned } };
}

pub inline fn makeEnvironment(
    self: Builder,
    namespace: []const Symbol.Interned,
    slot_symbols: []const Symbol.Interned,
) Vm.Error!Val {
    // Create
    const h = try self.vm.objects.modules.put(self.vm.allocator(), Module{});
    const module = self.vm.objects.modules.get(h) orelse return Vm.Error.Unreachable;

    // Initialize
    module.namespace = try self.vm.allocator().dupe(Symbol.Interned, namespace);
    for (slot_symbols, 0..) |sym, idx| {
        const slot = Slot{ .idx = idx };
        try module.slots.append(self.makeEmptyList());
        try module.symbol_to_slot.put(self.vm.allocator(), sym, slot);
    }

    // Return
    const val = Val{ .data = .{ .module = h } };
    return val;
}
