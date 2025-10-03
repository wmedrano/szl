const std = @import("std");

const Cons = @import("Cons.zig");
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
    var cons = try self.vm.allocator().create(Cons);
    cons.car = car;
    cons.cdr = cdr;
    const val = Val{ .data = .{ .pair = cons } };
    try self.vm.objects.append(self.vm.allocator(), val);
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

pub inline fn makeSymbol(self: Builder, symbol_str: []const u8) Vm.Error!Val {
    if (self.vm.symbols.get(symbol_str)) |s| return Val{ .data = .{ .symbol = s } };
    const sym = Symbol{ .string = try self.vm.allocator().dupe(u8, symbol_str) };
    errdefer self.vm.allocator().free(sym.string);
    try self.vm.symbols.put(self.vm.allocator(), sym.string, sym);
    return Val{ .data = .{ .symbol = sym } };
}
