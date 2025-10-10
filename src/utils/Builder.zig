const std = @import("std");

const Context = @import("../Context.zig");
const Continuation = @import("../types/Continuation.zig");
const Module = @import("../types/Module.zig");
const Handle = @import("../types/object_pool.zig").Handle;
const Pair = @import("../types/Pair.zig");
const String = @import("../types/String.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vector = @import("../types/Vector.zig");
const Vm = @import("../Vm.zig");

const Builder = @This();

vm: *Vm,

pub fn init(vm: *Vm) Builder {
    return Builder{ .vm = vm };
}

pub inline fn makePair(self: Builder, car: Val, cdr: Val) Vm.Error!Val {
    const h = try self.vm.objects.pairs.put(
        self.vm.allocator(),
        Pair{ .car = car, .cdr = cdr },
    );
    const val = Val{ .data = .{ .pair = h } };
    return val;
}

pub inline fn makeList(self: Builder, items: []const Val) Vm.Error!Val {
    return self.makePairsWithCdr(items, Val.initEmptyList());
}

pub inline fn makePairs(self: Builder, items: []const Val) Vm.Error!Val {
    const len = items.len;
    if (len < 2) return Vm.Error.ReadError;
    return self.makePairsWithCdr(items[0 .. len - 1], items[len - 1]);
}

pub inline fn makePairsWithCdr(self: Builder, items: []const Val, cdr: Val) Vm.Error!Val {
    var result = cdr;
    var index: usize = items.len;
    while (index > 0) {
        index -= 1;
        result = try self.makePair(items[index], result);
    }
    return result;
}

/// Creates a symbol value, copying the symbol's string data.
/// The input symbol's lifetime does not need to extend beyond this function call,
/// as the string data is copied and managed by the VM's allocator.
pub inline fn makeSymbol(self: Builder, symbol: []const u8) Vm.Error!Val {
    return Val.initSymbol(try self.makeSymbolHandle(symbol));
}

pub inline fn makeStaticSymbol(self: Builder, symbol: []const u8) Vm.Error!Val {
    return Val.initSymbol(try self.makeStaticSymbolHandle(symbol));
}

pub inline fn makeSymbolHandle(self: Builder, symbol: []const u8) error{OutOfMemory}!Symbol {
    return try self.vm.objects.symbols.intern(self.vm.allocator(), symbol);
}

pub inline fn makeStaticSymbolHandle(self: Builder, symbol: []const u8) error{OutOfMemory}!Symbol {
    return try self.vm.objects.symbols.internStatic(self.vm.allocator(), symbol);
}

pub inline fn makeString(self: Builder, s: []const u8) Vm.Error!Val {
    const string = try String.init(self.vm.allocator(), s);
    const h = try self.vm.objects.strings.put(self.vm.allocator(), string);
    return Val{ .data = .{ .string = h } };
}

pub const Definition = struct {
    symbol: Symbol,
    value: Val,
};

pub inline fn makeEnvironment(
    self: Builder,
    namespace: []const Symbol,
    definitions: []const Definition,
) Vm.Error!Handle(Module) {
    // Create
    const h = try self.vm.objects.modules.put(self.vm.allocator(), Module{});
    const module = self.vm.objects.modules.get(h) orelse return Vm.Error.Unreachable;

    // Initialize
    module.namespace = try self.vm.allocator().dupe(Symbol, namespace);
    for (definitions, 0..definitions.len) |def, idx| {
        const slot: u32 = @intCast(idx);
        try module.slots.append(self.vm.allocator(), def.value);
        try module.symbol_to_slot.put(self.vm.allocator(), def.symbol, slot);
    }

    // Return
    return h;
}

pub inline fn makeVector(self: Builder, vals: []const Val) Vm.Error!Val {
    const h = try self.makeVectorHandle(vals);
    return Val{ .data = .{ .vector = h } };
}

pub inline fn makeVectorHandle(self: Builder, vals: []const Val) Vm.Error!Handle(Vector) {
    const copy = try self.vm.allocator().dupe(Val, vals);
    errdefer self.vm.allocator().free(copy);
    const vec = Vector{ .data = std.ArrayList(Val).fromOwnedSlice(copy) };
    return try self.vm.objects.vectors.put(self.vm.allocator(), vec);
}

pub inline fn makeContinuationHandle(self: Builder, context: Context) Vm.Error!Handle(Continuation) {
    var cont = try Continuation.init(self.vm.allocator(), context);
    errdefer cont.deinit(self.vm.allocator());
    return try self.vm.objects.continuations.put(self.vm.allocator(), cont);
}

pub inline fn makeContinuation(self: Builder, context: Context) Vm.Error!Val {
    const h = try self.makeContinuationHandle(context);
    return Val{ .data = .{ .continuation = h } };
}
