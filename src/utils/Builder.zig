const std = @import("std");

const Context = @import("../Context.zig");
const Instruction = @import("../instruction.zig").Instruction;
const Closure = @import("../types/Closure.zig");
const Continuation = @import("../types/Continuation.zig");
const Module = @import("../types/Module.zig");
const Handle = @import("../types/object_pool.zig").Handle;
const Pair = @import("../types/Pair.zig");
const Proc = @import("../types/Proc.zig");
const String = @import("../types/String.zig");
const Symbol = @import("../types/Symbol.zig");
const SyntaxRules = @import("../types/SyntaxRules.zig");
const Val = @import("../types/Val.zig");
const Vector = Val.Vector;
const ByteVector = Val.ByteVector;
const Vm = @import("../Vm.zig");

const Builder = @This();

vm: *Vm,

pub fn init(vm: *Vm) Builder {
    return Builder{ .vm = vm };
}

pub inline fn makePair(self: Builder, car: Val, cdr: Val) error{OutOfMemory}!Val {
    const h = try self.vm.objects.pairs.put(
        self.vm.allocator(),
        Pair{ .car = car, .cdr = cdr },
    );
    const val = Val{ .data = .{ .pair = h } };
    return val;
}

pub inline fn makeList(self: Builder, items: []const Val) error{OutOfMemory}!Val {
    return self.makePairsWithCdr(items, Val.initEmptyList());
}

pub inline fn makePairs(self: Builder, items: []const Val) error{ ReadError, OutOfMemory }!Val {
    const len = items.len;
    if (len < 2) return error.ReadError;
    return self.makePairsWithCdr(items[0 .. len - 1], items[len - 1]);
}

pub inline fn makePairsWithCdr(self: Builder, items: []const Val, cdr: Val) error{OutOfMemory}!Val {
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
pub inline fn makeSymbol(self: Builder, symbol: []const u8) error{OutOfMemory}!Val {
    return Val.initSymbol(try self.makeSymbolHandle(symbol));
}

pub inline fn makeStaticSymbol(self: Builder, comptime symbol: []const u8) error{OutOfMemory}!Val {
    return Val.initSymbol(try self.makeStaticSymbolHandle(symbol));
}

pub inline fn makeSymbolHandle(self: Builder, symbol: []const u8) error{OutOfMemory}!Symbol {
    return try self.vm.objects.symbols.intern(self.vm.allocator(), symbol);
}

pub inline fn makeStaticSymbolHandle(self: Builder, comptime symbol: []const u8) error{OutOfMemory}!Symbol {
    return try self.vm.objects.symbols.internStatic(self.vm.allocator(), symbol);
}

pub inline fn makeString(self: Builder, s: []const u8) error{OutOfMemory}!Val {
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

pub inline fn makeVector(self: Builder, vals: []const Val) error{OutOfMemory}!Val {
    const h = try self.makeVectorHandle(vals);
    return Val{ .data = .{ .vector = h } };
}

pub inline fn makeVectorHandle(self: Builder, vals: []const Val) error{OutOfMemory}!Handle(Vector) {
    const copy = try self.vm.allocator().dupe(Val, vals);
    errdefer self.vm.allocator().free(copy);
    const vec = Vector.fromOwnedSlice(copy);
    return try self.vm.objects.vectors.put(self.vm.allocator(), vec);
}

pub inline fn makeBytevector(self: Builder, bytes: []const u8) error{OutOfMemory}!Val {
    const h = try self.makeBytevectorHandle(bytes);
    return Val{ .data = .{ .bytevector = h } };
}

pub inline fn makeBytevectorHandle(self: Builder, bytes: []const u8) error{OutOfMemory}!Handle(ByteVector) {
    const copy = try self.vm.allocator().dupe(u8, bytes);
    errdefer self.vm.allocator().free(copy);
    const bv = ByteVector.fromOwnedSlice(copy);
    return try self.vm.objects.bytevectors.put(self.vm.allocator(), bv);
}

pub inline fn makeContinuationHandle(self: Builder, context: Context) error{OutOfMemory}!Handle(Continuation) {
    var cont = try Continuation.init(self.vm.allocator(), context);
    errdefer cont.deinit(self.vm.allocator());
    return try self.vm.objects.continuations.put(self.vm.allocator(), cont);
}

pub inline fn makeContinuation(self: Builder, context: Context) error{OutOfMemory}!Val {
    const h = try self.makeContinuationHandle(context);
    return Val{ .data = .{ .continuation = h } };
}

pub inline fn makeSyntaxRulesHandle(self: Builder, syntax_rules: SyntaxRules) error{OutOfMemory}!Handle(SyntaxRules) {
    return try self.vm.objects.syntax_rules.put(self.vm.allocator(), syntax_rules);
}

pub inline fn makeSyntaxRules(self: Builder, syntax_rules: SyntaxRules) !Val {
    const h = try self.makeSyntaxRulesHandle(syntax_rules);
    return Val{ .data = .{ .syntax_rules = h } };
}

pub inline fn makeClosure(self: Builder, base_proc: Proc, caps: []const Val) error{OutOfMemory}!Handle(Closure) {
    const allocator = self.vm.allocator();
    const instructions = try allocator.dupe(Instruction, base_proc.instructions);
    errdefer allocator.free(instructions);
    const captures = try allocator.dupe(Val, caps);
    errdefer allocator.free(captures);
    const closure = Closure{
        .instructions = instructions,
        .captures = captures,
        .name = base_proc.name,
        .arg_count = base_proc.arg_count,
        .locals_count = base_proc.locals_count,
    };
    return try self.makeClosureHandle(closure);
}

pub inline fn makeClosureHandle(self: Builder, closure: Closure) error{OutOfMemory}!Handle(Closure) {
    return try self.vm.objects.closures.put(self.vm.allocator(), closure);
}
