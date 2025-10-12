const std = @import("std");

const Closure = @import("../types/Closure.zig");
const Continuation = @import("../types/Continuation.zig");
const Module = @import("../types/Module.zig");
const Handle = @import("../types/object_pool.zig").Handle;
const Pair = @import("../types/Pair.zig");
const Proc = @import("../types/Proc.zig");
const String = @import("../types/String.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vector = Val.Vector;
const ByteVector = Val.ByteVector;
const Vm = @import("../Vm.zig");

const Inspector = @This();

vm: *Vm,

pub fn init(vm: *Vm) Inspector {
    return Inspector{ .vm = vm };
}

pub inline fn asModule(_: Inspector, val: Val) Vm.Error!*Module {
    return switch (val.data) {
        .module => |env| env,
        else => Vm.Error.WrongType,
    };
}

pub inline fn asPair(self: Inspector, val: Val) Vm.Error!*Pair {
    return switch (val.data) {
        .pair => |h| self.vm.objects.pairs.get(h) orelse return Vm.Error.UndefinedBehavior,
        else => Vm.Error.WrongType,
    };
}

pub inline fn asSymbol(_: Inspector, val: Val) Vm.Error!Symbol {
    return switch (val.data) {
        .symbol => |sym| sym,
        else => Vm.Error.WrongType,
    };
}

pub inline fn asString(self: Inspector, val: Val) Vm.Error!*String {
    return switch (val.data) {
        .string => |h| self.vm.objects.strings.get(h) orelse return Vm.Error.UndefinedBehavior,
        else => Vm.Error.WrongType,
    };
}

pub inline fn handleToString(self: Inspector, h: Handle(String)) Vm.Error!*String {
    return self.vm.objects.strings.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn handleToPair(self: Inspector, h: Handle(Pair)) Vm.Error!*Pair {
    return self.vm.objects.pairs.get(h) orelse return Vm.Error.UndefinedBehavior;
}

const AsListError = error{
    OutOfMemory,
    UndefinedBehavior,
    WrongType,
};

pub fn listToSliceAlloc(self: Inspector, allocator: std.mem.Allocator, val: Val) AsListError![]Val {
    var items = std.ArrayList(Val){};
    errdefer items.deinit(allocator);

    var current = val;
    while (true) {
        switch (current.data) {
            .empty_list => break,
            .pair => |h| {
                const pair = self.vm.objects.pairs.get(h) orelse
                    return Vm.Error.UndefinedBehavior;
                try items.append(allocator, pair.car);
                current = pair.cdr;
            },
            else => return Vm.Error.WrongType,
        }
    }

    return try items.toOwnedSlice(allocator);
}

pub fn listToSliceExact(
    self: Inspector,
    val: Val,
    len: comptime_int,
) error{ WrongType, BadLength, UndefinedBehavior }![len]Val {
    var items: [len]Val = undefined;
    var actual_len: usize = 0;

    var current = val;
    while (true) {
        switch (current.data) {
            .empty_list => break,
            .pair => |h| {
                if (actual_len == len) return error.BadLength;
                const pair = self.vm.objects.pairs.get(h) orelse
                    return Vm.Error.UndefinedBehavior;
                items[actual_len] = pair.car;
                actual_len += 1;
                current = pair.cdr;
            },
            else => return Vm.Error.WrongType,
        }
    }

    if (actual_len != len) return error.BadLength;
    return items;
}

pub inline fn handleToProc(self: Inspector, h: Handle(Proc)) Vm.Error!*Proc {
    return self.vm.objects.procs.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn handleToClosure(self: Inspector, h: Handle(Closure)) Vm.Error!*Closure {
    return self.vm.objects.closures.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn handleToContinuation(self: Inspector, h: Handle(Continuation)) Vm.Error!*Continuation {
    return self.vm.objects.continuations.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn handleToVector(self: Inspector, h: Handle(Vector)) Vm.Error!*Vector {
    return self.vm.objects.vectors.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn handleToBytevector(self: Inspector, h: Handle(ByteVector)) Vm.Error!*ByteVector {
    return self.vm.objects.bytevectors.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn handleToModule(self: Inspector, h: Handle(Module)) Vm.Error!*Module {
    return self.vm.objects.modules.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub fn findModule(self: Inspector, path: []const Symbol) ?Handle(Module) {
    var moduleIter = self.vm.objects.modules.iterator();
    while (moduleIter.next()) |module| {
        if (pathEq(module.value.namespace, path))
            return module.handle;
    }
    return null;
}

fn pathEq(a: []const Symbol, b: []const Symbol) bool {
    if (a.len != b.len) return false;
    for (a, b) |x, y| {
        if (!x.eq(y)) return false;
    }
    return true;
}

pub fn getReplEnv(self: Inspector) Vm.Error!Handle(Module) {
    const b = self.vm.builder();
    return self.findModule(&.{
        try b.makeStaticSymbolHandle("user"),
        try b.makeStaticSymbolHandle("repl"),
    }) orelse return Vm.Error.Unreachable;
}

/// Iterator for traversing Scheme-style linked lists (pairs).
/// Iterates through the car of each pair until reaching an empty list.
pub const PairIterator = struct {
    inspector: Inspector,
    current: Val,

    /// Returns the car of the current pair and advances to the cdr.
    /// Returns null when reaching an empty list.
    /// Returns WrongType error if current value is not a pair or empty list.
    pub fn next(self: *PairIterator) Vm.Error!?Val {
        return switch (self.current.data) {
            .empty_list => null,
            .pair => |h| {
                const pair = self.inspector.vm.objects.pairs.get(h) orelse
                    return Vm.Error.UndefinedBehavior;
                const result = pair.car;
                self.current = pair.cdr;
                return result;
            },
            else => Vm.Error.WrongType,
        };
    }
};

/// Creates an iterator for traversing a linked list of pairs.
/// The iterator yields each car value in sequence until reaching an empty list.
pub fn iteratePairs(self: Inspector, val: Val) PairIterator {
    return PairIterator{
        .inspector = self,
        .current = val,
    };
}

/// Checks if two values are equal according to Scheme's equal? predicate.
/// This performs deep equality checking for pairs, vectors, and strings.
/// For other types, it uses structural equality (same as eq?).
pub fn isEqual(self: Inspector, a: Val, b: Val) Vm.Error!bool {
    // Fast path: if they're structurally equal, they're equal
    if (a.eq(b)) return true;

    // Check type-specific deep equality
    switch (a.data) {
        .pair => |a_handle| {
            switch (b.data) {
                .pair => |b_handle| {
                    const a_pair = try self.handleToPair(a_handle);
                    const b_pair = try self.handleToPair(b_handle);
                    return try self.isEqual(a_pair.car, b_pair.car) and
                        try self.isEqual(a_pair.cdr, b_pair.cdr);
                },
                else => return false,
            }
        },
        .vector => |a_handle| {
            switch (b.data) {
                .vector => |b_handle| {
                    const a_vec = try self.handleToVector(a_handle);
                    const b_vec = try self.handleToVector(b_handle);
                    const a_slice = a_vec.items;
                    const b_slice = b_vec.items;
                    if (a_slice.len != b_slice.len) return false;
                    for (a_slice, b_slice) |a_item, b_item| {
                        if (!try self.isEqual(a_item, b_item)) return false;
                    }
                    return true;
                },
                else => return false,
            }
        },
        .string => |a_handle| {
            switch (b.data) {
                .string => |b_handle| {
                    const a_str = try self.handleToString(a_handle);
                    const b_str = try self.handleToString(b_handle);
                    return std.mem.eql(u8, a_str.asSlice(), b_str.asSlice());
                },
                else => return false,
            }
        },
        else => return false,
    }
}
