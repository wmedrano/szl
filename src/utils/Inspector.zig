const std = @import("std");

const Box = @import("../types/Box.zig");
const Continuation = @import("../types/Continuation.zig");
const ErrorDetails = @import("../types/ErrorDetails.zig");
const Module = @import("../types/Module.zig");
const Handle = @import("../types/object_pool.zig").Handle;
const Pair = @import("../types/Pair.zig");
const Parameter = @import("../types/Parameter.zig");
const Port = @import("../types/Port.zig");
const Proc = @import("../types/Proc.zig");
const Record = @import("../types/Record.zig");
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
        else => Vm.Error.UncaughtException,
    };
}

pub inline fn asPair(self: Inspector, val: Val) Vm.Error!*Pair {
    return switch (val.data) {
        .pair => |h| self.vm.objects.pairs.get(h) orelse return Vm.Error.UndefinedBehavior,
        else => Vm.Error.UncaughtException,
    };
}

pub inline fn asSymbol(_: Inspector, val: Val) Vm.Error!Symbol {
    return switch (val.data) {
        .symbol => |sym| sym,
        else => Vm.Error.UncaughtException,
    };
}

pub inline fn asString(self: Inspector, val: Val) Vm.Error!*String {
    return switch (val.data) {
        .string => |h| self.vm.objects.strings.get(h) orelse return Vm.Error.UndefinedBehavior,
        else => Vm.Error.UncaughtException,
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
    ImproperList,
    UndefinedBehavior,
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
                    return AsListError.UndefinedBehavior;
                try items.append(allocator, pair.car);
                current = pair.cdr;
            },
            else => return AsListError.ImproperList,
        }
    }

    return try items.toOwnedSlice(allocator);
}

pub fn listToSliceExact(
    self: Inspector,
    val: Val,
    len: comptime_int,
) error{ UncaughtException, BadLength, UndefinedBehavior }![len]Val {
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
            else => return Vm.Error.UncaughtException,
        }
    }

    if (actual_len != len) return error.BadLength;
    return items;
}

pub inline fn handleToProc(self: Inspector, h: Handle(Proc)) Vm.Error!*Proc {
    return self.vm.objects.procs.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn handleToContinuation(self: Inspector, h: Handle(Continuation)) error{UndefinedBehavior}!*Continuation {
    return self.vm.objects.continuations.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn handleToVector(self: Inspector, h: Handle(Vector)) error{UndefinedBehavior}!*Vector {
    return self.vm.objects.vectors.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn handleToBytevector(self: Inspector, h: Handle(ByteVector)) error{UndefinedBehavior}!*ByteVector {
    return self.vm.objects.bytevectors.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn handleToBox(self: Inspector, h: Handle(Box)) error{UndefinedBehavior}!*Box {
    return self.vm.objects.boxes.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn handleToModule(self: Inspector, h: Handle(Module)) error{UndefinedBehavior}!*Module {
    return self.vm.objects.modules.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn handleToRecord(self: Inspector, h: Handle(Record)) Vm.Error!*Record {
    return self.vm.objects.records.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn handleToRecordDescriptor(self: Inspector, h: Handle(Record.Descriptor)) Vm.Error!*Record.Descriptor {
    return self.vm.objects.record_descriptors.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn handleToParameter(self: Inspector, h: Handle(Parameter)) Vm.Error!*Parameter {
    return self.vm.objects.parameters.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn resolveParameter(self: Inspector, h: Handle(Parameter)) Vm.Error!Val {
    return self.vm.context.resolveParameter(self.vm, h);
}

pub inline fn asPort(self: Inspector, val: Val) Vm.Error!*Port {
    return switch (val.data) {
        .port => |h| self.vm.objects.ports.get(h) orelse return Vm.Error.UndefinedBehavior,
        else => Vm.Error.UncaughtException,
    };
}

pub inline fn handleToPort(self: Inspector, h: Handle(Port)) Vm.Error!*Port {
    return self.vm.objects.ports.get(h) orelse return Vm.Error.UndefinedBehavior;
}

pub inline fn asErrorDetails(self: Inspector, val: Val) Vm.Error!*ErrorDetails {
    return switch (val.data) {
        .error_details => |h| self.vm.objects.error_details.get(h) orelse return Vm.Error.UndefinedBehavior,
        else => Vm.Error.UncaughtException,
    };
}

pub inline fn handleToErrorDetails(self: Inspector, h: Handle(ErrorDetails)) Vm.Error!*ErrorDetails {
    return self.vm.objects.error_details.get(h) orelse return Vm.Error.UndefinedBehavior;
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

pub fn getReplEnv(self: Inspector, diagnostics: *ErrorDetails) error{ UndefinedBehavior, OutOfMemory }!Handle(Module) {
    const b = self.vm.builder();
    const module = self.findModule(&.{
        try b.makeStaticSymbolHandle("user"),
        try b.makeStaticSymbolHandle("repl"),
    });
    if (module) |m| {
        @branchHint(.likely);
        return m;
    }
    diagnostics.addDiagnostic(self.vm.allocator(), .{ .undefined_behavior = "Could not find (user repl) module" });
    return Vm.Error.UndefinedBehavior;
}

/// Iterator for traversing Scheme-style linked lists (pairs).
/// Iterates through the car of each pair until reaching an empty list.
pub const PairIterator = struct {
    inspector: Inspector,
    current: Val,

    /// Returns the car of the current pair and advances to the cdr.
    /// Returns null when reaching an empty list.
    /// Returns UncaughtException error if current value is not a pair or empty list.
    pub fn next(self: *PairIterator) error{ UndefinedBehavior, UncaughtException }!?Val {
        return switch (self.current.data) {
            .empty_list => null,
            .pair => |h| {
                const pair = self.inspector.vm.objects.pairs.get(h) orelse
                    return Vm.Error.UndefinedBehavior;
                const result = pair.car;
                self.current = pair.cdr;
                return result;
            },
            else => Vm.Error.UncaughtException,
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
