//! Value representation system for the Scheme interpreter.
//!
//! This module defines the core value types that can be stored and manipulated
//! in the Scheme interpreter. Values are represented using a tagged union to
//! efficiently handle the dynamic typing inherent in Scheme. Supports nil,
//! booleans, integers, symbols, pairs, and procedures.

const std = @import("std");
const testing = std.testing;

const object_pool = @import("object_pool.zig");
const Handle = object_pool.Handle;
const Pair = @import("Pair.zig");
const Procedure = @import("Procedure.zig");
const Symbol = @import("Symbol.zig");
const Vm = @import("Vm.zig");

const Val = @This();

/// The internal representation of this value.
/// Uses a tagged union to store different value types efficiently.
repr: Repr,

/// Tagged union representing all possible value types in the Scheme interpreter.
/// Each variant corresponds to a different Scheme data type that can be
/// stored and manipulated at runtime.
pub const Repr = union(enum) {
    /// Represents the end of a list (equivalent to '() or nil in Scheme).
    /// This is used to terminate linked list structures.
    nil,

    /// Represents a boolean value.
    boolean: bool,

    /// Represents a 64-bit signed integer value.
    /// Used for numeric computations and integer literals in Scheme.
    i64: i64,

    /// Represents a symbol value using an interned symbol for efficient comparison.
    /// Symbols in Scheme are identifiers that evaluate to themselves when quoted
    /// or are used for variable/function names when unquoted.
    symbol: Symbol.Interned,

    /// Represents a pair using a handle to an object pool.
    ///
    /// Pairs are the fundamental building blocks for lists and pairs in Scheme.
    pair: Handle(Pair),

    /// Represents a procedure (function) that can be called in the interpreter.
    /// Procedures include both built-in functions and user-defined functions.
    procedure: Handle(Procedure),
};

/// Create a new `Val` for a supported type.
///
/// Supported primitive types:
///   - Val, Val.Repr: Pass-through values
///   - void: Converted to nil
///   - bool: Converted to boolean values
///   - i64, comptime_int: Converted to integer values
///   - Symbol.Interned: Converted to symbol values
///   - Handle(Pair): Converted to pair values
///   - Handle(Procedure): Converted to procedure values
///
/// For more complex types like Symbol, Pair, or Procedure structs, use `Vm.builder()`.
pub fn init(v: anytype) Val {
    const type_info = @TypeOf(v);
    switch (type_info) {
        Val => return v,
        Val.Repr => return Val{ .repr = v },
        void => return init(Val.Repr{ .nil = {} }),
        bool => return init(Val.Repr{ .boolean = v }),
        i64, comptime_int => return init(Val.Repr{ .i64 = v }),
        Symbol.Interned => return init(Val.Repr{ .symbol = v }),
        Handle(Pair) => return init(Val.Repr{ .pair = v }),
        Handle(Procedure) => return init(Val.Repr{ .procedure = v }),
        else => @compileError("type " ++ @typeName(type_info) ++ " not supported for Val.init."),
    }
}

/// Determines if a value is truthy according to Scheme semantics.
/// In Scheme, only the boolean value false (#f) is considered falsy.
/// All other values, including nil/empty list, numbers, symbols, and cons cells are truthy.
///
/// Args:
///   self: The value to test for truthiness.
///
/// Returns:
///   true if the value is truthy, false if falsy.
pub fn isTruthy(self: Val) bool {
    return switch (self.repr) {
        .boolean => |b| b,
        else => true,
    };
}

test "isTruthy returns false for boolean false" {
    const val = Val{ .repr = .{ .boolean = false } };
    try testing.expectEqual(false, val.isTruthy());
}

test "isTruthy returns true for boolean true" {
    const val = Val{ .repr = .{ .boolean = true } };
    try testing.expectEqual(true, val.isTruthy());
}

test "isTruthy returns true for nil" {
    const val = Val{ .repr = .{ .nil = {} } };
    try testing.expectEqual(true, val.isTruthy());
}

test "isTruthy returns true for numbers" {
    const val_zero = Val{ .repr = .{ .i64 = 0 } };
    const val_positive = Val{ .repr = .{ .i64 = 42 } };
    const val_negative = Val{ .repr = .{ .i64 = -5 } };

    try testing.expectEqual(true, val_zero.isTruthy());
    try testing.expectEqual(true, val_positive.isTruthy());
    try testing.expectEqual(true, val_negative.isTruthy());
}

test "isTruthy returns true for symbols" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = try vm.interner.internStatic(@import("Symbol.zig").init("test"));
    const val = Val{ .repr = .{ .symbol = symbol } };

    try testing.expectEqual(true, val.isTruthy());
}

test "isTruthy returns true for pair" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const cons_val = try vm.toVal(@import("Pair.zig"){
        .car = Val{ .repr = .{ .i64 = 1 } },
        .cdr = Val{ .repr = .{ .nil = {} } },
    });

    try testing.expectEqual(true, cons_val.isTruthy());
}

test "isTruthy returns true for procedure" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const test_procedure = Procedure{
        .name = try vm.interner.internStatic(Symbol.init("test-proc")),
        .implementation = .{ .native = .{ .func = struct {
            fn testFunc(vm_ptr: *Vm) Val {
                _ = vm_ptr;
                return Val{ .repr = .{ .nil = {} } };
            }
        }.testFunc } },
    };
    const procedure_handle = try vm.toVal(test_procedure);

    try testing.expectEqual(true, procedure_handle.isTruthy());
}
