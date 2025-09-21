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

    /// Represents a 64-bit floating point value.
    /// Used for floating point numeric computations in Scheme.
    f64: f64,

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
///   - f64, comptime_float: Converted to floating point values
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
        f64, comptime_float => return init(Val.Repr{ .f64 = v }),
        Symbol.Interned => return init(Val.Repr{ .symbol = v }),
        Handle(Pair) => return init(Val.Repr{ .pair = v }),
        Handle(Procedure) => return init(Val.Repr{ .procedure = v }),
        else => @compileError("type " ++ @typeName(type_info) ++ " not supported for Val.init."),
    }
}

/// Determines if a value is nil.
///
/// Args:
///   self: The value to test for nil.
///
/// Returns:
///   true if the value is nil, false otherwise.
pub fn isNil(self: Val) bool {
    return switch (self.repr) {
        .nil => true,
        else => false,
    };
}

/// Determines if a value is a pair.
///
/// Args:
///   self: The value to test for pair type.
///
/// Returns:
///   true if the value is a pair, false otherwise.
pub fn isPair(self: Val) bool {
    return switch (self.repr) {
        .pair => true,
        else => false,
    };
}

/// Determines if a value is a procedure.
///
/// Args:
///   self: The value to test for procedure type.
///
/// Returns:
///   true if the value is a procedure, false otherwise.
pub fn isProcedure(self: Val) bool {
    return switch (self.repr) {
        .procedure => true,
        else => false,
    };
}

/// Determines if a value is a number (either integer or floating point).
/// Returns true for both i64 and f64 value types.
///
/// Args:
///   self: The value to test for numeric type.
///
/// Returns:
///   true if the value is an i64 or f64, false otherwise.
pub fn isNumber(self: Val) bool {
    return switch (self.repr) {
        .i64, .f64 => true,
        else => false,
    };
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
    const val = Val.init(false);
    try testing.expectEqual(false, val.isTruthy());
}

test "isTruthy returns true for boolean true" {
    const val = Val.init(true);
    try testing.expectEqual(true, val.isTruthy());
}

test "isTruthy returns true for nil" {
    const val = Val.init({});
    try testing.expectEqual(true, val.isTruthy());
}

test "isTruthy returns true for numbers" {
    const val_zero = Val.init(0);
    const val_positive = Val.init(42);
    const val_negative = Val.init(-5);

    try testing.expectEqual(true, val_zero.isTruthy());
    try testing.expectEqual(true, val_positive.isTruthy());
    try testing.expectEqual(true, val_negative.isTruthy());
}

test "isTruthy returns true for symbols" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = try vm.interner.internStatic(Symbol.init("test"));
    const val = Val.init(symbol);

    try testing.expectEqual(true, val.isTruthy());
}

test "isTruthy returns true for pair" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const cons_val = try vm.toVal(Pair{
        .car = Val.init(1),
        .cdr = Val.init({}),
    });

    try testing.expectEqual(true, cons_val.isTruthy());
}

test "isTruthy returns true for procedure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc = Procedure{
        .name = try vm.interner.internStatic(Symbol.init("test-proc")),
        .implementation = .{ .native = .{ .func = struct {
            fn func(_: Procedure.Context) Val {
                return Val.init({});
            }
        }.func } },
    };
    const proc_val = try vm.toVal(proc);

    try testing.expectEqual(true, proc_val.isTruthy());
}

test "isNil returns true for nil value" {
    const val = Val.init({});
    try testing.expectEqual(true, val.isNil());
}

test "isNil returns false for non-nil values" {
    const bool_val = Val.init(true);
    const int_val = Val.init(42);

    try testing.expectEqual(false, bool_val.isNil());
    try testing.expectEqual(false, int_val.isNil());
}

test "isProcedure returns true for procedure value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc = Procedure{
        .name = try vm.interner.internStatic(Symbol.init("test-proc")),
        .implementation = .{ .native = .{ .func = struct {
            fn func(_: Procedure.Context) Val {
                return Val.init({});
            }
        }.func } },
    };
    const proc_val = try vm.toVal(proc);

    try testing.expectEqual(true, proc_val.isProcedure());
}

test "isProcedure returns false for non-procedure values" {
    const bool_val = Val.init(true);
    const int_val = Val.init(42);
    const nil_val = Val.init({});

    try testing.expectEqual(false, bool_val.isProcedure());
    try testing.expectEqual(false, int_val.isProcedure());
    try testing.expectEqual(false, nil_val.isProcedure());
}

test "isPair returns true for pair value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const pair_val = try vm.toVal(Pair{
        .car = Val.init(1),
        .cdr = Val.init({}),
    });

    try testing.expectEqual(true, pair_val.isPair());
}

test "isPair returns false for non-pair values" {
    const bool_val = Val.init(true);
    const int_val = Val.init(42);
    const nil_val = Val.init({});

    try testing.expectEqual(false, bool_val.isPair());
    try testing.expectEqual(false, int_val.isPair());
    try testing.expectEqual(false, nil_val.isPair());
}

test "f64 values can be created and are truthy" {
    const val_zero = Val.init(0.0);
    const val_positive = Val.init(3.14);
    const val_negative = Val.init(-2.718);

    try testing.expectEqual(true, val_zero.isTruthy());
    try testing.expectEqual(true, val_positive.isTruthy());
    try testing.expectEqual(true, val_negative.isTruthy());
}

test "isNumber returns true for f64 values" {
    const val_float = Val.init(3.14);
    const val_int = Val.init(42);
    const val_bool = Val.init(true);

    try testing.expectEqual(true, val_float.isNumber());
    try testing.expectEqual(true, val_int.isNumber());
    try testing.expectEqual(false, val_bool.isNumber());
}

test "isNumber returns false for non-numeric values" {
    const bool_val = Val.init(true);
    const nil_val = Val.init({});

    try testing.expectEqual(false, bool_val.isNumber());
    try testing.expectEqual(false, nil_val.isNumber());
}
