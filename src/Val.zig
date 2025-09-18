//! Value representation system for the Scheme interpreter.
//!
//! This module defines the core value types that can be stored and manipulated
//! in the Scheme interpreter. Values are represented using a tagged union to
//! efficiently handle the dynamic typing inherent in Scheme.

const std = @import("std");

const Cons = @import("Cons.zig");
const object_pool = @import("object_pool.zig");
const Handle = object_pool.Handle;
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

    /// Represents a cons cell (pair) using a handle to an object pool.
    /// Cons cells are the fundamental building blocks for lists and pairs in Scheme.
    cons: Handle(Cons),
};

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
    try std.testing.expectEqual(false, val.isTruthy());
}

test "isTruthy returns true for boolean true" {
    const val = Val{ .repr = .{ .boolean = true } };
    try std.testing.expectEqual(true, val.isTruthy());
}

test "isTruthy returns true for nil" {
    const val = Val{ .repr = .{ .nil = {} } };
    try std.testing.expectEqual(true, val.isTruthy());
}

test "isTruthy returns true for numbers" {
    const val_zero = Val{ .repr = .{ .i64 = 0 } };
    const val_positive = Val{ .repr = .{ .i64 = 42 } };
    const val_negative = Val{ .repr = .{ .i64 = -5 } };

    try std.testing.expectEqual(true, val_zero.isTruthy());
    try std.testing.expectEqual(true, val_positive.isTruthy());
    try std.testing.expectEqual(true, val_negative.isTruthy());
}

test "isTruthy returns true for symbols" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const symbol = try vm.interner.internStatic(@import("Symbol.zig").init("test"));
    const val = Val{ .repr = .{ .symbol = symbol } };

    try std.testing.expectEqual(true, val.isTruthy());
}

test "isTruthy returns true for cons cells" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const cons_val = try vm.builder().build(@import("Cons.zig"){
        .car = Val{ .repr = .{ .i64 = 1 } },
        .cdr = Val{ .repr = .{ .nil = {} } },
    });

    try std.testing.expectEqual(true, cons_val.isTruthy());
}
