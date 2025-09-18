//! Inspector utilities for examining and formatting Scheme values.
//!
//! This module provides inspection capabilities for Scheme values within the
//! virtual machine environment. It offers convenient methods for creating
//! pretty-printed representations of values for debugging and display purposes.

const std = @import("std");
const testing = std.testing;

const object_pool = @import("object_pool.zig");
const Handle = object_pool.Handle;
const Pair = @import("Pair.zig");
const PrettyPrinter = @import("PrettyPrinter.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const Inspector = @This();

vm: *const Vm,

/// Creates a PrettyPrinter for formatting a Scheme value.
/// The PrettyPrinter can be used with Zig's standard formatting functions
/// to display Scheme values in a human-readable format.
///
/// Args:
///   self: Pointer to the VM that owns the value's data.
///   val: The Scheme value to format.
///
/// Returns:
///   A PrettyPrinter instance ready for formatting.
pub fn pretty(self: Inspector, val: Val) PrettyPrinter {
    return PrettyPrinter{ .vm = self.vm, .val = val };
}

/// Converts a Scheme value to a Zig type.
/// This function provides type-safe conversion from the dynamic value system
/// back to compile-time known Zig types.
///
/// Args:
///   self: Pointer to the Inspector.
///   T: The target Zig type to convert to.
///   val: The Scheme value to convert.
///
/// Returns:
///   The converted value of type T, or an error if the conversion is not possible.
///
/// Note:
///   Currently supports void, bool, i64, Symbol.Interned, and Handle(Pair).
pub fn to(self: Inspector, T: type, val: Val) !T {
    switch (val.repr) {
        .nil => switch (T) {
            void => return {},
            else => return error.TypeMismatch,
        },
        .boolean => |b| switch (T) {
            bool => return b,
            else => return error.TypeMismatch,
        },
        .i64 => |i| switch (T) {
            i64 => return i,
            else => return error.TypeMismatch,
        },
        .symbol => |s| switch (T) {
            Symbol.Interned => return s,
            Symbol => return self.vm.interner.get(s),
            else => return error.TypeMismatch,
        },
        .pair => |c| switch (T) {
            Handle(Pair) => return c,
            Pair => return self.vm.pairs.get(c) orelse error.ObjectNotFound,
            else => return error.TypeMismatch,
        },
    }
}

test "to converts nil to void" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal({});
    const result = try vm.inspector().to(void, val);

    try testing.expectEqual({}, result);
}

test "to converts i64 to i64" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal(@as(i64, 42));

    try testing.expectEqual(
        42,
        try vm.inspector().to(i64, val),
    );
}

test "to converts symbol to Symbol.Interned" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "test-symbol" };
    const val = try vm.toVal(symbol);
    const result = try vm.inspector().to(Symbol.Interned, val);

    try testing.expectEqualDeep(symbol, try vm.interner.get(result));
}

test "to converts symbol to Symbol" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "another-symbol" };
    const val = try vm.toVal(symbol);
    const result = try vm.inspector().to(Symbol, val);

    try testing.expect(symbol.eql(result));
}

test "to converts cons to Handle(Pair)" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal(Pair{
        .car = try vm.toVal(1),
        .cdr = try vm.toVal(2),
    });
    const result = try vm.inspector().to(Handle(Pair), val);

    // Verify we can retrieve the original cons through the handle
    const retrieved_cons = vm.pairs.get(result) orelse return error.TestUnexpectedResult;
    try testing.expectEqual(try vm.toVal(1), retrieved_cons.car);
    try testing.expectEqual(try vm.toVal(2), retrieved_cons.cdr);
}

test "to converts cons to Pair" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal(Pair{
        .car = try vm.toVal(42),
        .cdr = try vm.toVal({}),
    });
    const result = try vm.inspector().to(Pair, val);

    try testing.expectEqual(
        vm.toVal(42),
        result.car,
    );
    try testing.expectEqual(
        vm.toVal({}),
        result.cdr,
    );
}

test "to returns TypeMismatch for incompatible types" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal(42);

    try testing.expectError(
        error.TypeMismatch,
        vm.inspector().to(Symbol, val),
    );
}

test "to returns TypeMismatch for nil to non-void" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal({});

    try testing.expectError(
        error.TypeMismatch,
        vm.inspector().to(i64, val),
    );
}

test "to converts boolean true to bool" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal(true);
    const result = try vm.inspector().to(bool, val);

    try testing.expectEqual(true, result);
}

test "to converts boolean false to bool" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal(false);
    const result = try vm.inspector().to(bool, val);

    try testing.expectEqual(false, result);
}

test "to returns TypeMismatch for boolean to non-bool" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal(true);

    try testing.expectError(
        error.TypeMismatch,
        vm.inspector().to(i64, val),
    );
}
