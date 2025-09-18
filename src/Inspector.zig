//! Inspector utilities for examining and formatting Scheme values.
//!
//! This module provides inspection capabilities for Scheme values within the
//! virtual machine environment. It offers convenient methods for creating
//! pretty-printed representations of values for debugging and display purposes.

const std = @import("std");

const Cons = @import("Cons.zig");
const object_pool = @import("object_pool.zig");
const Handle = object_pool.Handle;
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
///   Currently supports void, bool, i64, Symbol.Interned, and Handle(Cons).
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
        .cons => |c| switch (T) {
            Handle(Cons) => return c,
            Cons => return self.vm.cons.get(c) orelse error.ObjectNotFound,
            else => return error.TypeMismatch,
        },
    }
}

test "to converts nil to void" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const val = try vm.builder().build({});
    const result = try vm.inspector().to(void, val);

    try std.testing.expectEqual({}, result);
}

test "to converts i64 to i64" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const val = try vm.builder().build(@as(i64, 42));

    try std.testing.expectEqual(
        @as(i64, 42),
        try vm.inspector().to(i64, val),
    );
}

test "to converts symbol to Symbol.Interned" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "test-symbol" };
    const val = try vm.builder().build(symbol);
    const result = try vm.inspector().to(Symbol.Interned, val);

    try std.testing.expectEqualDeep(symbol, try vm.interner.get(result));
}

test "to converts symbol to Symbol" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "another-symbol" };
    const val = try vm.builder().build(symbol);
    const result = try vm.inspector().to(Symbol, val);

    try std.testing.expect(symbol.eql(result));
}

test "to converts cons to Handle(Cons)" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const val = try vm.builder().build(Cons{
        .car = try vm.builder().build(1),
        .cdr = try vm.builder().build(2),
    });
    const result = try vm.inspector().to(Handle(Cons), val);

    // Verify we can retrieve the original cons through the handle
    const retrieved_cons = vm.cons.get(result) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(try vm.builder().build(1), retrieved_cons.car);
    try std.testing.expectEqual(try vm.builder().build(2), retrieved_cons.cdr);
}

test "to converts cons to Cons" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const val = try vm.builder().build(Cons{
        .car = try vm.builder().build(42),
        .cdr = try vm.builder().build({}),
    });
    const result = try vm.inspector().to(Cons, val);

    try std.testing.expectEqual(
        vm.builder().build(42),
        result.car,
    );
    try std.testing.expectEqual(
        vm.builder().build({}),
        result.cdr,
    );
}

test "to returns TypeMismatch for incompatible types" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const val = try vm.builder().build(42);

    try std.testing.expectError(
        error.TypeMismatch,
        vm.inspector().to(Symbol, val),
    );
}

test "to returns TypeMismatch for nil to non-void" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const val = try vm.builder().build({});

    try std.testing.expectError(
        error.TypeMismatch,
        vm.inspector().to(i64, val),
    );
}

test "to converts boolean true to bool" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const val = try vm.builder().build(true);
    const result = try vm.inspector().to(bool, val);

    try std.testing.expectEqual(true, result);
}

test "to converts boolean false to bool" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const val = try vm.builder().build(false);
    const result = try vm.inspector().to(bool, val);

    try std.testing.expectEqual(false, result);
}

test "to returns TypeMismatch for boolean to non-bool" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const val = try vm.builder().build(true);

    try std.testing.expectError(
        error.TypeMismatch,
        vm.inspector().to(i64, val),
    );
}
