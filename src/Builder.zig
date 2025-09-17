//! Builder for converting Zig values to Scheme value representations.
//!
//! This module provides type-safe conversion from compile-time known
//! Zig types to the dynamic value system used by the Scheme interpreter.

const std = @import("std");
const testing = std.testing;

const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const Builder = @This();

vm: *Vm,

/// Converts a Zig value to a Scheme value representation.
/// This function provides type-safe conversion from compile-time known
/// Zig types to the dynamic value system used by the Scheme interpreter.
///
/// Args:
///   self: Pointer to the Builder.
///   v: The value to convert to a Scheme value.
///
/// Returns:
///   A Val representing the converted value, or a compile error for unsupported types.
///
/// Note:
///   Currently only supports void type (converted to end_of_list).
///   Additional type support will be added as needed.
pub fn build(self: Builder, v: anytype) !Val {
    const type_info = @TypeOf(v);
    switch (type_info) {
        Val.Repr => return Val{ .repr = v },
        void => return self.build(Val.Repr{ .end_of_list = {} }),
        Symbol.Interned => return self.build(Val.Repr{ .symbol = v }),
        Symbol => return self.build(try self.vm.interner.intern(v)),
        else => @compileError("type " ++ @typeName(type_info) ++ " not supported for toVal."),
    }
}

test "build with void is end_of_list" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try testing.expectEqual(
        Val{ .repr = Val.Repr{ .end_of_list = {} } },
        try vm.builder().build({}),
    );
}

test "build with Symbol creates symbol val" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "test-symbol" };
    const result = try vm.builder().build(symbol);

    try testing.expect(result.repr == .symbol);
    try testing.expect(
        symbol.eql(try vm.interner.get(result.repr.symbol)),
    );
}

test "build with Symbol.Interned creates symbol val" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "interned-symbol" };
    const interned = try vm.interner.intern(symbol);

    try std.testing.expectEqual(
        interned,
        (try vm.builder().build(symbol)).repr.symbol,
    );
}
