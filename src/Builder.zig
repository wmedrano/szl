//! Builder for converting Zig values to Scheme value representations.
//!
//! This module provides type-safe conversion from compile-time known
//! Zig types to the dynamic value system used by the Scheme interpreter.

const std = @import("std");
const testing = std.testing;

const Cons = @import("Cons.zig");
const object_pool = @import("object_pool.zig");
const Handle = object_pool.Handle;
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
        void => return self.build(Val.Repr{ .nil = {} }),
        i64, comptime_int => return self.build(Val.Repr{ .i64 = v }),
        Symbol.Interned => return self.build(Val.Repr{ .symbol = v }),
        Symbol => return self.build(try self.vm.interner.intern(v)),
        Handle(Cons) => return self.build(Val.Repr{ .cons = v }),
        Cons => {
            const handle = try self.vm.cons.put(self.vm.allocator, v);
            return self.build(handle);
        },
        else => @compileError("type " ++ @typeName(type_info) ++ " not supported for toVal."),
    }
}

test "build with void is end_of_list" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try testing.expectEqual(
        Val{ .repr = Val.Repr{ .nil = {} } },
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

test "build with i64 creates i64 val" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const value: i64 = 42;
    const result = try vm.builder().build(value);

    try testing.expectEqual(
        Val{ .repr = Val.Repr{ .i64 = 42 } },
        result,
    );
}

test "build with comptime_int creates i64 val" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.builder().build(123);

    try testing.expectEqual(
        Val{ .repr = Val.Repr{ .i64 = 123 } },
        result,
    );
}
