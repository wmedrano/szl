//! Builder for converting Zig values to Scheme value representations.
//!
//! This module provides type-safe conversion from compile-time known
//! Zig types to the dynamic value system used by the Scheme interpreter.

const std = @import("std");

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
///   Currently supports void, bool, i64, Symbol, and Cons types.
pub fn build(self: Builder, v: anytype) !Val {
    const type_info = @TypeOf(v);
    switch (type_info) {
        Val.Repr => return Val{ .repr = v },
        void => return self.build(Val.Repr{ .nil = {} }),
        bool => return self.build(Val.Repr{ .boolean = v }),
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
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();
    try std.testing.expectEqual(
        Val{ .repr = Val.Repr{ .nil = {} } },
        try vm.builder().build({}),
    );
}

test "build with Symbol creates symbol val" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "test-symbol" };
    const result = try vm.builder().build(symbol);

    try std.testing.expect(result.repr == .symbol);
    try std.testing.expect(@TypeOf(result.repr.symbol) == Symbol.Interned);
    try std.testing.expect(
        symbol.eql(try vm.interner.get(result.repr.symbol)),
    );
}

test "build with Symbol.Interned creates symbol val" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "interned-symbol" };
    const interned = try vm.interner.intern(symbol);
    const result = try vm.builder().build(interned);

    try std.testing.expect(result.repr == .symbol);
    try std.testing.expect(@TypeOf(result.repr.symbol) == Symbol.Interned);
    try std.testing.expectEqual(
        interned,
        result.repr.symbol,
    );
}

test "build with i64 creates i64 val" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const value: i64 = 42;
    const result = try vm.builder().build(value);

    try std.testing.expectEqual(
        Val{ .repr = Val.Repr{ .i64 = 42 } },
        result,
    );
}

test "build with comptime_int creates i64 val" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const result = try vm.builder().build(123);

    try std.testing.expectEqual(
        Val{ .repr = Val.Repr{ .i64 = 123 } },
        result,
    );
}

test "build with Symbol returns a symbol" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "hello-world" };
    const result = try vm.builder().build(symbol);

    try std.testing.expect(result.repr == .symbol);
    try std.testing.expectFmt("hello-world", "{any}", .{vm.inspector().pretty(result)});
}

test "build with Symbol.Interned returns a symbol" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "test-symbol" };
    const interned = try vm.interner.intern(symbol);
    const result = try vm.builder().build(interned);

    try std.testing.expect(result.repr == .symbol);
    try std.testing.expectFmt("test-symbol", "{any}", .{vm.inspector().pretty(result)});
}

test "build with Cons creates cons val" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const result = try vm.builder().build(Cons{
        .car = try vm.builder().build(1),
        .cdr = try vm.builder().build(2),
    });

    try std.testing.expect(result.repr == .cons);
    try std.testing.expectFmt("(1 . 2)", "{any}", .{vm.inspector().pretty(result)});
}

test "build with bool true creates boolean val" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const result = try vm.builder().build(true);

    try std.testing.expectEqual(
        Val{ .repr = Val.Repr{ .boolean = true } },
        result,
    );
}

test "build with bool false creates boolean val" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const result = try vm.builder().build(false);

    try std.testing.expectEqual(
        Val{ .repr = Val.Repr{ .boolean = false } },
        result,
    );
}
