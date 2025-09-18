//! Builder for converting Zig values to Scheme value representations.
//!
//! This module provides type-safe conversion from compile-time known
//! Zig types to the dynamic value system used by the Scheme interpreter.

const std = @import("std");
const testing = std.testing;

const object_pool = @import("object_pool.zig");
const Handle = object_pool.Handle;
const Pair = @import("Pair.zig");
const Procedure = @import("Procedure.zig");
const Reader = @import("Reader.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const Builder = @This();

vm: *Vm,

/// Converts a Zig value to a Scheme value representation.
/// This function provides type-safe conversion from compile-time known
/// Zig types to the dynamic value system used by the Scheme interpreter.
///
/// Supported types:
///   - Val, Val.Repr: Pass-through values
///   - void: Converted to nil (empty list)
///   - bool: Converted to boolean values
///   - i64, comptime_int: Converted to integer values
///   - Symbol, Symbol.Interned: Converted to interned symbols
///   - Handle(Pair): Direct pair handles
///   - Pair: Stored in object pool and converted to pair values
///   - Handle(Procedure): Direct procedure handles
///   - Procedure: Stored in object pool and converted to procedure values
///   - []const Val, []Val: Converted to proper Scheme lists
///
/// Args:
///   self: Pointer to the Builder.
///   v: The value to convert to a Scheme value.
///
/// Returns:
///   A Val representing the converted value, or a compile error for unsupported types.
pub fn build(self: Builder, v: anytype) !Val {
    const type_info = @TypeOf(v);
    switch (type_info) {
        Val,
        Val.Repr,
        void,
        bool,
        i64,
        comptime_int,
        Symbol.Interned,
        Handle(Pair),
        Handle(Procedure),
        => return Val.init(v),
        Symbol => return Val.init(try self.vm.interner.intern(v)),
        Pair => return Val.init(try self.vm.pairs.put(self.vm.allocator, v)),
        Procedure => return Val.init(try self.vm.procedures.put(self.vm.allocator, v)),
        []const Val, []Val => {
            if (v.len == 0) return self.build({});
            var val = try self.build({});
            for (0..v.len) |n| {
                const idx = v.len - n - 1;
                val = try self.build(Pair.init(v[idx], val));
            }
            return val;
        },
        else => @compileError("type " ++ @typeName(type_info) ++ " not supported for toVal."),
    }
}

/// Creates a new Reader for parsing Scheme source code.
///
/// This function initializes a Reader that can parse multiple values from
/// the provided source text. The Reader uses this Builder's VM instance
/// for value creation and symbol interning during parsing.
///
/// Args:
///   self: The Builder instance.
///   source: The Scheme source code to parse.
///
/// Returns:
///   A Reader instance ready to parse the source text.
pub fn read(self: Builder, source: []const u8) Reader {
    return Reader.init(self.vm, source);
}

/// Parses exactly one value from Scheme source code.
///
/// This is a convenience function that creates a Reader and parses exactly
/// one value from the source text. It validates that the source contains
/// exactly one parseable value - no more, no less.
///
/// Args:
///   self: The Builder instance.
///   source: The Scheme source code to parse.
///
/// Returns:
///   The parsed value if successful.
///
/// Errors:
///   - NoValue: If the source contains no parseable values.
///   - TooManyValues: If the source contains more than one value.
///   - BadExpression: If the source contains malformed expressions.
pub fn readOne(self: Builder, source: []const u8) !Val {
    return Reader.readOne(self.vm, source);
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
    try testing.expect(@TypeOf(result.repr.symbol) == Symbol.Interned);
    try testing.expect(
        symbol.eql(try vm.interner.get(result.repr.symbol)),
    );
}

test "build with Symbol.Interned creates symbol val" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "interned-symbol" };
    const interned = try vm.interner.intern(symbol);
    const result = try vm.builder().build(interned);

    try testing.expect(result.repr == .symbol);
    try testing.expect(@TypeOf(result.repr.symbol) == Symbol.Interned);
    try testing.expectEqual(
        interned,
        result.repr.symbol,
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

test "build with Symbol returns a symbol" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "hello-world" };
    const result = try vm.builder().build(symbol);

    try testing.expect(result.repr == .symbol);
    try testing.expectFmt(
        "hello-world",
        "{f}",
        .{vm.inspector().pretty(result)},
    );
}

test "build with Symbol.Interned returns a symbol" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "test-symbol" };
    const interned = try vm.interner.intern(symbol);
    const result = try vm.builder().build(interned);

    try testing.expect(result.repr == .symbol);
    try testing.expectFmt(
        "test-symbol",
        "{f}",
        .{vm.inspector().pretty(result)},
    );
}

test "build with Pair creates cons val" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.builder().build(Pair{
        .car = try vm.builder().build(1),
        .cdr = try vm.builder().build(2),
    });

    try testing.expect(result.repr == .pair);
    try testing.expectFmt(
        "(1 . 2)",
        "{f}",
        .{vm.inspector().pretty(result)},
    );
}

test "build with bool true creates boolean val" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.builder().build(true);

    try testing.expectEqual(
        Val{ .repr = Val.Repr{ .boolean = true } },
        result,
    );
}

test "build with bool false creates boolean val" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.builder().build(false);

    try testing.expectEqual(
        Val{ .repr = Val.Repr{ .boolean = false } },
        result,
    );
}

test "build with empty []Val creates nil" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const empty_array: []Val = &.{};
    const result = try vm.builder().build(empty_array);

    try testing.expectEqual(
        Val{ .repr = Val.Repr{ .nil = {} } },
        result,
    );
}

test "build with empty []const Val creates nil" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const empty_array: []const Val = &.{};
    const result = try vm.builder().build(empty_array);

    try testing.expectEqual(
        Val{ .repr = Val.Repr{ .nil = {} } },
        result,
    );
}

test "build with single element []Val creates proper list" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const array: []const Val = &.{try vm.builder().build(42)};
    const result = try vm.builder().build(array);

    try testing.expect(result.repr == .pair);
    const pair = try vm.inspector().to(Pair, result);
    try testing.expectEqual(
        try vm.builder().build(42),
        pair.car,
    );
    try testing.expectEqual(
        try vm.builder().build({}),
        pair.cdr,
    );
}

test "build with multiple element []Val creates proper list" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const array: []const Val = &.{
        try vm.builder().build(1),
        try vm.builder().build(2),
        try vm.builder().build(3),
    };
    const result = try vm.builder().build(array);

    // Check that result is a proper list: (1 2 3)
    try testing.expect(result.repr == .pair);

    const first_pair = try vm.inspector().to(Pair, result);
    try testing.expectEqual(
        try vm.builder().build(1),
        first_pair.car,
    );

    try testing.expect(first_pair.cdr.repr == .pair);
    const second_pair = try vm.inspector().to(Pair, first_pair.cdr);
    try testing.expectEqual(
        try vm.builder().build(2),
        second_pair.car,
    );

    try testing.expect(second_pair.cdr.repr == .pair);
    const third_pair = try vm.inspector().to(Pair, second_pair.cdr);
    try testing.expectEqual(
        try vm.builder().build(3),
        third_pair.car,
    );

    try testing.expectEqual(
        try vm.builder().build({}),
        third_pair.cdr,
    );
}

test "build with Procedure creates procedure val" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const test_procedure = Procedure{
        .name = try vm.interner.internStatic(Symbol.init("test-proc")),
        .implementation = .{ .native = .{ .func = struct {
            fn testFunc(vm_ptr: *Vm) Val {
                _ = vm_ptr;
                return Val.init(42);
            }
        }.testFunc } },
    };

    const result = try vm.builder().build(test_procedure);

    try testing.expect(result.repr == .procedure);
    try testing.expectFmt(
        "#<procedure:test-proc>",
        "{f}",
        .{vm.inspector().pretty(result)},
    );
}
