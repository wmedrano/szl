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
const Procedure = @import("Procedure.zig");
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

/// Creates a PrettyPrinter.Slice for formatting a slice of Scheme values.
/// The PrettyPrinter.Slice can be used with Zig's standard formatting functions
/// to display a slice of Scheme values as a parenthesized list.
///
/// Args:
///   self: Pointer to the VM that owns the values' data.
///   vals: The slice of Scheme values to format.
///
/// Returns:
///   A PrettyPrinter.Slice instance ready for formatting.
pub fn prettySlice(self: Inspector, vals: []const Val) PrettyPrinter.Slice {
    return PrettyPrinter.Slice{ .vm = self.vm, .vals = vals };
}

/// Gets a global value by symbol name.
///
/// This function looks up a global variable by its symbol name and returns
/// the associated value if it exists.
///
/// Args:
///   self: The Inspector instance.
///   symbol: The Symbol or Symbol.Interned to look up in global values.
///
/// Returns:
///   The value associated with the symbol, or null if not found.
pub fn get(self: Inspector, symbol: anytype) ?Val {
    const T = @TypeOf(symbol);
    switch (T) {
        Symbol => {
            const interned = self.vm.interner.lookup(symbol) orelse return null;
            return self.vm.global_values.get(interned);
        },
        Symbol.Interned => {
            return self.vm.global_values.get(symbol);
        },
        else => @compileError("get() only accepts Symbol or Symbol.Interned, got " ++ @typeName(T)),
    }
}

/// Iterator for traversing Scheme lists.
///
/// Provides safe iteration over linked list structures built from Pair objects.
/// The iterator automatically handles list termination and validates list structure.
pub const ListIterator = struct {
    vm: *const Vm,
    current: Val,

    /// Returns the next value in the list and advances the iterator.
    ///
    /// Args:
    ///   self: Pointer to the iterator instance.
    ///
    /// Returns:
    ///   The next value in the list, or null if iteration is complete.
    ///
    /// Errors:
    ///   - TypeMismatch if the list structure is improper (cdr is not nil or pair).
    pub fn next(self: *ListIterator) !?Val {
        switch (self.current.repr) {
            .nil => {
                return null;
            },
            .pair => |handle| {
                const pair = try self.vm.inspector().resolve(Pair, handle);
                self.current = pair.cdr;
                return pair.car;
            },
            else => return error.TypeMismatch,
        }
    }
};

/// Creates an iterator for traversing a Scheme list.
///
/// Lists in Scheme are represented as linked lists using Pair objects,
/// terminated by nil. This function validates that the input is a valid
/// list structure and returns an iterator for traversing its elements.
///
/// Args:
///   self: The Inspector instance.
///   val: The Scheme value to iterate over (must be nil or start with a pair).
///
/// Returns:
///   A ListIterator positioned at the beginning of the list.
///
/// Errors:
///   - TypeMismatch if the value is not nil or a pair.
pub fn iterList(self: Inspector, val: Val) !ListIterator {
    switch (val.repr) {
        .nil, .pair => return ListIterator{
            .vm = self.vm,
            .current = val,
        },
        else => return error.TypeMismatch,
    }
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
/// Supported conversions:
///   - nil → void
///   - boolean → bool
///   - i64 → i64
///   - symbol → Symbol.Interned, Symbol
///   - pair → Handle(Pair), Pair
///   - procedure → Handle(Procedure), Procedure
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
            Pair => return try self.resolve(Pair, c),
            else => return error.TypeMismatch,
        },
        .procedure => |p| switch (T) {
            Handle(Procedure) => return p,
            Procedure => return try self.resolve(Procedure, p),
            else => return error.TypeMismatch,
        },
    }
}

/// Gets the value from a Handle(T).
///
/// Args:
///   self: The Inspector instance.
///   T: The type stored in the handle.
///   handle: The handle to dereference.
///
/// Returns:
///   The value of type T, or an error if the handle is invalid.
pub fn resolve(self: Inspector, T: type, handle: Handle(T)) !T {
    return switch (T) {
        Pair => self.vm.pairs.get(handle) orelse error.ObjectNotFound,
        Procedure => self.vm.procedures.get(handle) orelse error.ObjectNotFound,
        else => @compileError("resolve() only supports Pair and Procedure, got " ++ @typeName(T)),
    };
}

test "to converts nil to void" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal({});
    const result = try vm.inspector().to(void, val);

    try testing.expectEqual({}, result);
}

test "to converts i64 to i64" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal(42);

    try testing.expectEqual(
        42,
        try vm.inspector().to(i64, val),
    );
}

test "to converts symbol to Symbol.Interned" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "test-symbol" };
    const val = try vm.toVal(symbol);
    const result = try vm.inspector().to(Symbol.Interned, val);

    try testing.expectEqualDeep(symbol, try vm.interner.get(result));
}

test "to converts symbol to Symbol" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "another-symbol" };
    const val = try vm.toVal(symbol);
    const result = try vm.inspector().to(Symbol, val);

    try testing.expect(symbol.eql(result));
}

test "to converts cons to Handle(Pair)" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal(Pair{
        .car = try vm.toVal(1),
        .cdr = try vm.toVal(2),
    });
    const result = try vm.inspector().to(Handle(Pair), val);

    // Verify we can retrieve the original cons through the handle
    const retrieved_cons = try vm.inspector().resolve(Pair, result);
    try testing.expectEqual(try vm.toVal(1), retrieved_cons.car);
    try testing.expectEqual(try vm.toVal(2), retrieved_cons.cdr);
}

test "to converts cons to Pair" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
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
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal(42);

    try testing.expectError(
        error.TypeMismatch,
        vm.inspector().to(Symbol, val),
    );
}

test "to returns TypeMismatch for nil to non-void" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal({});

    try testing.expectError(
        error.TypeMismatch,
        vm.inspector().to(i64, val),
    );
}

test "to converts boolean true to bool" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal(true);
    const result = try vm.inspector().to(bool, val);

    try testing.expectEqual(true, result);
}

test "to converts boolean false to bool" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal(false);
    const result = try vm.inspector().to(bool, val);

    try testing.expectEqual(false, result);
}

test "to returns TypeMismatch for boolean to non-bool" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.toVal(true);

    try testing.expectError(
        error.TypeMismatch,
        vm.inspector().to(i64, val),
    );
}

test "get returns null for non-existent symbol" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        null,
        vm.inspector().get(Symbol.init("non-existent")),
    );
}

test "get returns value for existing global variable" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.builder().define(Symbol.init("test-var"), Val.init(42));

    try testing.expectEqual(
        Val.init(42),
        vm.inspector().get(Symbol.init("test-var")),
    );
}

test "get returns updated value after redefinition" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Define initial value
    try vm.builder().define(Symbol.init("update-var"), Val.init(100));
    try testing.expectEqual(
        Val.init(100),
        vm.inspector().get(Symbol.init("update-var")),
    );

    // Redefine with new value
    try vm.builder().define(Symbol.init("update-var"), Val.init(200));
    try testing.expectEqual(
        Val.init(200),
        vm.inspector().get(Symbol.init("update-var")),
    );
}

test "iterList creates iterator for empty list (nil)" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var iter = try vm.inspector().iterList(Val.init({}));

    try testing.expectEqual(null, try iter.next());
}

test "iterList creates iterator for single-element list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Create (42)
    const list = try vm.toVal(Pair{
        .car = try vm.toVal(42),
        .cdr = try vm.toVal({}),
    });

    var iter = try vm.inspector().iterList(list);

    // Should get 42, then null
    try testing.expectEqual(
        try vm.toVal(42),
        (try iter.next()).?,
    );
    try testing.expectEqual(null, try iter.next());
}

test "iterList creates iterator for multi-element list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Create (1 2 3)
    const list = try vm.toVal(Pair{
        .car = try vm.toVal(1),
        .cdr = try vm.toVal(Pair{
            .car = try vm.toVal(2),
            .cdr = try vm.toVal(Pair{
                .car = try vm.toVal(3),
                .cdr = try vm.toVal({}),
            }),
        }),
    });

    var iter = try vm.inspector().iterList(list);

    // Should get 1, 2, 3, then null
    try testing.expectEqual(
        try vm.toVal(1),
        try iter.next(),
    );
    try testing.expectEqual(
        try vm.toVal(2),
        try iter.next(),
    );
    try testing.expectEqual(
        try vm.toVal(3),
        try iter.next(),
    );
    try testing.expectEqual(null, try iter.next());
}

test "iterList returns TypeMismatch for non-list values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test with integer
    try testing.expectError(
        error.TypeMismatch,
        vm.inspector().iterList(Val.init(42)),
    );

    // Test with boolean
    try testing.expectError(
        error.TypeMismatch,
        vm.inspector().iterList(Val.init(true)),
    );

    // Test with symbol
    try testing.expectError(
        error.TypeMismatch,
        vm.inspector().iterList(try vm.toVal(Symbol.init("test"))),
    );
}

test "iterList handles improper lists" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Create (1 . 2) - improper list where cdr is not nil or pair
    const improper_list = try vm.toVal(Pair{
        .car = try vm.toVal(1),
        .cdr = try vm.toVal(2),
    });

    var iter = try vm.inspector().iterList(improper_list);
    try testing.expectEqual(try vm.toVal(1), try iter.next());

    // Next call should return TypeMismatch error because cdr (2) is not nil or pair
    try testing.expectError(
        error.TypeMismatch,
        iter.next(),
    );
}

test "iterList iterator always returns null after completion" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const list = try vm.toVal(Pair{
        .car = try vm.toVal(42),
        .cdr = try vm.toVal({}),
    });

    var iter = try vm.inspector().iterList(list);
    try testing.expectEqual(Val.init(42), try iter.next());
    try testing.expectEqual(null, try iter.next()); // End of list

    // Further calls should continue to return null
    try testing.expectEqual(null, try iter.next());
    try testing.expectEqual(null, try iter.next());
}
