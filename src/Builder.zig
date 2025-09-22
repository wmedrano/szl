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
const String = @import("String.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vector = @import("Vector.zig");
const ByteVector = @import("ByteVector.zig");
const Vm = @import("Vm.zig");

const Builder = @This();

vm: *Vm,

const Error = error{OutOfMemory};

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
///   - Handle(String): Direct string handles
///   - String: Stored in object pool and converted to string values
///   - Handle(Pair): Direct pair handles
///   - Pair: Stored in object pool and converted to pair values
///   - Handle(Procedure): Direct procedure handles
///   - Handle(Vector): Direct vector handles
///   - Vector: Stored in object pool and converted to vector values
///   - Handle(ByteVector): Direct bytevector handles
///   - ByteVector: Stored in object pool and converted to bytevector values
///   - Procedure: Stored in object pool and converted to procedure values
///   - []const Val, []Val: Converted to proper Scheme lists
///
/// Args:
///   self: Pointer to the Builder.
///   v: The value to convert to a Scheme value.
///
/// Returns:
///   A Val representing the converted value, or a compile error for unsupported types.
pub fn build(self: Builder, v: anytype) Error!Val {
    const type_info = @TypeOf(v);
    switch (type_info) {
        Val,
        Val.Repr,
        void,
        bool,
        i64,
        comptime_int,
        Symbol.Interned,
        Handle(String),
        Handle(Pair),
        Handle(Procedure),
        Handle(Vector),
        Handle(ByteVector),
        => return Val.init(v),
        Symbol => return self.internVal(v),
        String => return Val.init(try self.vm.strings.put(self.vm.allocator, v)),
        Pair => return Val.init(try self.vm.pairs.put(self.vm.allocator, v)),
        Procedure => return Val.init(try self.vm.procedures.put(self.vm.allocator, v)),
        Vector => return Val.init(try self.vm.vectors.put(self.vm.allocator, v)),
        ByteVector => return Val.init(try self.vm.bytevectors.put(self.vm.allocator, v)),
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

/// Creates an interned symbol from a Symbol.
///
/// This function takes a Symbol and returns its interned representation,
/// which can be efficiently stored and compared.
///
/// Args:
///   self: The Builder instance.
///   symbol: The Symbol to intern.
///
/// Returns:
///   The interned symbol.
pub fn intern(self: Builder, symbol: Symbol) !Symbol.Interned {
    return self.vm.interner.intern(symbol);
}

/// Creates an interned symbol from a Symbol with static lifetime data.
///
/// This function takes a Symbol with static lifetime data and returns its
/// interned representation without copying the data. This is more efficient
/// than intern() when the symbol data has static lifetime.
///
/// Args:
///   self: The Builder instance.
///   symbol: The Symbol to intern (data will NOT be copied).
///
/// Returns:
///   The interned symbol.
pub fn internStatic(self: Builder, symbol: Symbol) !Symbol.Interned {
    return self.vm.interner.internStatic(symbol);
}

/// Creates an interned symbol from a Symbol and returns it as a Val.
///
/// This function takes a Symbol, interns it, and returns the interned
/// symbol wrapped in a Val for direct use in the Scheme value system.
///
/// Args:
///   self: The Builder instance.
///   symbol: The Symbol to intern.
///
/// Returns:
///   A Val containing the interned symbol.
pub fn internVal(self: Builder, symbol: Symbol) !Val {
    return Val.init(try self.vm.interner.intern(symbol));
}

/// Creates an interned symbol from a Symbol with static lifetime data and returns it as a Val.
///
/// This function takes a Symbol with static lifetime data, interns it without
/// copying the data, and returns the interned symbol wrapped in a Val.
/// This is more efficient than internVal() when the symbol data has static lifetime.
///
/// Args:
///   self: The Builder instance.
///   symbol: The Symbol to intern (data will NOT be copied).
///
/// Returns:
///   A Val containing the interned symbol.
pub fn internStaticVal(self: Builder, symbol: Symbol) !Val {
    return Val.init(try self.vm.interner.internStatic(symbol));
}

/// Creates a symbol table struct with all fields automatically interned.
/// Each field name becomes an interned symbol, useful for creating collections
/// of commonly used symbols for efficient access throughout the program.
///
/// Example:
///   const Operators = struct {
///       add: Symbol.Interned,
///       subtract: Symbol.Interned,
///   };
///   const ops = try builder.symbolTable(Operators);
///   // ops.add contains the interned symbol for "add"
///   // ops.subtract contains the interned symbol for "subtract"
///
/// Args:
///   self: The Builder instance.
///   T: A struct type where all fields are of type Symbol.Interned.
///
/// Returns:
///   An instance of T with all fields populated with interned symbols.
///
/// Errors:
///   Returns an error if symbol interning fails.
pub fn symbolTable(self: Builder, T: anytype) !T {
    var symbol_table: T = undefined;
    const type_info = @typeInfo(T);
    if (type_info != .@"struct") {
        @compileError("symbolTable expects a struct type but found" ++
            @typeName(type_info));
    }

    inline for (type_info.@"struct".fields) |field| {
        if (field.type != Symbol.Interned) {
            @compileError("All fields in symbolTable struct must be Symbol.Interned, found " ++
                @typeName(field.type) ++ " for field " ++ field.name);
        }
        const symbol = Symbol.init(field.name);
        @field(symbol_table, field.name) = try self.internStatic(symbol);
    }

    return symbol_table;
}

/// Defines a global variable by associating a symbol with a value.
///
/// This function interns the given symbol and stores the value in the VM's
/// global value table, making it accessible throughout the program.
///
/// Args:
///   self: The Builder instance.
///   symbol: The `Symbol` or `Symbol.Interned` to use as the variable name.
///   value: The `Val` to associate with the symbol.
///
/// Returns:
///   An error if the symbol cannot be interned or the value cannot be stored.
pub fn define(self: Builder, symbol: anytype, value: Val) !void {
    const T = @TypeOf(symbol);
    const interned_symbol = switch (T) {
        Symbol => try self.vm.interner.intern(symbol),
        Symbol.Interned => symbol,
        else => @compileError(
            "symbol must be Symbol or Symbol.Interned but found type " ++ @typeName(T),
        ),
    };
    try self.vm.global_values.put(self.vm.allocator, interned_symbol, value);
}

test "build with void is end_of_list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try testing.expectEqual(
        Val{ .repr = Val.Repr{ .nil = {} } },
        try vm.builder().build({}),
    );
}

test "build with Symbol creates symbol val" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "test-symbol" };
    const result = try vm.builder().build(symbol);

    try testing.expectEqual(.symbol, std.meta.activeTag(result.repr));
    try testing.expect(@TypeOf(result.repr.symbol) == Symbol.Interned);
    try testing.expect(
        symbol.eql(try vm.interner.get(result.repr.symbol)),
    );
}

test "build with Symbol.Interned creates symbol val" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "interned-symbol" };
    const interned = try vm.interner.intern(symbol);
    const result = try vm.builder().build(interned);

    try testing.expectEqual(.symbol, std.meta.activeTag(result.repr));
    try testing.expect(@TypeOf(result.repr.symbol) == Symbol.Interned);
    try testing.expectEqual(
        interned,
        result.repr.symbol,
    );
}

test "build with i64 creates i64 val" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const value: i64 = 42;
    const result = try vm.builder().build(value);

    try testing.expectEqual(
        Val{ .repr = Val.Repr{ .i64 = 42 } },
        result,
    );
}

test "build with comptime_int creates i64 val" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.builder().build(123);

    try testing.expectEqual(
        Val{ .repr = Val.Repr{ .i64 = 123 } },
        result,
    );
}

test "build with Symbol returns a symbol" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "hello-world" };
    const result = try vm.builder().build(symbol);

    try testing.expectEqual(.symbol, std.meta.activeTag(result.repr));
    try testing.expectFmt(
        "hello-world",
        "{f}",
        .{vm.inspector().pretty(result)},
    );
}

test "build with Symbol.Interned returns a symbol" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol{ .data = "test-symbol" };
    const interned = try vm.interner.intern(symbol);
    const result = try vm.builder().build(interned);

    try testing.expectEqual(.symbol, std.meta.activeTag(result.repr));
    try testing.expectFmt(
        "test-symbol",
        "{f}",
        .{vm.inspector().pretty(result)},
    );
}

test "build with Pair creates cons val" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.builder().build(Pair{
        .car = try vm.builder().build(1),
        .cdr = try vm.builder().build(2),
    });

    try testing.expectEqual(.pair, std.meta.activeTag(result.repr));
    try testing.expectFmt(
        "(1 . 2)",
        "{f}",
        .{vm.inspector().pretty(result)},
    );
}

test "build with bool true creates boolean val" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.builder().build(true);

    try testing.expectEqual(
        Val{ .repr = Val.Repr{ .boolean = true } },
        result,
    );
}

test "build with bool false creates boolean val" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.builder().build(false);

    try testing.expectEqual(
        Val{ .repr = Val.Repr{ .boolean = false } },
        result,
    );
}

test "build with empty []Val creates nil" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const empty_array: []Val = &.{};
    const result = try vm.builder().build(empty_array);

    try testing.expectEqual(
        Val{ .repr = Val.Repr{ .nil = {} } },
        result,
    );
}

test "build with empty []const Val creates nil" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const empty_array: []const Val = &.{};
    const result = try vm.builder().build(empty_array);

    try testing.expectEqual(
        Val{ .repr = Val.Repr{ .nil = {} } },
        result,
    );
}

test "build with single element []Val creates proper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const array: []const Val = &.{try vm.builder().build(42)};
    const result = try vm.builder().build(array);

    try testing.expectEqual(.pair, std.meta.activeTag(result.repr));
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
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const array: []const Val = &.{
        try vm.builder().build(1),
        try vm.builder().build(2),
        try vm.builder().build(3),
    };
    const result = try vm.builder().build(array);

    // Check that result is a proper list: (1 2 3)
    try testing.expectEqual(.pair, std.meta.activeTag(result.repr));

    const first_pair = try vm.inspector().to(Pair, result);
    try testing.expectEqual(
        try vm.builder().build(1),
        first_pair.car,
    );

    try testing.expectEqual(.pair, std.meta.activeTag(first_pair.cdr.repr));
    const second_pair = try vm.inspector().to(Pair, first_pair.cdr);
    try testing.expectEqual(
        try vm.builder().build(2),
        second_pair.car,
    );

    try testing.expectEqual(.pair, std.meta.activeTag(second_pair.cdr.repr));
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
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const test_procedure = Procedure{
        .name = try vm.interner.internStatic(Symbol.init("test-proc")),
        .implementation = .{
            .native = .{
                .func = struct {
                    fn testFunc(_: Procedure.Context) Val {
                        return Val.init(42);
                    }
                }.testFunc,
            },
        },
    };

    const result = try vm.builder().build(test_procedure);

    try testing.expectEqual(.proc, std.meta.activeTag(result.repr));
    try testing.expectFmt(
        "#<procedure:test-proc>",
        "{f}",
        .{vm.inspector().pretty(result)},
    );
}

test "define stores value in global variables" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol.init("test-var");
    const value = Val.init(42);

    try vm.builder().define(symbol, value);

    const retrieved = vm.inspector().get(symbol);
    try testing.expectEqual(Val.init(42), retrieved);
}

test "define can overwrite existing variables" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol.init("overwrite-var");

    // Define initial value
    try vm.builder().define(symbol, Val.init(100));
    try testing.expectEqual(Val.init(100), vm.inspector().get(symbol).?);

    // Overwrite with new value
    try vm.builder().define(symbol, Val.init(200));
    try testing.expectEqual(Val.init(200), vm.inspector().get(symbol).?);
}

test "symbolTable creates struct with interned symbols" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const TestSymbols = struct {
        add: Symbol.Interned,
        subtract: Symbol.Interned,
        multiply: Symbol.Interned,
    };

    const symbols = try vm.builder().symbolTable(TestSymbols);

    // Verify each field is properly interned
    const add_symbol = try vm.interner.get(symbols.add);
    const subtract_symbol = try vm.interner.get(symbols.subtract);
    const multiply_symbol = try vm.interner.get(symbols.multiply);

    try testing.expectEqualStrings("add", add_symbol.data);
    try testing.expectEqualStrings("subtract", subtract_symbol.data);
    try testing.expectEqualStrings("multiply", multiply_symbol.data);
}

test "internVal creates symbol val from Symbol" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol.init("test-symbol");
    const result = try vm.builder().internVal(symbol);

    try testing.expectEqual(.symbol, std.meta.activeTag(result.repr));
    try testing.expectFmt(
        "test-symbol",
        "{f}",
        .{vm.inspector().pretty(result)},
    );
}

test "internStaticVal creates symbol val from Symbol" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = Symbol.init("static-symbol");
    const result = try vm.builder().internStaticVal(symbol);

    try testing.expectEqual(.symbol, std.meta.activeTag(result.repr));
    try testing.expectFmt(
        "static-symbol",
        "{f}",
        .{vm.inspector().pretty(result)},
    );
}

test "build with String creates string val" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const test_string = try String.initFromSlice(testing.allocator, "hello world");
    const result = try vm.builder().build(test_string);

    try testing.expectEqual(.string, std.meta.activeTag(result.repr));
    try testing.expectFmt(
        "\"hello world\"",
        "{f}",
        .{vm.inspector().pretty(result)},
    );
}

test "build with Handle(String) creates string val" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const test_string = try String.initFromSlice(testing.allocator, "test string");
    const string_handle = try vm.strings.put(testing.allocator, test_string);
    const result = try vm.builder().build(string_handle);

    try testing.expectEqual(.string, std.meta.activeTag(result.repr));
    try testing.expectEqual(string_handle, result.repr.string);
}

test "build with Vector creates vector val" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const values = [_]Val{ Val.init(1), Val.init(2), Val.init(3) };
    const test_vector = try Vector.initFromSlice(testing.allocator, &values);
    const result = try vm.builder().build(test_vector);

    try testing.expectEqual(.vector, std.meta.activeTag(result.repr));
    try testing.expectFmt(
        "#(1 2 3)",
        "{f}",
        .{vm.inspector().pretty(result)},
    );
}

test "build with Handle(Vector) creates vector val" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const values = [_]Val{ Val.init(42), Val.init(true) };
    const test_vector = try Vector.initFromSlice(testing.allocator, &values);
    const vector_handle = try vm.vectors.put(testing.allocator, test_vector);
    const result = try vm.builder().build(vector_handle);

    try testing.expectEqual(.vector, std.meta.activeTag(result.repr));
    try testing.expectEqual(vector_handle, result.repr.vector);
}

test "build with ByteVector creates bytevector val" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const bytes = [_]u8{ 0x01, 0x02, 0x03 };
    const test_bytevector = try ByteVector.initFromSlice(testing.allocator, &bytes);
    const result = try vm.builder().build(test_bytevector);

    try testing.expectEqual(.bytevector, std.meta.activeTag(result.repr));
    try testing.expectFmt(
        "#u8(1 2 3)",
        "{f}",
        .{vm.inspector().pretty(result)},
    );
}

test "build with Handle(ByteVector) creates bytevector val" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const bytes = [_]u8{ 0x42, 0xFF };
    const test_bytevector = try ByteVector.initFromSlice(testing.allocator, &bytes);
    const bytevector_handle = try vm.bytevectors.put(testing.allocator, test_bytevector);
    const result = try vm.builder().build(bytevector_handle);

    try testing.expectEqual(.bytevector, std.meta.activeTag(result.repr));
    try testing.expectEqual(bytevector_handle, result.repr.bytevector);
}
