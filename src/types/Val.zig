//! Value representation system for the Scheme interpreter.
//!
//! This module defines the core value types that can be stored and manipulated
//! in the Scheme interpreter. Values are represented using a tagged union to
//! efficiently handle the dynamic typing inherent in Scheme. Supports nil,
//! booleans, integers, symbols, pairs, and procedures.

const std = @import("std");
const testing = std.testing;

const object_pool = @import("../object_pool.zig");
const Handle = object_pool.Handle;
const Procedure = @import("../Procedure.zig");
const Vm = @import("../Vm.zig");
const ByteVector = @import("ByteVector.zig");
const Char = @import("Char.zig");
const Pair = @import("Pair.zig");
const Record = @import("Record.zig");
const String = @import("String.zig");
const Symbol = @import("Symbol.zig");
const Vector = @import("Vector.zig");

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

    /// Represents a character value.
    /// Characters in Scheme are single byte values represented with #\ notation.
    char: Char,

    /// Represents a string value using a handle to an object pool.
    /// Strings in Scheme are sequences of characters enclosed in double quotes.
    string: Handle(String),

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
    proc: Handle(Procedure),

    /// Similar to `proc`, but implemented in natively in Zig.
    native_proc: *const Procedure.Native,

    /// Represents a vector using a handle to an object pool.
    /// Vectors in Scheme are sequences of values that can be accessed by index.
    vector: Handle(Vector),

    /// Represents a bytevector using a handle to an object pool.
    /// Bytevectors in Scheme are sequences of bytes that can be accessed by index.
    bytevector: Handle(ByteVector),

    /// Represents a record using a handle to an object pool.
    /// Records in Scheme are user-defined data types with named fields.
    record: Handle(Record),

    /// Represents a record type descriptor using a handle to an object pool.
    /// Record type descriptors define the structure and behavior of record types.
    record_type_descriptor: Handle(Record.RecordTypeDescriptor),
};

/// Create a new `Val` for a supported type.
///
/// Supported primitive types:
///   - Val, Val.Repr: Pass-through values
///   - void: Converted to nil
///   - bool: Converted to boolean values
///   - i64, comptime_int: Converted to integer values
///   - f64, comptime_float: Converted to floating point values
///   - Char: Converted to character values
///   - Symbol.Interned: Converted to symbol values
///   - Handle(String): Converted to string values
///   - Handle(Pair): Converted to pair values
///   - Handle(Procedure): Converted to procedure values
///   - Handle(Vector): Converted to vector values
///   - Handle(ByteVector): Converted to bytevector values
///   - Handle(Record): Converted to record values
///   - Handle(Record.RecordTypeDescriptor): Converted to record type descriptor values
///
/// For more complex types like Symbol, Pair, Procedure, Vector, ByteVector, or Record structs, use `Vm.builder()`.
pub fn init(v: anytype) Val {
    const type_info = @TypeOf(v);
    switch (type_info) {
        Val => return v,
        Val.Repr => return Val{ .repr = v },
        void => return init(Val.Repr{ .nil = {} }),
        bool => return init(Val.Repr{ .boolean = v }),
        i64, comptime_int => return init(Val.Repr{ .i64 = v }),
        f64, comptime_float => return init(Val.Repr{ .f64 = v }),
        Char => return init(Val.Repr{ .char = v }),
        Symbol.Interned => return init(Val.Repr{ .symbol = v }),
        Handle(String) => return init(Val.Repr{ .string = v }),
        Handle(Pair) => return init(Val.Repr{ .pair = v }),
        Handle(Procedure) => return init(Val.Repr{ .proc = v }),
        *const Procedure.Native => return init(Val.Repr{ .native_proc = v }),
        Handle(Vector) => return init(Val.Repr{ .vector = v }),
        Handle(ByteVector) => return init(Val.Repr{ .bytevector = v }),
        Handle(Record) => return init(Val.Repr{ .record = v }),
        Handle(Record.RecordTypeDescriptor) => return init(Val.Repr{ .record_type_descriptor = v }),
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
        .proc, .native_proc => true,
        else => false,
    };
}

/// Determines if a value is a character.
///
/// Args:
///   self: The value to test for character type.
///
/// Returns:
///   true if the value is a character, false otherwise.
pub fn isChar(self: Val) bool {
    return switch (self.repr) {
        .char => true,
        else => false,
    };
}

/// Determines if a value is a string.
///
/// Args:
///   self: The value to test for string type.
///
/// Returns:
///   true if the value is a string, false otherwise.
pub fn isString(self: Val) bool {
    return switch (self.repr) {
        .string => true,
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

/// Determines if a value is a vector.
///
/// Args:
///   self: The value to test for vector type.
///
/// Returns:
///   true if the value is a vector, false otherwise.
pub fn isVector(self: Val) bool {
    return switch (self.repr) {
        .vector => true,
        else => false,
    };
}

/// Determines if a value is a bytevector.
///
/// Args:
///   self: The value to test for bytevector type.
///
/// Returns:
///   true if the value is a bytevector, false otherwise.
pub fn isBytevector(self: Val) bool {
    return switch (self.repr) {
        .bytevector => true,
        else => false,
    };
}

/// Determines if a value is a record.
///
/// Args:
///   self: The value to test for record type.
///
/// Returns:
///   true if the value is a record, false otherwise.
pub fn isRecord(self: Val) bool {
    return switch (self.repr) {
        .record => true,
        else => false,
    };
}

/// Determines if a value is a record type descriptor.
///
/// Args:
///   self: The value to test for record type descriptor type.
///
/// Returns:
///   true if the value is a record type descriptor, false otherwise.
pub fn isRecordTypeDescriptor(self: Val) bool {
    return switch (self.repr) {
        .record_type_descriptor => true,
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

/// Determines if two values are equal using structural comparison.
/// This performs a deep equality check comparing the internal representations
/// of both values, including all nested data structures.
///
/// Args:
///   self: The first value to compare.
///   other: The second value to compare.
///
/// Returns:
///   true if the values are structurally equal, false otherwise.
pub fn eq(self: Val, other: Val) bool {
    return std.meta.eql(self, other);
}

test "Val size is 16 bytes" {
    try testing.expectEqual(16, @sizeOf(Val));
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

    const proc = Procedure.Native{
        .name = "test-proc",
        .func = struct {
            fn func(_: Procedure.Context) Vm.Error!Val {
                return Val.init({});
            }
        }.func,
    };
    const proc_val = try vm.toVal(&proc);

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

    const proc = Procedure.Native{
        .name = "test-proc",
        .func = struct {
            fn func(_: Procedure.Context) Vm.Error!Val {
                return Val.init({});
            }
        }.func,
    };
    const proc_val = try vm.toVal(&proc);

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

test "isChar returns true for character values" {
    const char_a = Val.init(Char.init('a'));
    const char_space = Val.init(Char.init(' '));
    const char_null = Val.init(Char.init(0));

    try testing.expectEqual(true, char_a.isChar());
    try testing.expectEqual(true, char_space.isChar());
    try testing.expectEqual(true, char_null.isChar());
}

test "isChar returns false for non-character values" {
    const bool_val = Val.init(true);
    const int_val = Val.init(42);
    const nil_val = Val.init({});

    try testing.expectEqual(false, bool_val.isChar());
    try testing.expectEqual(false, int_val.isChar());
    try testing.expectEqual(false, nil_val.isChar());
}

test "character values are truthy" {
    const char_a = Val.init(Char.init('a'));
    const char_null = Val.init(Char.init(0));

    try testing.expectEqual(true, char_a.isTruthy());
    try testing.expectEqual(true, char_null.isTruthy());
}

test "isString returns true for string values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const string_val = try vm.toVal(String.initStatic("hello"));
    try testing.expectEqual(true, string_val.isString());
}

test "isString returns false for non-string values" {
    const bool_val = Val.init(true);
    const int_val = Val.init(42);
    const nil_val = Val.init({});
    const char_val = Val.init(Char.init('a'));

    try testing.expectEqual(false, bool_val.isString());
    try testing.expectEqual(false, int_val.isString());
    try testing.expectEqual(false, nil_val.isString());
    try testing.expectEqual(false, char_val.isString());
}

test "string values are truthy" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const string_val = try vm.toVal(String.initStatic("hello"));
    try testing.expectEqual(true, string_val.isTruthy());

    const empty_val = try vm.toVal(String.init());
    try testing.expectEqual(true, empty_val.isTruthy());
}

test "isVector returns true for vector values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const vector_val = try vm.toVal(Vector.init());
    try testing.expectEqual(true, vector_val.isVector());
}

test "isVector returns false for non-vector values" {
    const bool_val = Val.init(true);
    const int_val = Val.init(42);
    const nil_val = Val.init({});

    try testing.expectEqual(false, bool_val.isVector());
    try testing.expectEqual(false, int_val.isVector());
    try testing.expectEqual(false, nil_val.isVector());
}

test "vector values are truthy" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const vector_val = try vm.toVal(Vector.init());
    try testing.expectEqual(true, vector_val.isTruthy());

    const values = [_]Val{ Val.init(1), Val.init(2) };
    const filled_vector = try Vector.initFromSlice(testing.allocator, &values);
    const filled_vector_val = try vm.toVal(filled_vector);
    try testing.expectEqual(true, filled_vector_val.isTruthy());
}

test "isBytevector returns true for bytevector values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const bytevector_val = try vm.toVal(ByteVector.init());
    try testing.expectEqual(true, bytevector_val.isBytevector());
}

test "isBytevector returns false for non-bytevector values" {
    const bool_val = Val.init(true);
    const int_val = Val.init(42);
    const nil_val = Val.init({});

    try testing.expectEqual(false, bool_val.isBytevector());
    try testing.expectEqual(false, int_val.isBytevector());
    try testing.expectEqual(false, nil_val.isBytevector());
}

test "bytevector values are truthy" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const bytevector_val = try vm.toVal(ByteVector.init());
    try testing.expectEqual(true, bytevector_val.isTruthy());

    const bytes = [_]u8{ 0x01, 0x02 };
    const filled_bytevector = try ByteVector.initFromSlice(testing.allocator, &bytes);
    const filled_bytevector_val = try vm.toVal(filled_bytevector);
    try testing.expectEqual(true, filled_bytevector_val.isTruthy());
}

test "isRecord returns true for record values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const type_name = try vm.interner.internStatic(Symbol.init("person"));
    const field_names = [_]Symbol.Interned{};
    const descriptor = try Record.RecordTypeDescriptor.init(testing.allocator, type_name, &field_names);
    const descriptor_handle = try vm.builder().buildHandle(descriptor);

    const record = try Record.init(testing.allocator, &vm, descriptor_handle);
    const record_val = try vm.toVal(record);
    try testing.expectEqual(true, record_val.isRecord());
}

test "isRecord returns false for non-record values" {
    const bool_val = Val.init(true);
    const int_val = Val.init(42);
    const nil_val = Val.init({});

    try testing.expectEqual(false, bool_val.isRecord());
    try testing.expectEqual(false, int_val.isRecord());
    try testing.expectEqual(false, nil_val.isRecord());
}

test "record values are truthy" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const type_name = try vm.interner.internStatic(Symbol.init("person"));
    const field_names = [_]Symbol.Interned{};
    const descriptor = try Record.RecordTypeDescriptor.init(testing.allocator, type_name, &field_names);
    const descriptor_handle = try vm.builder().buildHandle(descriptor);

    const record = try Record.init(testing.allocator, &vm, descriptor_handle);
    const record_val = try vm.toVal(record);
    try testing.expectEqual(true, record_val.isTruthy());
}

test "isRecordTypeDescriptor returns true for record type descriptor values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const type_name = try vm.interner.internStatic(Symbol.init("person"));
    const field_names = [_]Symbol.Interned{};
    const descriptor = try Record.RecordTypeDescriptor.init(testing.allocator, type_name, &field_names);
    const descriptor_handle = try vm.builder().buildHandle(descriptor);
    const descriptor_val = Val.init(descriptor_handle);

    try testing.expectEqual(true, descriptor_val.isRecordTypeDescriptor());
}

test "isRecordTypeDescriptor returns false for non-record-type-descriptor values" {
    const bool_val = Val.init(true);
    const int_val = Val.init(42);
    const nil_val = Val.init({});

    try testing.expectEqual(false, bool_val.isRecordTypeDescriptor());
    try testing.expectEqual(false, int_val.isRecordTypeDescriptor());
    try testing.expectEqual(false, nil_val.isRecordTypeDescriptor());
}

test "record type descriptor values are truthy" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const type_name = try vm.interner.internStatic(Symbol.init("person"));
    const field_names = [_]Symbol.Interned{};
    const descriptor = try Record.RecordTypeDescriptor.init(testing.allocator, type_name, &field_names);
    const descriptor_handle = try vm.builder().buildHandle(descriptor);
    const descriptor_val = Val.init(descriptor_handle);

    try testing.expectEqual(true, descriptor_val.isTruthy());
}
