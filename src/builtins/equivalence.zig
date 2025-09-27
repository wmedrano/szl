//! Equivalence operations for the Scheme interpreter.
//!
//! This module provides native implementations of equivalence predicates
//! including eq? which tests for object identity.

const std = @import("std");
const testing = std.testing;

const instruction = @import("../instruction.zig");
const Instruction = instruction.Instruction;
const NativeProc = @import("../NativeProc.zig");
const Proc = @import("../Proc.zig");
const ByteVector = @import("../types/ByteVector.zig");
const Pair = @import("../types/Pair.zig");
const Record = @import("../types/Record.zig");
const String = @import("../types/String.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vector = @import("../types/Vector.zig");
const Vm = @import("../Vm.zig");

/// Registers all equivalence functions with the virtual machine.
///
/// Args:
///   vm: Pointer to the VM instance to register equivalence functions with.
///
/// Errors:
///   May return allocation errors if registering functions fails.
pub fn register(vm: *Vm) !void {
    try vm.builder().defineNativeProc(&eq_func);
    try vm.builder().defineNativeProc(&eqv_func);
    try vm.builder().defineNativeProc(&equal_func);
}

/// Native implementation of the `eq?` equivalence predicate.
///
/// Tests for object identity - returns #t if both arguments refer to the same object.
/// For immediate values (nil, booleans, numbers, characters), this is value equality.
/// For heap objects (symbols, pairs, procedures), this tests reference equality.
///
/// Args:
///   arg1: The first value to compare
///   arg2: The second value to compare
///
/// Returns:
///   #t if the arguments are identical, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
const eq_func = NativeProc.Native{
    .name = "eq?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }
            return Val.init(args[0].eq(args[1]));
        }
    }.func,
};

/// Native implementation of the `eqv?` equivalence predicate.
///
/// Tests for equivalence - returns #t if both arguments are equivalent.
/// Similar to eq? but with slightly different semantics for some types.
///
/// Args:
///   arg1: The first value to compare
///   arg2: The second value to compare
///
/// Returns:
///   #t if the arguments are equivalent, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
const eqv_func = NativeProc.Native{
    .name = "eqv?",
    .func = eq_func.func,
};

/// Native implementation of the `equal?` equivalence predicate.
///
/// Tests for structural equality - returns #t if both arguments are recursively equal.
/// Performs deep comparison of nested structures like pairs, vectors, and records.
///
/// Args:
///   arg1: The first value to compare
///   arg2: The second value to compare
///
/// Returns:
///   #t if the arguments are structurally equal, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
const equal_func = NativeProc.Native{
    .name = "equal?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }
            const result = equalRecursive(ctx.vm, args[0], args[1]) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };
            return Val.init(result);
        }
    }.func,
};

/// Recursively compares two values for structural equality.
///
/// This function performs deep comparison of nested structures:
/// - For primitive values (nil, boolean, i64, f64, char, symbol): uses identity comparison
/// - For strings: compares string content
/// - For pairs: recursively compares both car and cdr
/// - For vectors: compares length and recursively compares all elements
/// - For bytevectors: compares length and all byte values
/// - For records: compares type descriptors and recursively compares all fields
/// - For procedures: uses identity comparison (same as eq?)
///
/// Args:
///   vm: Pointer to the VM instance for resolving handles
///   a: The first value to compare
///   b: The second value to compare
///
/// Returns:
///   true if the values are structurally equal, false otherwise
///
/// Errors:
///   May return errors if handle resolution fails
pub fn equalRecursive(vm: *Vm, a: Val, b: Val) !bool {
    if (a.eq(b)) return true;
    if (std.meta.activeTag(a.repr) != std.meta.activeTag(b.repr)) return false;
    return switch (a.repr) {
        // Types that use `eq?`.
        .nil,
        .boolean,
        .i64,
        .f64,
        .char,
        .symbol,
        .proc,
        .native_proc,
        .operator,
        .record_type_descriptor,
        .continuation,
        => false,
        // String comparison: compare content
        .string => |handle_a| {
            const handle_b = b.repr.string;
            const string_a = try vm.inspector().resolve(String, handle_a);
            const string_b = try vm.inspector().resolve(String, handle_b);
            return string_a.eql(string_b);
        },
        // Pair comparison: recursively compare car and cdr
        .pair => |handle_a| {
            const handle_b = b.repr.pair;
            const pair_a = try vm.inspector().resolve(Pair, handle_a);
            const pair_b = try vm.inspector().resolve(Pair, handle_b);
            return try equalRecursive(vm, pair_a.car, pair_b.car) and
                try equalRecursive(vm, pair_a.cdr, pair_b.cdr);
        },
        // Vector comparison: compare length and all elements
        .vector => |handle_a| {
            const handle_b = b.repr.vector;
            const vector_a = try vm.inspector().resolve(Vector, handle_a);
            const vector_b = try vm.inspector().resolve(Vector, handle_b);
            if (vector_a.len() != vector_b.len()) return false;

            for (vector_a.slice(), vector_b.slice()) |elem_a, elem_b| {
                if (!try equalRecursive(vm, elem_a, elem_b)) return false;
            }
            return true;
        },
        // Bytevector comparison: compare length and all bytes
        .bytevector => |handle_a| {
            const handle_b = b.repr.bytevector;
            const bytevector_a = try vm.inspector().resolve(ByteVector, handle_a);
            const bytevector_b = try vm.inspector().resolve(ByteVector, handle_b);
            if (bytevector_a.len() != bytevector_b.len()) return false;

            return std.mem.eql(u8, bytevector_a.slice(), bytevector_b.slice());
        },
        // Record comparison: compare type descriptors and all fields
        .record => |handle_a| {
            const record_a = try vm.inspector().resolve(Record, handle_a);
            const record_b = try vm.inspector().resolve(Record, b.repr.record);
            // Records must have the same type descriptor
            if (!record_a.type_descriptor_handle.eq(record_b.type_descriptor_handle)) return false;
            // Compare all field values
            if (record_a.fields.len != record_b.fields.len) return false;
            for (record_a.fields, record_b.fields) |field_a, field_b| {
                if (!try equalRecursive(vm, field_a, field_b)) return false;
            }
            return true;
        },
    };
}

test "eq? with same immediate values returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(eq? 42 42)");
    try vm.expectEval("#t", "(eq? #t #t)");
    try vm.expectEval("#t", "(eq? #f #f)");
    try vm.expectEval("#t", "(eq? #\\a #\\a)");
}

test "eq? with different immediate values returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(eq? 42 43)");
    try vm.expectEval("#f", "(eq? #t #f)");
    try vm.expectEval("#f", "(eq? #\\a #\\b)");
    try vm.expectEval("#f", "(eq? 42 #t)");
}

test "eq? with nil returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(eq? '() '())");
}

test "eq? with same symbols returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(eq? 'hello 'hello)");
}

test "eq? with different symbols returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(eq? 'hello 'world)");
}

test "eq? with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(eq?)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(eq? 1)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(eq? 1 2 3)"),
    );
}

test "eq? with mixed types returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(eq? 42 'foo)");
    try vm.expectEval("#f", "(eq? #t 1)");
    try vm.expectEval("#f", "(eq? '() #f)");
}

test "equal? with same immediate values returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(equal? 42 42)");
    try vm.expectEval("#t", "(equal? #t #t)");
    try vm.expectEval("#t", "(equal? #f #f)");
    try vm.expectEval("#t", "(equal? #\\a #\\a)");
    try vm.expectEval("#t", "(equal? '() '())");
    try vm.expectEval("#t", "(equal? 'hello 'hello)");
}

test "equal? with different immediate values returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(equal? 42 43)");
    try vm.expectEval("#f", "(equal? #t #f)");
    try vm.expectEval("#f", "(equal? #\\a #\\b)");
    try vm.expectEval("#f", "(equal? 'hello 'world)");
    try vm.expectEval("#f", "(equal? 42 #t)");
}

test "equal? with equivalent strings returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(equal? \"hello\" \"hello\")");
    try vm.expectEval("#t", "(equal? \"\" \"\")");
}

test "equal? with different strings returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(equal? \"hello\" \"world\")");
    try vm.expectEval("#f", "(equal? \"hello\" \"Hello\")");
}

test "equal? with equivalent pairs returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(equal? '(1 . 2) '(1 . 2))");
    try vm.expectEval("#t", "(equal? '(hello . world) '(hello . world))");
}

test "equal? with different pairs returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(equal? '(1 . 2) '(1 . 3))");
    try vm.expectEval("#f", "(equal? '(1 . 2) '(2 . 1))");
}

test "equal? with equivalent lists returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(equal? '(1 2 3) '(1 2 3))");
    try vm.expectEval("#t", "(equal? '(hello world) '(hello world))");
    try vm.expectEval("#t", "(equal? '() '())");
}

test "equal? with different lists returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(equal? '(1 2 3) '(1 2 4))");
    try vm.expectEval("#f", "(equal? '(1 2 3) '(1 2))");
    try vm.expectEval("#f", "(equal? '(1 2) '(1 2 3))");
}

test "equal? with nested lists returns correct result" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(equal? '((1 2) (3 4)) '((1 2) (3 4)))");
    try vm.expectEval("#f", "(equal? '((1 2) (3 4)) '((1 2) (3 5)))");
    try vm.expectEval("#t", "(equal? '(1 (2 (3 4))) '(1 (2 (3 4))))");
}

test "equal? with mixed nested structures returns correct result" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(equal? '(1 \"hello\" (2 3)) '(1 \"hello\" (2 3)))");
    try vm.expectEval("#f", "(equal? '(1 \"hello\" (2 3)) '(1 \"world\" (2 3)))");
    try vm.expectEval("#f", "(equal? '(1 \"hello\" (2 3)) '(1 \"hello\" (2 4)))");
}

test "equal? with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(equal?)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(equal? 1)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(equal? 1 2 3)"),
    );
}
