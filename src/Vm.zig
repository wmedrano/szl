//! Virtual machine and runtime environment for the Scheme interpreter.
//!
//! This module provides the core virtual machine that manages the execution
//! environment for Scheme programs. It handles symbol interning, value conversion,
//! stack management with procedure call frames, and provides the foundation for
//! evaluating Scheme expressions.

const std = @import("std");
const testing = std.testing;

const Builder = @import("Builder.zig");
const builtins = @import("builtins/builtins.zig");
const Color = @import("object_pool.zig").Color;
const Compiler = @import("compiler/Compiler.zig");
const Handle = @import("object_pool.zig").Handle;
const Inspector = @import("Inspector.zig");
const instruction = @import("instruction.zig");
const Instruction = instruction.Instruction;
const ObjectPool = @import("object_pool.zig").ObjectPool;
const PrettyPrinter = @import("PrettyPrinter.zig");
const Procedure = @import("Procedure.zig");
const ByteVector = @import("types/ByteVector.zig");
const Pair = @import("types/Pair.zig");
const Record = @import("types/Record.zig");
const String = @import("types/String.zig");
const Symbol = @import("types/Symbol.zig");
const Val = @import("types/Val.zig");
const Vector = @import("types/Vector.zig");

const Vm = @This();

/// Standard errors that can occur during VM operations.
///
/// These errors represent the common failure modes that can happen during
/// virtual machine execution, compilation, and memory management.
pub const Error = error{
    /// Indicates that a Scheme-level error occurred during execution.
    /// This error is set when the VM encounters runtime errors such as
    /// undefined variables, type mismatches, or other semantic errors.
    UncaughtException,
    /// Indicates that the system ran out of available memory.
    /// This can occur during object allocation, stack growth, or other
    /// memory-intensive operations within the VM.
    OutOfMemory,
    StackUnderflow,
    StackOverflow,
};

/// Table of commonly used symbols that are pre-interned for efficient compilation.
///
/// This struct contains interned versions of symbols that the compiler needs
/// to recognize and handle specially during compilation. By pre-interning these
/// symbols, the compiler can perform fast symbol comparisons without needing
/// to intern symbols repeatedly during compilation.
const CommonSymbolTable = struct {
    // Language construct symbols
    @"*unspecified*": Symbol.Interned,
    @"=>": Symbol.Interned,
    @"else": Symbol.Interned,
    @"if": Symbol.Interned,
    @"let*": Symbol.Interned,
    @"szl-define": Symbol.Interned,
    begin: Symbol.Interned,
    cond: Symbol.Interned,
    define: Symbol.Interned,
    lambda: Symbol.Interned,
    let: Symbol.Interned,
    quote: Symbol.Interned,

    // Error symbols
    @"undefined-variable": Symbol.Interned,
    @"wrong-number-of-arguments": Symbol.Interned,
    @"invalid-procedure": Symbol.Interned,
    @"type-error": Symbol.Interned,
    @"division-by-zero": Symbol.Interned,
    @"invalid-argument": Symbol.Interned,
};

/// Memory allocator used for all dynamic allocations within the VM.
allocator: std.mem.Allocator,

/// Cached interned symbols used during compilation for efficient lookups.
common_symbols: CommonSymbolTable,

/// Execution stack that holds values during computation and procedure calls.
/// Values are pushed and popped as expressions are evaluated.
stack: std.ArrayList(Val),

/// Stack of procedure call frames, enabling nested procedure calls and proper scoping.
/// Each frame tracks the stack position and execution state for a procedure call.
stack_frames: std.ArrayList(StackFrame),

/// The currently executing stack frame containing execution state.
/// Tracks the current instruction pointer and stack frame boundaries.
current_stack_frame: StackFrame = .{},

/// Global variable storage mapping interned symbols to their values.
/// Used for storing and retrieving globally defined variables and functions.
global_values: std.AutoHashMapUnmanaged(Symbol.Interned, Val) = .{},

/// Symbol interner for efficient string deduplication and comparison.
/// All symbols used in the VM are interned through this interner.
interner: Symbol.Interner,

/// Current color used to mark reachable objects during garbage collection.
/// This color alternates between cycles to distinguish newly marked objects
/// from objects marked in the previous collection cycle.
reachable_color: Color,

/// Object pool for managing Pair objects with automatic memory recycling.
/// Provides efficient allocation and deallocation of cons cells for lists.
pairs: ObjectPool(Pair),

/// Object pool for managing Procedure objects with automatic memory recycling.
/// Stores both native and bytecode procedures for efficient reuse.
procedures: ObjectPool(Procedure),

/// Object pool for managing String objects with automatic memory recycling.
/// Provides efficient allocation and deallocation of string values.
strings: ObjectPool(String),

/// Object pool for managing Vector objects with automatic memory recycling.
/// Provides efficient allocation and deallocation of vector values.
vectors: ObjectPool(Vector),

/// Object pool for managing ByteVector objects with automatic memory recycling.
/// Provides efficient allocation and deallocation of bytevector values.
bytevectors: ObjectPool(ByteVector),

/// Object pool for managing Record objects with automatic memory recycling.
/// Provides efficient allocation and deallocation of record values.
records: ObjectPool(Record),

/// Object pool for managing RecordTypeDescriptor objects with automatic memory recycling.
/// Provides efficient allocation and deallocation of record type descriptors.
record_type_descriptors: ObjectPool(Record.RecordTypeDescriptor),

/// Error state for the virtual machine. When set, indicates that an error
/// has occurred during execution and should be handled or propagated.
err: ?Val = null,

/// Configuration options for initializing the virtual machine.
pub const Options = struct {
    /// The allocator to use for memory management within the VM.
    allocator: std.mem.Allocator,
};

/// Represents a stack frame for procedure calls.
///
/// Each frame tracks the start position of its arguments on the stack, enabling
/// proper scoping and parameter access during procedure execution.
pub const StackFrame = struct {
    /// Index into the stack where this frame's local variables and arguments begin.
    /// Used as the base address for calculating absolute positions of local variables.
    stack_start: usize = 0,
    /// Bytecode instructions for this stack frame to execute.
    /// Contains the compiled procedure's instruction sequence.
    instructions: []const Instruction = &.{},
    /// Current instruction pointer within the instructions array.
    /// Points to the next instruction to be executed in this frame.
    instruction_idx: usize = 0,
};

/// Initializes a new virtual machine with the given options.
///
/// Creates a new symbol interner, sets up the runtime environment, initializes
/// the execution stack and stack frame management for procedure calls.
///
/// Args:
///   options: Configuration options including the allocator to use.
///
/// Returns:
///   A new Vm instance ready for use with stack frame support.
pub fn init(options: Options) error{OutOfMemory}!Vm {
    const interner = Symbol.Interner.init(options.allocator);
    var vm = Vm{
        .allocator = options.allocator,
        .common_symbols = undefined,
        .stack = try std.ArrayList(Val).initCapacity(options.allocator, 1024),
        .stack_frames = try std.ArrayList(StackFrame).initCapacity(options.allocator, 64),
        .interner = interner,
        .reachable_color = Color.blue,
        .pairs = ObjectPool(Pair).init(),
        .procedures = ObjectPool(Procedure).init(),
        .strings = ObjectPool(String).init(),
        .vectors = ObjectPool(Vector).init(),
        .bytevectors = ObjectPool(ByteVector).init(),
        .records = ObjectPool(Record).init(),
        .record_type_descriptors = ObjectPool(Record.RecordTypeDescriptor).init(),
    };
    vm.common_symbols = try vm.builder().symbolTable(CommonSymbolTable);
    try builtins.register(&vm);
    return vm;
}

/// Releases all memory associated with the virtual machine.
/// This includes all interned symbols and values.
///
/// Args:
///   self: Pointer to the VM to deinitialize.
pub fn deinit(self: *Vm) void {
    self.stack.deinit(self.allocator);
    self.stack_frames.deinit(self.allocator);
    self.global_values.deinit(self.allocator);
    self.interner.deinit();
    self.pairs.deinit(self.allocator);

    var proc_iter = self.procedures.iterator();
    while (proc_iter.next()) |proc| proc.deinit(self.allocator);
    self.procedures.deinit(self.allocator);

    var string_iter = self.strings.iterator();
    while (string_iter.next()) |string| string.deinit(self.allocator);
    self.strings.deinit(self.allocator);

    var vector_iter = self.vectors.iterator();
    while (vector_iter.next()) |vector| vector.deinit(self.allocator);
    self.vectors.deinit(self.allocator);

    var bytevector_iter = self.bytevectors.iterator();
    while (bytevector_iter.next()) |bytevector| bytevector.deinit(self.allocator);
    self.bytevectors.deinit(self.allocator);

    var record_iter = self.records.iterator();
    while (record_iter.next()) |record| record.deinit(self.allocator);
    self.records.deinit(self.allocator);

    var record_type_descriptor_iter = self.record_type_descriptors.iterator();
    while (record_type_descriptor_iter.next()) |descriptor| descriptor.deinit(self.allocator);
    self.record_type_descriptors.deinit(self.allocator);
}

fn PrettyReturnType(comptime T: type) type {
    return switch (T) {
        Val => PrettyPrinter,
        []const Val, []Val => PrettyPrinter.Slice,
        else => @compileError("pretty() only accepts Val, []const Val, or []Val, got " ++ @typeName(T)),
    };
}

/// Creates a PrettyPrinter for formatting a Scheme value or slice of values.
///
/// Provides a formatted representation of values for debugging and display
/// purposes.
///
/// Args:
///   self: Pointer to the VM that owns the value's data.
///   val: The Scheme value to format, or a slice of values ([]const Val or []Val).
///
/// Returns:
///   A PrettyPrinter or PrettyPrinter.Slice instance ready for use with Zig's standard formatting.
pub fn pretty(self: *Vm, val: anytype) PrettyReturnType(@TypeOf(val)) {
    const T = @TypeOf(val);
    return switch (T) {
        Val => self.inspector().pretty(val),
        []const Val, []Val => self.inspector().prettySlice(val),
        else => unreachable,
    };
}

/// Converts a Zig value to a Scheme value representation.
///
/// Provides type-safe conversion from compile-time known Zig types to the
/// dynamic value system used by the Scheme interpreter.
///
/// Args:
///   self: Pointer to the VM.
///   val: The value to convert to a Scheme value.
///
/// Returns:
///   A Val representing the converted value, or a compile error for unsupported types.
pub fn toVal(self: *Vm, val: anytype) !Val {
    return self.builder().build(val);
}

/// Converts a Scheme value to a Zig type.
///
/// Provides type-safe conversion from the dynamic value system back to
/// compile-time known Zig types.
///
/// Args:
///   self: Pointer to the VM.
///   T: The target Zig type to convert to.
///   val: The Scheme value to convert.
///
/// Returns:
///   The converted value of type T, or an error if the conversion is not possible.
pub fn fromVal(self: *const Vm, T: type, val: Val) !T {
    return self.inspector().to(T, val);
}

/// Creates a new Builder instance for converting Zig values to Scheme values.
/// The Builder provides type-safe conversion from compile-time known Zig types
/// to the dynamic value system used by the Scheme interpreter.
///
/// Args:
///   self: Pointer to the VM that will be used by the Builder.
///
/// Returns:
///   A Builder instance configured to use this VM.
pub fn builder(self: *Vm) Builder {
    return Builder{ .vm = self };
}

/// Creates an Inspector instance for examining and formatting Scheme values.
/// The Inspector provides utilities for type-safe conversion from the dynamic
/// value system back to compile-time known Zig types and pretty-printing capabilities.
///
/// Args:
///   self: Pointer to the VM that will be used by the Inspector.
///
/// Returns:
///   An Inspector instance configured to use this VM.
pub fn inspector(self: *const Vm) Inspector {
    return Inspector{ .vm = self };
}

pub fn reset(self: *Vm) void {
    self.err = null;
    self.stack.clearRetainingCapacity();
    self.stack_frames.clearRetainingCapacity();
    self.current_stack_frame = StackFrame{};
}

/// Compiles a string of Scheme source code and returns the result.
/// Parses the source string into expressions using the Reader and compiles
/// each expression to bytecode, returning a procedure for the last expression.
///
/// Args:
///   self: Pointer to the VM instance.
///   source: The Scheme source code string to compile.
///
/// Returns:
///   A procedure containing compiled bytecode for the last expression, or unit value if no expressions.
///
/// Errors:
///   - May return parsing errors from the Reader if the source is malformed.
///   - May return compilation errors if expressions cannot be compiled to bytecode.
pub fn evalStr(self: *Vm, source: []const u8) !Val {
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    defer arena.deinit();
    var reader = self.builder().read(source);
    var ret = Val.init({});
    while (try reader.next()) |expr| {
        const proc = try Compiler.compile(self, &arena, expr);
        _ = arena.reset(.retain_capacity);
        ret = try self.evalProc(proc, &.{});
    }
    return ret;
}

test evalStr {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    _ = try vm.evalStr("(define (foo arg) (+ arg 2))");
    _ = try vm.evalStr("(define bar 40)");
    try testing.expectEqual(
        Val.init(42),
        try vm.evalStr("(foo bar)"),
    );

    const define_fib =
        \\ (define (fib n)
        \\   (if (<= n 2)
        \\     1
        \\     (+ (fib (- n 1))
        \\        (fib (- n 2)))))
    ;
    _ = try vm.evalStr(define_fib);
    try testing.expectEqual(
        Val.init(55),
        try vm.evalStr("(fib 10)"),
    );
}

/// Evaluates a compiled procedure by executing its bytecode instructions.
///
/// Loads the procedure into the VM's execution environment and runs it to completion,
/// managing the call stack and returning the final result value.
///
/// Args:
///   self: Pointer to the VM instance.
///   proc: The compiled procedure value containing bytecode to execute.
///   args: Arguments to pass to the procedure.
///
/// Returns:
///   The final result value after executing all instructions in the procedure.
///
/// Errors:
///   - StackUnderflow if the execution stack becomes empty unexpectedly.
///   - Any errors that may occur during instruction execution.
fn evalProc(self: *Vm, proc: Val, args: []const Val) !Val {
    if (self.err) |_| return Vm.Error.UncaughtException;
    try instruction.load(self, proc);
    try instruction.loadMany(self, args);
    const initial_call_stack_size = self.stack_frames.items.len;
    try instruction.evalProc(self, args.len);
    while (self.stack_frames.items.len > initial_call_stack_size) {
        try instruction.executeNext(self);
    }
    const return_value = self.stack.pop() orelse return error.StackUnderflow;
    return return_value;
}

/// Runs a garbage collection cycle to reclaim memory from unreachable objects.
///
/// This function performs a mark-and-sweep garbage collection algorithm:
/// 1. Mark phase: Starting from GC roots (stack values and global variables),
///    mark all reachable objects with the current reachable_color
/// 2. Sweep phase: Iterate through all object pools and deallocate objects
///    that were not marked during the mark phase
/// 3. Color swap: Prepare for the next GC cycle by swapping the reachable_color
///
/// GC roots include:
/// - All values currently on the execution stack
/// - All globally defined variables and functions
/// - Note: Call frames are not explicit roots since executing procedures
///   are always present on the stack
///
/// Returns:
///   An error if memory allocation fails during the marking process.
pub fn runGc(self: *Vm) !void {
    // Mark phase: mark all reachable objects starting from GC roots
    // Note: Call frames are not marked as GC roots because the executing
    // procedure is always on the stack at position -1, so it's already marked

    // Mark objects on the execution stack
    for (self.stack.items) |val| try self.markOne(val);

    // Mark global variables
    var global_iter = self.global_values.valueIterator();
    while (global_iter.next()) |val| try self.markOne(val.*);

    // Sweep
    var string_sweep = self.strings.sweep(self.allocator, self.reachable_color);
    while (try string_sweep.next()) |string| string.deinit(self.allocator);
    var proc_sweep = self.procedures.sweep(self.allocator, self.reachable_color);
    while (try proc_sweep.next()) |proc| proc.deinit(self.allocator);
    var vector_sweep = self.vectors.sweep(self.allocator, self.reachable_color);
    while (try vector_sweep.next()) |vector| vector.deinit(self.allocator);
    var bytevector_sweep = self.bytevectors.sweep(self.allocator, self.reachable_color);
    while (try bytevector_sweep.next()) |bytevector| bytevector.deinit(self.allocator);
    var record_sweep = self.records.sweep(self.allocator, self.reachable_color);
    while (try record_sweep.next()) |record| record.deinit(self.allocator);
    var record_type_descriptor_sweep = self.record_type_descriptors.sweep(self.allocator, self.reachable_color);
    while (try record_type_descriptor_sweep.next()) |descriptor| descriptor.deinit(self.allocator);
    var pair_sweep = self.pairs.sweep(self.allocator, self.reachable_color);
    while (try pair_sweep.next()) |_| {} // Pairs don't require cleanup

    // Prepare for next iteration
    self.reachable_color = self.reachable_color.other();
}

/// Marks a single value and all values it references as reachable during garbage collection.
///
/// This function recursively traverses object references to mark all reachable objects
/// with the current reachable_color. Objects that are already marked with the current
/// color are skipped to avoid infinite recursion and redundant work.
///
/// Marking behavior by value type:
/// - Primitive values (nil, boolean, i64, f64, char, symbol): No marking needed
/// - String, bytevector: Mark the object but no further traversal required
/// - Pair: Mark the pair and recursively mark both car and cdr
/// - Procedure: Mark the procedure and any embedded values in bytecode instructions
/// - Vector: Mark the vector and recursively mark all contained elements
/// - Record: Mark the record, its type descriptor, and all field values
/// - Record type descriptor: Mark the descriptor (no further traversal needed)
///
/// Parameters:
///   val: The value to mark as reachable
///
/// Returns:
///   An error if recursive marking encounters memory allocation issues.
fn markOne(self: *Vm, val: Val) !void {
    switch (val.repr) {
        // These types don't require marking:
        // - Primitive values (.nil, .boolean, .i64, .f64, .char) are stored inline
        // - Symbols are never garbage collected and live for the program's lifetime
        .nil, .boolean, .i64, .f64, .char, .symbol, .native_proc => {},
        .string => |h| {
            _ = self.strings.setColor(h, self.reachable_color);
        },
        .pair => |h| {
            if (self.pairs.setColor(h, self.reachable_color)) |pair| {
                try self.markOne(pair.car);
                try self.markOne(pair.cdr);
            }
        },
        .proc => |h| {
            if (self.procedures.setColor(h, self.reachable_color)) |proc| {
                for (proc.instructions) |inst| {
                    switch (inst) {
                        .load => |v| try self.markOne(v),
                        else => {},
                    }
                }
            }
        },
        .vector => |h| {
            if (self.vectors.setColor(h, self.reachable_color)) |vector| {
                for (vector.slice()) |item| try self.markOne(item);
            }
        },
        .bytevector => |h| {
            _ = self.bytevectors.setColor(h, self.reachable_color);
        },
        .record => |h| {
            if (self.records.setColor(h, self.reachable_color)) |record| {
                try self.markOne(Val.init(record.type_descriptor_handle));
                for (record.fields) |item| try self.markOne(item);
            }
        },
        .record_type_descriptor => |h| {
            _ = self.record_type_descriptors.setColor(h, self.reachable_color);
        },
    }
}

test runGc {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.runGc();
}

/// Test helper function that evaluates a Scheme expression and compares it to an expected value.
///
/// This function is designed for use in test cases to verify that Scheme expressions
/// evaluate to expected values. It first parses the expected string as a Scheme value
/// and performs structural equality comparison. If that fails, it falls back to
/// formatted string comparison for better error messages.
///
/// Args:
///   self: Pointer to the VM instance to use for evaluation.
///   expected: The expected Scheme value as a string (e.g., "42", "#t", "(a b c)").
///   input: The Scheme expression string to evaluate.
///
/// Errors:
///   - Any errors from evalStr() during expression evaluation.
///   - Any errors from parsing the expected value string.
///   - Test failure if neither structural equality nor formatted output matches.
pub fn expectEval(self: *Vm, expected: []const u8, input: []const u8) !void {
    self.reset();
    const expected_val = try self.builder().readOne(expected);
    const actual_val = self.evalStr(input) catch |e| {
        if (self.err) |err| try testing.expectFmt("", "{f}", .{self.pretty(err)});
        return e;
    };
    if (std.meta.eql(expected_val, actual_val)) return;
    try testing.expectFmt(expected, "{f}", .{self.pretty(actual_val)});
    switch (expected_val.repr) {
        .nil, .boolean, .i64, .f64, .char, .symbol, .native_proc => try testing.expectEqual(expected_val, actual_val),
        .string, .pair, .proc, .vector, .bytevector, .record, .record_type_descriptor => {},
    }
}

/// Asserts that evaluating the given Scheme source results in an error,
/// and that the VM's error state matches the expected error value.
///
/// This function is used in tests to verify that evaluation of invalid
/// Scheme code fails as expected. It ensures:
///   - `evalStr` returns an error,
///   - `vm.err` is set (i.e., the VM recorded an error),
///   - `vm.err` equals the expected error `Val` (typically a symbol like
///     `vm.common_symbols.@"undefined-variable"`).
///
/// After the check, the VM's error state is cleared to avoid affecting other tests.
///
/// Args:
///   vm: The virtual machine instance to evaluate code with.
///   source: The Scheme expression string to evaluate.
///   expected_err: A `Val` representing the expected error (usually an interned symbol).
///
/// Errors:
///   - Returns a test failure if the evaluation succeeds unexpectedly,
///     or if the VM's error state doesn't match `expected_err`.
pub fn expectEvalErr(self: *Vm, expected: []const u8, source: []const u8) !void {
    self.reset();
    try testing.expectError(Vm.Error.UncaughtException, self.evalStr(source));
    const expected_err = try self.builder().readOne(expected);
    const actual_err = self.err orelse return error.ErrorNotFound;
    if (std.meta.eql(expected_err, actual_err)) return;
    try testing.expectFmt(expected, "{f}", .{self.pretty(actual_err)});
    switch (expected_err.repr) {
        .nil, .boolean, .i64, .f64, .char, .symbol, .native_proc => try testing.expectEqual(expected_err, actual_err),
        .string, .pair, .proc, .vector, .bytevector, .record, .record_type_descriptor => {},
    }
}

/// Test helper function that reads a single Scheme expression and validates both its type and formatted output.
///
/// This function parses a single Scheme expression from the input string and performs validation
/// against expected results. It can optionally verify the value's type tag and always checks
/// that the formatted output matches the expected string representation.
///
/// Args:
///   self: Pointer to the VM instance to use for reading and formatting.
///   expected_tag: Optional type tag to verify against the parsed value's type.
///   expected: The expected formatted string representation of the parsed value.
///   input: The Scheme expression string to parse and validate.
///
/// Errors:
///   - Any errors from readOne() during expression parsing.
///   - Test failure if the type tag doesn't match (when expected_tag is provided).
///   - Test failure if the formatted output doesn't match the expected string.
pub fn expectReadOne(self: *Vm, expected_tag: ?std.meta.Tag(Val.Repr), expected: []const u8, input: []const u8) !void {
    const actual_val = try self.builder().readOne(input);
    if (expected_tag) |tag|
        try testing.expectEqual(tag, std.meta.activeTag(actual_val.repr));
    try testing.expectFmt(expected, "{f}", .{self.pretty(actual_val)});
}

test "evalStr with multiple expressions compiles last expression to procedure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("42", "#t #f 42");
    try vm.expectEval("#t", "42 #f #t");
}

test "evalStr with empty string returns unit value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("()", "");
}

test "evalStr with expression evalutes it" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("42", "(+ 40 2)");
}

test "garbage collection with reachable strings" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Create a string and put it on the stack to keep it reachable
    const test_string = String.initStatic("hello world");
    const string_handle = try vm.strings.put(vm.allocator, test_string, vm.reachable_color.other());
    const string_val = Val.init(string_handle);
    try vm.stack.append(vm.allocator, string_val);

    // Create another string that's not on the stack (unreachable)
    const unreachable_string = String.initStatic("unreachable");
    _ = try vm.strings.put(vm.allocator, unreachable_string, vm.reachable_color.other());

    // Initial count should be 2
    try testing.expectEqual(2, vm.strings.objects.items.len - vm.strings.free.items.len);

    // Run GC - should keep the string on the stack
    try vm.runGc();

    // Should still have 1 string (the reachable one)
    try testing.expectEqual(1, vm.strings.objects.items.len - vm.strings.free.items.len);

    // Verify the reachable string is still accessible
    const resolved = vm.strings.get(string_handle);
    try testing.expect(resolved != null);
}

test "garbage collection with reachable pairs" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Create a pair and put it on the stack
    const car = Val.init(42);
    const cdr = Val.init(true);
    const test_pair = Pair{ .car = car, .cdr = cdr };
    const pair_handle = try vm.pairs.put(vm.allocator, test_pair, vm.reachable_color.other());
    const pair_val = Val.init(pair_handle);
    try vm.stack.append(vm.allocator, pair_val);

    // Create an unreachable pair
    const unreachable_pair = Pair{ .car = Val.init(1), .cdr = Val.init(2) };
    _ = try vm.pairs.put(vm.allocator, unreachable_pair, vm.reachable_color.other());

    try testing.expectEqual(2, vm.pairs.objects.items.len - vm.pairs.free.items.len);

    try vm.runGc();

    try testing.expectEqual(1, vm.pairs.objects.items.len - vm.pairs.free.items.len);

    // Verify the reachable pair is still accessible
    const resolved = vm.pairs.get(pair_handle);
    try testing.expect(resolved != null);
}

test "garbage collection with nested pairs" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Create nested pairs: (1 . (2 . 3))
    const inner_pair = Pair{ .car = Val.init(2), .cdr = Val.init(3) };
    const inner_handle = try vm.pairs.put(vm.allocator, inner_pair, vm.reachable_color.other());

    const outer_pair = Pair{ .car = Val.init(1), .cdr = Val.init(inner_handle) };
    const outer_handle = try vm.pairs.put(vm.allocator, outer_pair, vm.reachable_color.other());

    // Put only the outer pair on the stack
    try vm.stack.append(vm.allocator, Val.init(outer_handle));

    // Create an unreachable pair
    const unreachable_pair = Pair{ .car = Val.init(99), .cdr = Val.init(100) };
    _ = try vm.pairs.put(vm.allocator, unreachable_pair, vm.reachable_color.other());

    try testing.expectEqual(3, vm.pairs.objects.items.len - vm.pairs.free.items.len);

    try vm.runGc();

    // Should keep both reachable pairs (outer and inner)
    try testing.expectEqual(2, vm.pairs.objects.items.len - vm.pairs.free.items.len);

    // Both pairs should still be accessible
    try testing.expect(vm.pairs.get(outer_handle) != null);
    try testing.expect(vm.pairs.get(inner_handle) != null);
}

test "garbage collection with vectors containing strings" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Create strings
    const hello_string = String.initStatic("hello");
    const hello_handle = try vm.strings.put(vm.allocator, hello_string, vm.reachable_color.other());

    const world_string = String.initStatic("world");
    const world_handle = try vm.strings.put(vm.allocator, world_string, vm.reachable_color.other());

    // Create vector containing the strings
    const values = [_]Val{ Val.init(hello_handle), Val.init(world_handle) };
    const test_vector = try Vector.initFromSlice(vm.allocator, &values);
    const vector_handle = try vm.vectors.put(vm.allocator, test_vector, vm.reachable_color.other());

    // Put vector on stack
    try vm.stack.append(vm.allocator, Val.init(vector_handle));

    // Create unreachable string
    const unreachable_string = String.initStatic("unreachable");
    _ = try vm.strings.put(vm.allocator, unreachable_string, vm.reachable_color.other());

    try testing.expectEqual(1, vm.vectors.objects.items.len - vm.vectors.free.items.len);
    try testing.expectEqual(3, vm.strings.objects.items.len - vm.strings.free.items.len);

    try vm.runGc();

    // Vector and its contained strings should be preserved
    try testing.expectEqual(1, vm.vectors.objects.items.len - vm.vectors.free.items.len);
    try testing.expectEqual(2, vm.strings.objects.items.len - vm.strings.free.items.len);

    // Verify objects are still accessible
    try testing.expect(vm.vectors.get(vector_handle) != null);
    try testing.expect(vm.strings.get(hello_handle) != null);
    try testing.expect(vm.strings.get(world_handle) != null);
}

test "garbage collection with empty stack" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Create some objects but don't put them on the stack
    const test_string = String.initStatic("test");
    _ = try vm.strings.put(vm.allocator, test_string, vm.reachable_color.other());

    const test_pair = Pair{ .car = Val.init(1), .cdr = Val.init(2) };
    _ = try vm.pairs.put(vm.allocator, test_pair, vm.reachable_color.other());

    try testing.expectEqual(1, vm.strings.objects.items.len - vm.strings.free.items.len);
    try testing.expectEqual(1, vm.pairs.objects.items.len - vm.pairs.free.items.len);

    // With empty stack, all objects should be collected
    try vm.runGc();

    try testing.expectEqual(0, vm.strings.objects.items.len - vm.strings.free.items.len);
    try testing.expectEqual(0, vm.pairs.objects.items.len - vm.pairs.free.items.len);
}
