//! Virtual machine and runtime environment for the Scheme interpreter.
//!
//! This module provides the core virtual machine that manages the execution
//! environment for Scheme programs. It handles symbol interning, value conversion,
//! stack management with procedure call frames, and provides the foundation for
//! evaluating Scheme expressions.

const std = @import("std");
const testing = std.testing;

const Builder = @import("Builder.zig");
const builtins = @import("builtins.zig");
const Compiler = @import("Compiler.zig");
const Handle = @import("object_pool.zig").Handle;
const Inspector = @import("Inspector.zig");
const Instruction = @import("Instruction.zig");
const ObjectPool = @import("object_pool.zig").ObjectPool;
const Pair = @import("Pair.zig");
const PrettyPrinter = @import("PrettyPrinter.zig");
const Procedure = @import("Procedure.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");

const Vm = @This();

/// Memory allocator used for all dynamic allocations within the VM.
allocator: std.mem.Allocator,

/// Execution stack that holds values during computation and procedure calls.
/// Values are pushed and popped as expressions are evaluated.
stack: std.ArrayList(Val) = .{},

/// Stack of procedure call frames, enabling nested procedure calls and proper scoping.
/// Each frame tracks the stack position and execution state for a procedure call.
stack_frames: std.ArrayList(StackFrame) = .{},

/// The currently executing stack frame containing execution state.
/// Tracks the current instruction pointer and stack frame boundaries.
current_stack_frame: StackFrame = .{},

/// Global variable storage mapping interned symbols to their values.
/// Used for storing and retrieving globally defined variables and functions.
global_values: std.AutoHashMapUnmanaged(Symbol.Interned, Val) = .{},

/// Symbol interner for efficient string deduplication and comparison.
/// All symbols used in the VM are interned through this interner.
interner: Symbol.Interner,

/// Object pool for managing Pair objects with automatic memory recycling.
/// Provides efficient allocation and deallocation of cons cells for lists.
pairs: ObjectPool(Pair),

/// Object pool for managing Procedure objects with automatic memory recycling.
/// Stores both native and bytecode procedures for efficient reuse.
procedures: ObjectPool(Procedure),

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
pub fn init(options: Options) !Vm {
    const interner = Symbol.Interner.init(options.allocator);
    var vm = Vm{
        .allocator = options.allocator,
        .interner = interner,
        .pairs = ObjectPool(Pair).init(),
        .procedures = ObjectPool(Procedure).init(),
    };
    try builtins.register(&vm);
    return vm;
}

/// Releases all memory associated with the virtual machine.  This includes all
/// interned symbols and value.
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
    var reader = self.builder().read(source);
    var ret = Val.init({});
    while (try reader.next()) |expr| {
        const proc = try Compiler.compile(self, expr);
        ret = try self.evalProc(proc, &.{});
    }
    return ret;
}

test evalStr {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init({}),
        try vm.evalStr("(define (foo arg) (+ arg 2))"),
    );
    try testing.expectEqual(
        Val.init({}),
        try vm.evalStr("(define bar 40)"),
    );
    try testing.expectEqual(
        Val.init(42),
        try vm.evalStr("(foo bar)"),
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
    if (self.err) |_| return error.UnhandledError;
    try Instruction.load(self, proc);
    try Instruction.loadMany(self, args);
    const initial_call_stack_size = self.stack_frames.items.len;
    try Instruction.evalProcedure(self, args.len);
    while (self.stack_frames.items.len > initial_call_stack_size) {
        try Instruction.executeNext(self);
    }
    const return_value = self.stack.pop() orelse return error.StackUnderflow;
    return return_value;
}

test "evalStr with multiple expressions compiles last expression to procedure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        try vm.evalStr("#t #f 42"),
        Val.init(42),
    );
    try testing.expectEqual(
        try vm.evalStr("42 #f #t"),
        Val.init(true),
    );
}

test "evalStr with empty string returns unit value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init({}),
        try vm.evalStr(""),
    );
}

test "evalStr with expression evalutes it" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.init(42),
        try vm.evalStr("(+ 40 2)"),
    );
}
