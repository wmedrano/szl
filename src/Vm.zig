//! Virtual machine and runtime environment for the Scheme interpreter.
//!
//! This module provides the core virtual machine that manages the execution
//! environment for Scheme programs. It handles symbol interning, value conversion,
//! and provides the foundation for evaluating Scheme expressions.

const std = @import("std");
const testing = std.testing;

const Builder = @import("Builder.zig");
const Handle = @import("object_pool.zig").Handle;
const Inspector = @import("Inspector.zig");
const ObjectPool = @import("object_pool.zig").ObjectPool;
const Pair = @import("Pair.zig");
const PrettyPrinter = @import("PrettyPrinter.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");

const Vm = @This();

allocator: std.mem.Allocator,
interner: Symbol.Interner,
pairs: ObjectPool(Pair),

/// Configuration options for initializing the virtual machine.
pub const Options = struct {
    /// The allocator to use for memory management within the VM.
    allocator: std.mem.Allocator,
};

/// Initializes a new virtual machine with the given options.
/// Creates a new symbol interner and sets up the runtime environment.
///
/// Args:
///   options: Configuration options including the allocator to use.
///
/// Returns:
///   A new Vm instance ready for use.
pub fn init(options: Options) Vm {
    const interner = Symbol.Interner.init(options.allocator);
    return Vm{
        .allocator = options.allocator,
        .interner = interner,
        .pairs = ObjectPool(Pair).init(),
    };
}

/// Releases all memory associated with the virtual machine.
/// This includes the symbol interner and all internal data structures.
/// After calling this function, the VM should not be used.
///
/// Args:
///   self: Pointer to the VM to deinitialize.
pub fn deinit(self: *Vm) void {
    self.interner.deinit();
    self.pairs.deinit(self.allocator);
}

/// Creates a PrettyPrinter for formatting a Scheme value.
/// Provides a formatted representation of values for debugging and display purposes.
///
/// Args:
///   self: Pointer to the VM that owns the value's data.
///   val: The Scheme value to format.
///
/// Returns:
///   A PrettyPrinter instance ready for use with Zig's standard formatting.
pub fn pretty(self: *Vm, val: Val) PrettyPrinter {
    return self.inspector().pretty(val);
}

/// Converts a Zig value to a Scheme value representation.
/// Provides type-safe conversion from compile-time known Zig types to the dynamic
/// value system used by the Scheme interpreter.
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
/// Provides type-safe conversion from the dynamic value system back to compile-time
/// known Zig types.
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

/// Evaluates a string of Scheme source code and returns the result.
/// Parses the source string into expressions using the Reader and evaluates
/// each expression in sequence, returning the value of the last expression.
///
/// Args:
///   self: Pointer to the VM instance.
///   source: The Scheme source code string to evaluate.
///
/// Returns:
///   The value of the last expression evaluated, or unit value if no expressions.
///
/// Errors:
///   - May return parsing errors from the Reader if the source is malformed.
///   - May return evaluation errors (when evaluation is implemented).
pub fn evalStr(self: *Vm, source: []const u8) !Val {
    var reader = self.builder().read(source);
    var ret = Val.init({});
    while (try reader.next()) |expr| {
        // TODO: Evaluate the expression. We return it for now.
        ret = expr;
    }
    return ret;
}

test "evalStr with single symbol returns symbol value" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.evalStr("hello");
    try testing.expectEqual(
        try vm.toVal(Symbol.init("hello")),
        result,
    );
}

test "evalStr with multiple expressions returns last expression value" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.evalStr("#t #f hello");
    try testing.expectEqual(
        try vm.toVal(Symbol.init("hello")),
        result,
    );
}

test "evalStr with empty string returns unit value" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.evalStr("");
    try testing.expectEqual(Val.init({}), result);
}

test "evalStr with list expression returns list value" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const result = try vm.evalStr("(hello world)");
    try testing.expectFmt(
        "(hello world)",
        "{f}",
        .{vm.pretty(result)},
    );
}
