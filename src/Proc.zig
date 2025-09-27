//! Procedure representation for the Scheme interpreter.
//!
//! This module defines the core procedure types that can be called
//! in the Scheme interpreter. Procedures represent callable bytecode
//! functions, while native functions are handled separately as Native structs.

const std = @import("std");
const testing = std.testing;

const Instruction = @import("instruction.zig").Instruction;
const Symbol = @import("types/Symbol.zig");
const Val = @import("types/Val.zig");
const Vm = @import("Vm.zig");

const Proc = @This();

/// The name of this procedure as an interned symbol.
/// This is used for identifying procedures in error messages,
/// debugging, and reflection.
name: ?Symbol.Interned = null,

/// The number of arguments for the procedure.
args: usize = 0,

/// The total number of local variable slots (arguments + local variables).
locals_count: usize = 0,

/// Array of instructions that implement the procedure logic.
instructions: []const Instruction,

/// Optimized operator procedures for common Scheme operations.
/// These operators provide more efficient implementations than bytecode
/// for frequently used operations like call-with-current-continuation.
pub const Operator = union(enum) {
    /// Unary call-with-current-continuation operator.
    /// Captures the current continuation and passes it to a procedure.
    unary_call_with_cc,

    /// Converts this operator to a Val for VM usage.
    ///
    /// Returns:
    ///   A Val containing this operator.
    pub fn toVal(self: Operator) Val {
        return Val.init(self);
    }
};

/// Deallocates resources associated with this procedure.
/// Frees the instructions array and resets the procedure to an empty state.
pub fn deinit(self: *Proc, allocator: std.mem.Allocator) void {
    allocator.free(self.instructions);
    self.instructions = &.{};
}

test "Procedure is small" {
    try testing.expectEqual(40, @sizeOf(Proc));
}
