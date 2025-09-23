//! Procedure representation for the Scheme interpreter.
//!
//! This module defines the core procedure types that can be called
//! in the Scheme interpreter. Procedures represent callable bytecode
//! functions, while native functions are handled separately as Native structs.

const std = @import("std");
const testing = std.testing;

const Instruction = @import("instruction.zig").Instruction;
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const Procedure = @This();

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

/// Native Zig function wrapper for procedures.
/// Contains a function pointer that takes a Context and returns a value.
pub const Native = struct {
    /// The name of the procedure.
    name: []const u8,
    /// Function pointer that implements the procedure logic.
    /// Takes a Context providing access to VM state and returns a value.
    func: *const fn (Context) Val,
};

/// Execution context for procedure calls.
/// Provides access to the VM state during procedure execution.
pub const Context = struct {
    vm: *Vm,

    /// Gets the local stack slice for the current procedure call.
    /// Returns the portion of the VM's stack that contains the arguments
    /// for the currently executing procedure.
    ///
    /// Returns:
    ///   A slice of Val containing the arguments for the current procedure.
    pub fn localStack(self: Context) []const Val {
        const frame = self.vm.current_stack_frame;
        return self.vm.stack.items[frame.stack_start..];
    }
};

test "Procedure is small" {
    try testing.expectEqual(40, @sizeOf(Procedure));
}

/// Deallocates resources associated with this procedure.
/// Frees the instructions array and resets the procedure to an empty state.
pub fn deinit(self: *Procedure, allocator: std.mem.Allocator) void {
    allocator.free(self.instructions);
    self.instructions = &.{};
}
