//! Native Zig function wrapper for procedures.
//!
//! This module defines native procedure types that can be called
//! in the Scheme interpreter. Native procedures are implemented
//! in Zig and provide efficient implementations for built-in functions.

const std = @import("std");

const Val = @import("types/Val.zig");
const Vm = @import("Vm.zig");

/// Native Zig function wrapper for procedures.
/// Contains a function pointer that takes a Context and returns a value.
pub const Native = struct {
    /// The name of the procedure.
    name: []const u8,
    /// Function pointer that implements the procedure logic.
    /// Takes a Context providing access to VM state and returns a value.
    func: *const fn (NativeContext) Vm.Error!Val,
};

/// Execution context for procedure calls.
/// Provides access to the VM state during procedure execution.
pub const NativeContext = struct {
    vm: *Vm,

    /// Gets the local stack slice for the current procedure call.
    /// Returns the portion of the VM's stack that contains the arguments
    /// for the currently executing procedure.
    ///
    /// Returns:
    ///   A slice of Val containing the arguments for the current procedure.
    pub fn localStack(self: NativeContext) []const Val {
        const frame = self.vm.context.current_stack_frame;
        return self.vm.context.stack()[frame.stack_start..];
    }
};