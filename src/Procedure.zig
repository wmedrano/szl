//! Procedure representation for the Scheme interpreter.
//!
//! This module defines the core procedure types that can be called
//! in the Scheme interpreter. Procedures represent callable values
//! including both built-in and user-defined functions.

const std = @import("std");
const testing = std.testing;

const Instruction = @import("Instruction.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const Procedure = @This();

/// The name of this procedure as an interned symbol.
/// This is used for identifying procedures in error messages,
/// debugging, and reflection.
name: ?Symbol.Interned,

/// The implementation for this procedure.
/// Can be either a native Zig function or compiled bytecode.
implementation: Impl,

/// Implementation types for procedures.
/// Procedures can be implemented either as native Zig functions
/// or as compiled bytecode instructions.
pub const Impl = union(enum) {
    /// Native Zig function implementation.
    native: Native,
    /// Bytecode implementation with compiled instructions.
    bytecode: Bytecode,
};

/// Native Zig function wrapper for procedures.
/// Contains a function pointer that takes a VM and returns a value.
pub const Native = struct {
    /// Function pointer that implements the procedure logic.
    /// Takes a pointer to the VM and returns a value.
    func: *const fn (*Vm) Val,
};

/// Bytecode implementation for procedures.
/// Contains compiled instructions that will be executed by the VM.
pub const Bytecode = struct {
    /// Array of instructions that implement the procedure logic.
    instructions: []const Instruction,
};

/// Gets the local stack slice for the current procedure call.
/// Returns the portion of the VM's stack that contains the arguments
/// for the currently executing procedure.
///
/// Args:
///   vm: Pointer to the VM whose local stack will be returned.
///
/// Returns:
///   A slice of Val containing the arguments for the current procedure.
pub fn localStack(vm: *const Vm) []const Val {
    const frame = vm.current_stack_frame;
    return vm.stack.items[frame.stack_start..];
}
