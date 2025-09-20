//! Built-in functions and procedures for the Scheme interpreter.
//!
//! This module provides the registration of native built-in functions that
//! are available to Scheme programs at runtime. These functions form the
//! core standard library of operations available in the interpreter.

const std = @import("std");
const testing = std.testing;

const Instruction = @import("../Instruction.zig");
const Procedure = @import("../Procedure.zig");
const Symbol = @import("../Symbol.zig");
const Val = @import("../Val.zig");
const Vm = @import("../Vm.zig");

const arithmetic = @import("arithmetic.zig");
const comparison = @import("comparison.zig");
const define = @import("define.zig");
const math = @import("math.zig");

/// Registers all built-in functions with the virtual machine.
///
/// This function should be called during VM initialization to make all
/// standard Scheme built-in procedures available to executing programs.
///
/// Args:
///   vm: Pointer to the VM instance to register built-ins with.
///
/// Errors:
///   May return allocation errors if registering built-ins fails.
pub fn register(vm: *Vm) !void {
    try define.register(vm);
    try arithmetic.register(vm);
    try comparison.register(vm);
    try math.register(vm);
}

// Reference the individual modules to include their tests
test {
    _ = arithmetic;
    _ = comparison;
    _ = define;
    _ = math;
}