//! Instruction representation and execution for the Scheme virtual machine.
//!
//! This module defines the instruction set that can be executed by the virtual
//! machine. Instructions represent atomic operations that manipulate the VM's
//! state, including stack operations and value loading.

const std = @import("std");
const testing = std.testing;

const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const Instruction = @This();

/// The internal representation of this instruction.
/// Contains the instruction type and any associated data.
repr: Repr,

/// Tagged union representing all possible instruction types in the virtual machine.
/// Each variant corresponds to a different operation that can be performed
/// during program execution.
pub const Repr = union(enum) {
    /// Instruction to load a value onto the stack.
    /// The associated value will be pushed to the top of the VM's stack.
    load: Val,
};

/// Executes this instruction on the given virtual machine.
/// Dispatches to the appropriate instruction handler based on the instruction type.
///
/// Args:
///   self: The instruction to execute.
///   vm: Pointer to the virtual machine that will execute the instruction.
///
/// Errors:
///   - May return memory allocation errors if stack operations fail.
pub fn execute(self: Instruction, vm: *Vm) !void {
    switch (self.repr) {
        .load => |val| return load(vm, val),
    }
}

/// Loads a value onto the virtual machine's stack.
/// Pushes the given value to the top of the VM's execution stack.
///
/// Args:
///   vm: Pointer to the virtual machine whose stack will be modified.
///   val: The value to push onto the stack.
///
/// Errors:
///   - May return memory allocation errors if the stack cannot be expanded.
pub fn load(vm: *Vm, val: Val) !void {
    return vm.stack.append(vm.allocator, val);
}

test "execute load instruction pushes value onto stack" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try (Instruction{ .repr = .{ .load = Val.init(10) } })
        .execute(&vm);
    try (Instruction{ .repr = .{ .load = Val.init(20) } })
        .execute(&vm);
    try (Instruction{ .repr = .{ .load = Val.init(30) } })
        .execute(&vm);

    try testing.expectFmt(
        "(10 20 30)",
        "{f}",
        .{vm.inspector().prettySlice(vm.stack.items)},
    );
}
