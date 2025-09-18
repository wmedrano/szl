//! Instruction representation and execution for the Scheme virtual machine.
//!
//! This module defines the instruction set that can be executed by the virtual
//! machine. Instructions represent atomic operations that manipulate the VM's
//! state, including stack operations and value loading.

const std = @import("std");
const testing = std.testing;

const Procedure = @import("Procedure.zig");
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
    /// Instruction to evaluate a procedure with arguments from the stack.
    /// The arg_count specifies how many arguments to pass to the procedure.
    eval_procedure: struct { arg_count: usize },
};

/// Executes this instruction on the given virtual machine.
/// Dispatches to the appropriate instruction handler based on the instruction type.
///
/// Supported instructions:
///   - load: Pushes a value onto the stack
///   - eval_procedure: Calls a procedure with specified number of arguments
///
/// Args:
///   self: The instruction to execute.
///   vm: Pointer to the virtual machine that will execute the instruction.
///
/// Errors:
///   - May return memory allocation errors if stack operations fail.
///   - May return StackUnderflow if stack frame operations fail.
pub fn execute(self: Instruction, vm: *Vm) !void {
    switch (self.repr) {
        .load => |val| return load(vm, val),
        .eval_procedure => |args| return eval_procedure(vm, args.arg_count),
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

/// Loads multiple values onto the virtual machine's stack.
/// Pushes each value from the slice to the top of the VM's execution stack.
///
/// Args:
///   vm: Pointer to the virtual machine whose stack will be modified.
///   vals: Slice of values to push onto the stack.
///
/// Errors:
///   - May return memory allocation errors if the stack cannot be expanded.
pub fn loadMany(vm: *Vm, vals: []const Val) !void {
    for (vals) |val| {
        try vm.stack.append(vm.allocator, val);
    }
}

/// Evaluates a procedure with the specified number of arguments.
/// Creates a new stack frame, calls the procedure function, and restores the previous frame.
/// The procedure and its arguments are expected to be on the stack, with the procedure
/// at the position just before the arguments.
///
/// Args:
///   vm: Pointer to the virtual machine that will execute the procedure.
///   arg_count: Number of arguments to pass to the procedure.
///
/// Errors:
///   - May return memory allocation errors if stack frame operations fail.
///   - May return StackUnderflow if there are no stack frames to restore.
pub fn eval_procedure(vm: *Vm, arg_count: usize) !void {
    try vm.stack_frames.append(vm.allocator, vm.current_stack_frame);
    vm.current_stack_frame = Vm.StackFrame{
        .stack_start = vm.stack.items.len - arg_count,
    };
    const procedure_idx = vm.current_stack_frame.stack_start - 1;
    const procedure = try vm.fromVal(Procedure, vm.stack.items[procedure_idx]);
    const return_val = procedure.func(vm);
    try vm.stack.resize(vm.allocator, procedure_idx + 1);
    vm.stack.items[procedure_idx] = return_val;
    vm.current_stack_frame = vm.stack_frames.pop() orelse return error.StackUnderflow;
}

test "execute load instruction pushes value onto stack" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try execute(Instruction{ .repr = .{ .load = Val.init(10) } }, &vm);
    try execute(Instruction{ .repr = .{ .load = Val.init(20) } }, &vm);
    try execute(Instruction{ .repr = .{ .load = Val.init(30) } }, &vm);

    try testing.expectFmt(
        "(10 20 30)",
        "{f}",
        .{vm.inspector().prettySlice(vm.stack.items)},
    );
}

test "execute eval_procedure instruction calls procedure with arguments" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const test_procedure = Procedure{
        .name = null,
        .func = struct {
            fn addTwo(vm_ptr: *Vm) Val {
                const args = Procedure.localStack(vm_ptr);
                const val1 = vm_ptr.fromVal(i64, args[0]) catch unreachable;
                const val2 = vm_ptr.fromVal(i64, args[1]) catch unreachable;
                return Val.init(val1 + val2);
            }
        }.addTwo,
    };
    try loadMany(&vm, &.{
        try vm.toVal(test_procedure),
        Val.init(10),
        Val.init(20),
    });

    try execute(Instruction{ .repr = .{ .eval_procedure = .{ .arg_count = 2 } } }, &vm);
    try testing.expectFmt(
        "(30)",
        "{f}",
        .{vm.inspector().prettySlice(vm.stack.items)},
    );
}
