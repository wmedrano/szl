//! Instruction representation and execution for the Scheme virtual machine.
//!
//! This module defines the instruction set that can be executed by the virtual
//! machine. Instructions represent atomic operations that manipulate the VM's
//! state, including stack operations and value loading.

const std = @import("std");
const testing = std.testing;

const Procedure = @import("Procedure.zig");
const Symbol = @import("Symbol.zig");
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
    /// Instruction to return from the current procedure.
    /// Restores the previous stack frame and places the return value at the correct position.
    return_value,
};

/// Executes this instruction on the given virtual machine.
/// Dispatches to the appropriate instruction handler based on the instruction type.
///
/// Supported instructions:
///   - load: Pushes a value onto the stack
///   - eval_procedure: Calls a procedure with specified number of arguments
///   - return_value: Returns from the current procedure, restoring the previous stack frame
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
        .eval_procedure => |args| return evalProcedure(vm, args.arg_count),
        .return_value => return returnValue(vm),
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

/// Returns from the current procedure call.
/// Copies the return value from the top of the stack to the position where the procedure was called,
/// resizes the stack to the previous frame's size, and restores the previous stack frame.
///
/// Args:
///   vm: Pointer to the virtual machine whose stack frame will be restored.
///
/// Errors:
///   - May return memory allocation errors if stack resizing fails.
///   - May return StackUnderflow if there are no stack frames to restore.
pub fn returnValue(vm: *Vm) !void {
    const new_stack_len = vm.current_stack_frame.stack_start;
    const dst_idx = new_stack_len - 1;
    const src_idx = vm.stack.items.len - 1;
    vm.stack.items[dst_idx] = vm.stack.items[src_idx];
    try vm.stack.resize(vm.allocator, new_stack_len);
    vm.current_stack_frame = vm.stack_frames.pop() orelse return error.StackUnderflow;
}

/// Evaluates a procedure with the specified number of arguments.
/// Creates a new stack frame and executes the procedure implementation.
/// For native procedures, executes immediately and calls ret() to restore the frame.
/// For bytecode procedures, sets up the instruction pointer for execution.
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
pub fn evalProcedure(vm: *Vm, arg_count: usize) !void {
    try vm.stack_frames.append(vm.allocator, vm.current_stack_frame);
    vm.current_stack_frame = Vm.StackFrame{
        .stack_start = vm.stack.items.len - arg_count,
    };
    const proc_idx = vm.current_stack_frame.stack_start - 1;
    const proc = try vm.fromVal(Procedure, vm.stack.items[proc_idx]);
    switch (proc.implementation) {
        .native => |native| {
            const return_val = native.func(Procedure.Context{ .vm = vm });
            try vm.stack.append(vm.allocator, return_val);
            try returnValue(vm);
        },
        .bytecode => |bytecode| {
            vm.current_stack_frame.instructions = bytecode.instructions;
        },
    }
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

    const proc = Procedure{
        .implementation = .{ .native = .{ .func = struct {
            fn addTwo(ctx: Procedure.Context) Val {
                const args = ctx.localStack();
                const val1 = ctx.vm.fromVal(i64, args[0]) catch unreachable;
                const val2 = ctx.vm.fromVal(i64, args[1]) catch unreachable;
                return Val.init(val1 + val2);
            }
        }.addTwo } },
    };
    try loadMany(&vm, &.{
        try vm.toVal(proc),
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

test "execute bytecode procedure loads instructions into stack frame" {
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc = try vm.toVal(Procedure{
        .name = try vm.interner.internStatic(Symbol.init("test-bytecode")),
        .implementation = .{
            .bytecode = .{
                .instructions = &[_]Instruction{
                    Instruction{ .repr = .{ .load = Val.init(5) } },
                    Instruction{ .repr = .{ .load = Val.init(7) } },
                },
            },
        },
    });
    try load(&vm, proc);

    try execute(Instruction{ .repr = .{ .eval_procedure = .{ .arg_count = 0 } } }, &vm);
    try testing.expectEqualDeep(
        Vm.StackFrame{
            .stack_start = 1,
            .instructions = &[_]Instruction{
                Instruction{ .repr = .{ .load = Val.init(5) } },
                Instruction{ .repr = .{ .load = Val.init(7) } },
            },
        },
        vm.current_stack_frame,
    );
}

test "execute bytecode procedure with arguments sets correct stack start" {
    const proc = Procedure{
        .implementation = .{
            .bytecode = .{
                .instructions = &[_]Instruction{
                    Instruction{ .repr = .{ .load = Val.init(42) } },
                },
            },
        },
    };
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try loadMany(&vm, &.{
        try vm.toVal(proc),
        Val.init(10),
        Val.init(20),
        Val.init(30),
    });

    try execute(Instruction{ .repr = .{ .eval_procedure = .{ .arg_count = 3 } } }, &vm);
    try testing.expectEqualDeep(
        Vm.StackFrame{
            .stack_start = 1,
            .instructions = proc.implementation.bytecode.instructions,
        },
        vm.current_stack_frame,
    );
}

test "return_value restores previous stack frame and places return value on top" {
    const proc = Procedure{
        .implementation = .{
            .bytecode = .{
                .instructions = &[_]Instruction{
                    Instruction{ .repr = .{ .load = Val.init(42) } },
                },
            },
        },
    };
    var vm = Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try loadMany(&vm, &.{
        Val.init(100),
        Val.init(200),
        try vm.toVal(proc),
        Val.init(10),
        Val.init(20),
    });
    try evalProcedure(&vm, 2);
    try load(&vm, Val.init(999));
    try testing.expectEqual(1, vm.stack_frames.items.len);
    try testing.expectEqualDeep(
        Vm.StackFrame{ .stack_start = 3, .instructions = proc.implementation.bytecode.instructions },
        vm.current_stack_frame,
    );

    try execute(Instruction{ .repr = .return_value }, &vm);
    try testing.expectEqual(0, vm.stack_frames.items.len);
    try testing.expectEqualDeep(Vm.StackFrame{}, vm.current_stack_frame);
    try testing.expectFmt(
        "(100 200 999)",
        "{f}",
        .{vm.inspector().prettySlice(vm.stack.items)},
    );
}
