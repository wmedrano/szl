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

/// Creates a new load instruction with the given value.
pub fn initLoad(val: Val) Instruction {
    return Instruction{ .repr = .{ .load = val } };
}

/// Creates a new eval_procedure instruction with the given argument count.
pub fn initEvalProcedure(arg_count: usize) Instruction {
    return Instruction{ .repr = .{ .eval_procedure = arg_count } };
}

/// Creates a new get_global instruction with the given symbol.
pub fn initGetGlobal(symbol: Symbol.Interned) Instruction {
    return Instruction{ .repr = .{ .get_global = symbol } };
}

/// Creates a new get_local instruction with the given local variable index.
/// The index is relative to the current stack frame's starting position.
pub fn initGetLocal(index: usize) Instruction {
    return Instruction{ .repr = .{ .get_local = index } };
}

/// Creates a new return_value instruction.
pub fn initReturnValue() Instruction {
    return Instruction{ .repr = .return_value };
}

/// Creates a new jump instruction with the given offset.
pub fn initJump(offset: isize) Instruction {
    return Instruction{ .repr = .{ .jump = offset } };
}

/// Creates a new jump_if instruction with the given offset.
pub fn initJumpIf(offset: isize) Instruction {
    return Instruction{ .repr = .{ .jump_if = offset } };
}

/// Tagged union representing all possible instruction types in the virtual machine.
/// Each variant corresponds to a different operation that can be performed
/// during program execution.
pub const Repr = union(enum) {
    /// Instruction to load a value onto the stack.
    /// The associated value will be pushed to the top of the VM's stack.
    load: Val,
    /// Instruction to retrieve a global variable value and load it onto the stack.
    /// The associated interned symbol identifies which global variable to retrieve.
    get_global: Symbol.Interned,
    /// Instruction to retrieve a local variable value and load it onto the stack.
    /// The associated index specifies which local variable (relative to stack frame start) to retrieve.
    get_local: usize,
    /// Instruction to evaluate a procedure with arguments from the stack.
    /// The arg_count specifies how many arguments to pass to the procedure.
    eval_procedure: usize,
    /// Instruction to return from the current procedure.
    /// Restores the previous stack frame and places the return value at the correct position.
    return_value,
    /// Instruction to raise an error by popping the top value from the stack.
    /// Sets the popped value as the VM's error state to signal a runtime error.
    raise_error,
    /// Instruction to jump by a specified offset in the instruction sequence.
    /// The offset is added to the current instruction index to change execution flow.
    jump: isize,
    /// Instruction to conditionally jump by a specified offset in the instruction sequence.
    /// Pops a value from the stack and jumps only if the value is truthy.
    jump_if: isize,
};

/// Executes this instruction on the given virtual machine.
/// Dispatches to the appropriate instruction handler based on the instruction type.
///
/// Supported instructions:
///   - load: Pushes a value onto the stack
///   - get_global: Retrieves a global variable and pushes its value onto the stack
///   - get_local: Retrieves a local variable and pushes its value onto the stack
///   - eval_procedure: Calls a procedure with specified number of arguments
///   - return_value: Returns from the current procedure, restoring the previous stack frame
///   - raise_error: Pops a value from the stack and sets it as the VM's error state
///   - jump: Jumps by a specified offset in the instruction sequence
///   - jump_if: Conditionally jumps by a specified offset if the popped stack value is truthy
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
        .get_global => |symbol| return getGlobal(vm, symbol),
        .get_local => |idx| return getLocal(vm, idx),
        .eval_procedure => |arg_count| return evalProcedure(vm, arg_count),
        .return_value => return returnValue(vm),
        .raise_error => return raiseError(vm),
        .jump => |offset| return jump(vm, offset),
        .jump_if => |offset| return jumpIf(vm, offset),
    }
}

/// Executes the next instruction in the current stack frame.
///
/// Advances the instruction pointer and executes the instruction. If no more
/// instructions are available, automatically returns from the current frame.
///
/// Args:
///   - vm: Pointer to the virtual machine instance.
///
/// Returns:
///   - May return execution errors from the underlying instruction.
pub fn executeNext(vm: *Vm) !void {
    const instruction_idx = vm.current_stack_frame.instruction_idx;
    const instruction = if (instruction_idx < vm.current_stack_frame.instructions.len) blk: {
        vm.current_stack_frame.instruction_idx += 1;
        break :blk vm.current_stack_frame.instructions[instruction_idx];
    } else Instruction.initReturnValue();
    try instruction.execute(vm);
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

/// Retrieves a global variable value and loads it onto the stack.
/// Looks up the global variable associated with the given interned symbol
/// and pushes its value onto the VM's execution stack.
///
/// Args:
///   vm: Pointer to the virtual machine whose stack will be modified.
///   symbol: The interned symbol identifying the global variable to retrieve.
///
/// Errors:
///   - May return memory allocation errors if the stack cannot be expanded.
///   - Currently panics if the global variable is not found (error handling not yet implemented).
pub fn getGlobal(vm: *Vm, symbol: Symbol.Interned) !void {
    const val = vm.inspector().get(symbol) orelse @panic("value not found, errors not supported");
    try load(vm, val);
}

/// Retrieves a local variable value and loads it onto the stack.
/// Calculates the absolute stack position by adding the local index to the current
/// stack frame's starting position, then loads the value at that position.
///
/// Args:
///   vm: Pointer to the virtual machine whose stack will be modified.
///   idx: The local variable index relative to the current stack frame's start.
///
/// Errors:
///   - May return memory allocation errors if the stack cannot be expanded.
pub fn getLocal(vm: *Vm, idx: usize) !void {
    const absolute_idx = vm.current_stack_frame.stack_start + idx;
    try load(vm, vm.stack.items[absolute_idx]);
}

/// Returns from the current procedure call, restoring the previous execution context.
/// Places the return value in the correct position for the calling procedure.
///
/// Args:
///   vm: Pointer to the virtual machine whose stack frame will be restored.
///
/// Errors:
///   - May return memory allocation errors if stack resizing fails.
///   - May return StackUnderflow if there are no stack frames to restore.
pub fn returnValue(vm: *Vm) !void {
    if (vm.err) |_| {
        return error.SzlError;
    }
    const new_stack_len = vm.current_stack_frame.stack_start;
    const dst_idx = new_stack_len - 1;
    const src_idx = vm.stack.items.len - 1;
    vm.stack.items[dst_idx] = vm.stack.items[src_idx];
    try vm.stack.resize(vm.allocator, new_stack_len);
    vm.current_stack_frame = vm.stack_frames.pop() orelse return error.StackUnderflow;
}

/// Evaluates a procedure with the specified number of arguments.
/// Handles both native and bytecode procedures, managing the execution context appropriately.
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

/// Raises an error by setting the given value as the VM's error state.  This is
/// a convenience function for directly raising an error with a specific value.
///
/// Args:
///   vm: Pointer to the virtual machine whose error state will be set.
///   err: The error value to set as the VM's error state.
pub fn raiseWithError(vm: *Vm, err: Val) void {
    vm.err = err;
}

/// Raises an error by popping the top value from the stack and setting it as the VM's error state.
/// This instruction is used to signal runtime errors during program execution.
///
/// Args:
///   vm: Pointer to the virtual machine whose error state will be set.
///
/// Errors:
///   - May return StackUnderflow if the stack is empty when trying to pop the error value.
pub fn raiseError(vm: *Vm) !void {
    const err = vm.stack.pop() orelse return error.StackUnderflow;
    vm.err = err;
}

/// Jumps by the specified offset in the instruction sequence.
/// Adds the offset to the current instruction index to change execution flow.
///
/// Args:
///   vm: Pointer to the virtual machine whose instruction pointer will be modified.
///   offset: The offset to add to the current instruction index.
pub fn jump(vm: *Vm, offset: isize) void {
    const current_idx: isize = @intCast(vm.current_stack_frame.instruction_idx);
    const new_idx = current_idx + offset;
    vm.current_stack_frame.instruction_idx = @intCast(new_idx);
}

/// Conditionally jumps by the specified offset in the instruction sequence.
/// Pops a value from the stack and jumps only if the value is truthy.
///
/// Args:
///   vm: Pointer to the virtual machine whose instruction pointer will be modified.
///   offset: The offset to add to the current instruction index if jumping.
///
/// Errors:
///   - May return StackUnderflow if the stack is empty when trying to pop the condition value.
pub fn jumpIf(vm: *Vm, offset: isize) !void {
    const condition = vm.stack.pop() orelse return error.StackUnderflow;
    if (condition.isTruthy()) {
        jump(vm, offset);
    }
}

test "execute load instruction pushes value onto stack" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try execute(Instruction.initLoad(Val.init(10)), &vm);
    try execute(Instruction.initLoad(Val.init(20)), &vm);
    try execute(Instruction.initLoad(Val.init(30)), &vm);

    try testing.expectFmt(
        "(10 20 30)",
        "{f}",
        .{vm.pretty(vm.stack.items)},
    );
}

test "execute eval_procedure instruction calls procedure with arguments" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc = Procedure{
        .implementation = Procedure.initNative(struct {
            fn addTwo(ctx: Procedure.Context) Val {
                const args = ctx.localStack();
                const val1 = ctx.vm.fromVal(i64, args[0]) catch unreachable;
                const val2 = ctx.vm.fromVal(i64, args[1]) catch unreachable;
                return Val.init(val1 + val2);
            }
        }.addTwo),
    };
    try loadMany(&vm, &.{
        try vm.toVal(proc),
        Val.init(10),
        Val.init(20),
    });

    try execute(Instruction.initEvalProcedure(2), &vm);
    try testing.expectFmt(
        "(30)",
        "{f}",
        .{vm.pretty(vm.stack.items)},
    );
}

test "execute bytecode procedure loads instructions into stack frame" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc = try vm.toVal(Procedure{
        .name = try vm.interner.internStatic(Symbol.init("test-bytecode")),
        .implementation = Procedure.initStaticBytecode(&[_]Instruction{
            Instruction.initLoad(Val.init(5)),
            Instruction.initLoad(Val.init(7)),
        }),
    });
    try load(&vm, proc);

    // Call procedure with 0 arguments - should set up new stack frame with the bytecode instructions
    try execute(Instruction.initEvalProcedure(0), &vm);
    try testing.expectEqualDeep(
        Vm.StackFrame{
            .stack_start = 1,
            .instructions = &[_]Instruction{
                Instruction.initLoad(Val.init(5)),
                Instruction.initLoad(Val.init(7)),
            },
        },
        vm.current_stack_frame,
    );
}

test "execute bytecode procedure with arguments sets correct stack start" {
    const proc = Procedure{
        .implementation = Procedure.initStaticBytecode(&[_]Instruction{
            Instruction.initLoad(Val.init(42)),
        }),
    };
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try loadMany(&vm, &.{
        try vm.toVal(proc),
        Val.init(10),
        Val.init(20),
        Val.init(30),
    });

    try execute(Instruction.initEvalProcedure(3), &vm);
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
        .implementation = Procedure.initStaticBytecode(&[_]Instruction{
            Instruction.initLoad(Val.init(42)),
        }),
    };
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Stack layout: [100, 200, proc, 10, 20]
    try loadMany(&vm, &.{
        Val.init(100),
        Val.init(200),
        try vm.toVal(proc),
        Val.init(10),
        Val.init(20),
    });
    try evalProcedure(&vm, 2);
    try load(&vm, Val.init(999)); // This becomes the return value
    try testing.expectEqual(1, vm.stack_frames.items.len);
    try testing.expectEqualDeep(
        Vm.StackFrame{ .stack_start = 3, .instructions = proc.implementation.bytecode.instructions },
        vm.current_stack_frame,
    );

    // Return should place 999 where the procedure was (position 2) and resize stack to [100, 200, 999]
    try execute(Instruction.initReturnValue(), &vm);
    try testing.expectEqual(0, vm.stack_frames.items.len);
    try testing.expectEqualDeep(Vm.StackFrame{}, vm.current_stack_frame);
    try testing.expectFmt(
        "(100 200 999)",
        "{f}",
        .{vm.pretty(vm.stack.items)},
    );
}

test "executeNext executes instruction and advances pointer" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    vm.current_stack_frame.instructions = &[_]Instruction{
        Instruction.initLoad(Val.init(42)),
        Instruction.initLoad(Val.init(99)),
    };

    try executeNext(&vm);
    try testing.expectEqual(1, vm.current_stack_frame.instruction_idx);
    try testing.expectFmt(
        "(42)",
        "{f}",
        .{vm.pretty(vm.stack.items)},
    );
}

test "executeNext automatically returns when no more instructions" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up a stack frame that we can return from
    try vm.stack_frames.append(vm.allocator, Vm.StackFrame{});
    vm.current_stack_frame = Vm.StackFrame{
        .stack_start = 1,
        .instructions = &[_]Instruction{},
        .instruction_idx = 0,
    };
    try vm.stack.append(vm.allocator, Val.init(123)); // Return value

    // Should automatically execute return_value instruction when no
    // instructions left.
    try executeNext(&vm);
    try testing.expectEqual(0, vm.stack_frames.items.len);
    try testing.expectFmt(
        "(123)",
        "{f}",
        .{vm.pretty(vm.stack.items)},
    );
}

test "executeNext executes multiple instructions in sequence" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    vm.current_stack_frame.instructions = &[_]Instruction{
        Instruction.initLoad(Val.init(1)),
        Instruction.initLoad(Val.init(2)),
        Instruction.initLoad(Val.init(3)),
    };

    try executeNext(&vm);
    try executeNext(&vm);
    try executeNext(&vm);

    try testing.expectEqual(3, vm.current_stack_frame.instruction_idx);
    try testing.expectEqual(3, vm.stack.items.len);
    try testing.expectFmt("(1 2 3)", "{f}", .{vm.pretty(vm.stack.items)});
}

test "executeNext handles instruction pointer at boundary correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up initial frame to return to.
    try vm.stack_frames.append(vm.allocator, Vm.StackFrame{});
    vm.current_stack_frame = Vm.StackFrame{
        .stack_start = 1,
        .instructions = &[_]Instruction{
            Instruction.initLoad(Val.init(100)),
        },
        .instruction_idx = 0,
    };
    try vm.stack.append(vm.allocator, Val.init(999)); // Return value

    // Execute the one instruction.
    try executeNext(&vm);
    try testing.expectEqual(1, vm.current_stack_frame.instruction_idx);

    // Next call should trigger automatic return since instruction_idx == instructions.len.
    try executeNext(&vm);
    try testing.expectEqual(0, vm.stack_frames.items.len);
    try testing.expectFmt(
        "(100)",
        "{f}",
        .{vm.pretty(vm.stack.items)},
    );
}

test "execute get_global instruction loads global value onto stack" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Define a global variable using Builder
    const symbol = Symbol.init("test-global");
    const value = Val.init(42);
    try vm.builder().define(symbol, value);

    // Get the interned symbol for the instruction
    const interned_symbol = try vm.interner.intern(symbol);

    // Execute get_global instruction
    try execute(Instruction.initGetGlobal(interned_symbol), &vm);

    // Verify the value was loaded onto the stack
    try testing.expectFmt(
        "(42)",
        "{f}",
        .{vm.pretty(vm.stack.items)},
    );
}

test "execute get_global instruction with multiple values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Define multiple global variables using Builder
    try vm.builder().define(Symbol.init("var1"), Val.init(10));
    try vm.builder().define(Symbol.init("var2"), Val.init(20));
    try vm.builder().define(Symbol.init("var3"), Val.init(30));

    // Get interned symbols
    const symbol1 = try vm.interner.intern(Symbol.init("var1"));
    const symbol2 = try vm.interner.intern(Symbol.init("var2"));
    const symbol3 = try vm.interner.intern(Symbol.init("var3"));

    // Execute get_global instructions
    try execute(Instruction.initGetGlobal(symbol1), &vm);
    try execute(Instruction.initGetGlobal(symbol2), &vm);
    try execute(Instruction.initGetGlobal(symbol3), &vm);

    // Verify all values were loaded onto the stack in order
    try testing.expectFmt(
        "(10 20 30)",
        "{f}",
        .{vm.pretty(vm.stack.items)},
    );
}

test "get_global instruction works with different value types" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Define global variables of different types using Builder
    try vm.builder().define(Symbol.init("bool-var"), Val.init(true));
    try vm.builder().define(Symbol.init("symbol-var"), try vm.builder().internVal(Symbol.init("test-symbol")));

    const bool_symbol = try vm.interner.intern(Symbol.init("bool-var"));
    const symbol_symbol = try vm.interner.intern(Symbol.init("symbol-var"));

    // Execute get_global instructions
    try execute(Instruction.initGetGlobal(bool_symbol), &vm);
    try execute(Instruction.initGetGlobal(symbol_symbol), &vm);

    // Verify the values were loaded correctly
    try testing.expectFmt(
        "(#t test-symbol)",
        "{f}",
        .{vm.pretty(vm.stack.items)},
    );
}

test "execute get_local instruction loads local value onto stack" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up a stack frame with local variables
    vm.current_stack_frame.stack_start = 2;
    try loadMany(&vm, &.{
        Val.init(100), // position 0 - before frame
        Val.init(200), // position 1 - before frame
        Val.init(42),  // position 2 - local variable 0
        Val.init(99),  // position 3 - local variable 1
    });

    // Execute get_local instruction for local variable 0
    try execute(Instruction.initGetLocal(0), &vm);

    // Verify the local value was loaded onto the stack
    try testing.expectFmt(
        "(100 200 42 99 42)",
        "{f}",
        .{vm.pretty(vm.stack.items)},
    );
}

test "execute get_local instruction with multiple local variables" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up a stack frame with multiple local variables
    vm.current_stack_frame.stack_start = 1;
    try loadMany(&vm, &.{
        Val.init(999), // position 0 - before frame
        Val.init(10),  // position 1 - local variable 0
        Val.init(20),  // position 2 - local variable 1
        Val.init(30),  // position 3 - local variable 2
    });

    // Execute get_local instructions for all local variables
    try execute(Instruction.initGetLocal(0), &vm);
    try execute(Instruction.initGetLocal(1), &vm);
    try execute(Instruction.initGetLocal(2), &vm);

    // Verify all local values were loaded onto the stack in order
    try testing.expectFmt(
        "(999 10 20 30 10 20 30)",
        "{f}",
        .{vm.pretty(vm.stack.items)},
    );
}

test "execute get_local instruction with zero stack frame start" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up a stack frame that starts at position 0
    vm.current_stack_frame.stack_start = 0;
    try loadMany(&vm, &.{
        Val.init(42),  // position 0 - local variable 0
        Val.init(99),  // position 1 - local variable 1
    });

    // Execute get_local instruction for local variable 1
    try execute(Instruction.initGetLocal(1), &vm);

    // Verify the local value was loaded onto the stack
    try testing.expectFmt(
        "(42 99 99)",
        "{f}",
        .{vm.pretty(vm.stack.items)},
    );
}

test "get_local instruction works with different value types" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up a stack frame with local variables of different types
    vm.current_stack_frame.stack_start = 0;
    try loadMany(&vm, &.{
        Val.init(true),  // position 0 - boolean local variable
        try vm.builder().internVal(Symbol.init("local-symbol")), // position 1 - symbol local variable
        Val.init(-123),  // position 2 - negative integer local variable
    });

    // Execute get_local instructions for different types
    try execute(Instruction.initGetLocal(0), &vm);
    try execute(Instruction.initGetLocal(1), &vm);
    try execute(Instruction.initGetLocal(2), &vm);

    // Verify the values were loaded correctly
    try testing.expectFmt(
        "(#t local-symbol -123 #t local-symbol -123)",
        "{f}",
        .{vm.pretty(vm.stack.items)},
    );
}

test "execute jump instruction modifies instruction pointer" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set initial instruction index
    vm.current_stack_frame.instruction_idx = 5;

    // Execute jump with positive offset
    try execute(Instruction.initJump(3), &vm);
    try testing.expectEqual(8, vm.current_stack_frame.instruction_idx);

    // Execute jump with negative offset
    try execute(Instruction.initJump(-2), &vm);
    try testing.expectEqual(6, vm.current_stack_frame.instruction_idx);

    // Execute jump with zero offset
    try execute(Instruction.initJump(0), &vm);
    try testing.expectEqual(6, vm.current_stack_frame.instruction_idx);
}

test "execute jump_if instruction with truthy value jumps" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set initial instruction index
    vm.current_stack_frame.instruction_idx = 5;

    // Push truthy value (true)
    try load(&vm, Val.init(true));
    try execute(Instruction.initJumpIf(3), &vm);
    try testing.expectEqual(8, vm.current_stack_frame.instruction_idx);

    // Push truthy value (non-zero integer)
    try load(&vm, Val.init(42));
    try execute(Instruction.initJumpIf(-2), &vm);
    try testing.expectEqual(6, vm.current_stack_frame.instruction_idx);

    // Push truthy value (symbol)
    try load(&vm, try vm.builder().internVal(Symbol.init("test")));
    try execute(Instruction.initJumpIf(1), &vm);
    try testing.expectEqual(7, vm.current_stack_frame.instruction_idx);
}

test "execute jump_if instruction with falsy value does not jump" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set initial instruction index
    vm.current_stack_frame.instruction_idx = 5;

    // Push falsy value (false)
    try load(&vm, Val.init(false));
    try execute(Instruction.initJumpIf(10), &vm);
    try testing.expectEqual(5, vm.current_stack_frame.instruction_idx);

    // Verify stack is empty after popping condition
    try testing.expectEqual(0, vm.stack.items.len);
}

test "execute jump_if instruction with empty stack returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Attempt to execute jump_if with empty stack
    try testing.expectError(error.StackUnderflow, execute(Instruction.initJumpIf(5), &vm));
}
