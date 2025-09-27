const std = @import("std");
const testing = std.testing;

const Context = @import("Context.zig");
const Handle = @import("object_pool.zig").Handle;
const NativeProc = @import("NativeProc.zig");
const Proc = @import("Proc.zig");
const Continuation = @import("types/Continuation.zig");
const Symbol = @import("types/Symbol.zig");
const Val = @import("types/Val.zig");
const Vm = @import("Vm.zig");

/// Operators for optimized VM operations.
/// These represent specialized operations that can be executed more efficiently
/// than through normal bytecode compilation.
pub const Operator = enum {
    /// Call-with-current-continuation operator.
    /// Captures the current continuation and passes it to a procedure.
    call_with_cc,
};

/// Instruction representation and execution for the Scheme virtual machine.
///
/// This module defines the instruction set that can be executed by the virtual
/// machine. Instructions represent atomic operations that manipulate the VM's
/// state, including stack operations and value loading.
pub const Instruction = union(enum) {
    /// Parameters for conditional jumpts.
    pub const ConditionalJumpParams = struct {
        steps: isize,
        pop: bool = true,
    };

    /// Instruction to load a value onto the stack.
    /// The associated value will be pushed to the top of the VM's stack.
    load: Val,
    /// Instruction to retrieve a global variable value and load it onto the stack.
    /// The associated interned symbol identifies which global variable to retrieve.
    get_global: Symbol.Interned,
    /// Instruction to retrieve a local variable value and load it onto the stack.
    /// The associated index specifies which local variable (relative to stack frame start) to retrieve.
    get_local: isize,
    /// Instruction to set a local variable value from the top of the stack.
    /// The associated index specifies which local variable (relative to stack frame start) to set.
    set_local: isize,
    /// Instruction to evaluate a procedure with arguments from the stack.
    /// The arg_count specifies how many arguments to pass to the procedure.
    eval_proc: usize,
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
    /// Pops a value from the stack and jumps only if the value is falsy.
    jump_if_not: ConditionalJumpParams,
    /// Instruction to squash the top n values on the stack, keeping only the topmost value.
    /// Takes n values from the stack and leaves only the top one.
    squash: usize,
    /// Instruction to capture the current continuation.
    /// Creates a continuation object representing the current execution state.
    call_operator: Operator,

    /// Executes the instruction on the given virtual machine.
    /// Dispatches to the appropriate instruction implementation based on the instruction type.
    ///
    /// Args:
    ///   self: The instruction to execute.
    ///   vm: Pointer to the virtual machine that will execute the instruction.
    ///
    /// Errors:
    ///   - May return any VM execution error depending on the instruction type.
    pub fn execute(self: Instruction, vm: *Vm) Vm.Error!void {
        switch (self) {
            .load => |val| return vm.context.stackPush(vm.allocator, val),
            .get_global => |symbol| return getGlobal(vm, symbol),
            .get_local => |idx| return getLocal(vm, idx),
            .set_local => |idx| return setLocal(vm, idx),
            .eval_proc => |arg_count| return evalProc(vm, arg_count),
            .return_value => return returnValue(vm),
            .raise_error => return raiseError(vm),
            .jump => |offset| return jump(vm, offset),
            .jump_if_not => |params| return jumpIfNot(vm, params.steps, params.pop),
            .squash => |count| return squash(vm, count),
            .call_operator => |op| return callOperator(vm, op),
        }
    }
};

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
    const instruction_idx = vm.context.current_stack_frame.instruction_idx;
    const instruction = if (instruction_idx < vm.context.current_stack_frame.instructions.len) blk: {
        vm.context.current_stack_frame.instruction_idx += 1;
        break :blk vm.context.current_stack_frame.instructions[instruction_idx];
    } else .return_value;
    try instruction.execute(vm);
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
///   - Raises an error if the global variable is not found.
pub fn getGlobal(vm: *Vm, symbol: Symbol.Interned) !void {
    const val = vm.inspector().get(symbol) orelse {
        try raiseWithError(vm, Val.init(vm.common_symbols.@"undefined-variable"));
        return;
    };
    try vm.context.stackPush(vm.allocator, val);
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
pub fn getLocal(vm: *Vm, idx: isize) !void {
    const absolute_idx = @as(isize, @intCast(vm.context.current_stack_frame.stack_start)) + idx;
    try vm.context.stackPush(vm.allocator, vm.context.stack()[@intCast(absolute_idx)]);
}

/// Sets a local variable value by popping a value from the stack.
/// Calculates the absolute stack position by adding the local index to the current
/// stack frame's starting position, then sets the value at that position.
///
/// Args:
///   vm: Pointer to the virtual machine whose stack will be modified.
///   idx: The local variable index relative to the current stack frame's start.
///
/// Errors:
///   - May return StackUnderflow if the stack is empty when trying to pop the value.
pub fn setLocal(vm: *Vm, idx: isize) !void {
    const val = vm.context.stackPop() orelse return error.StackUnderflow;
    const absolute_idx = @as(isize, @intCast(vm.context.current_stack_frame.stack_start)) + idx;
    vm.context.stack()[@intCast(absolute_idx)] = val;
}

/// Returns from the current procedure call by delegating to the context's returnValue method.
/// This is the instruction-level implementation that simply calls the context method.
///
/// Args:
///   vm: Pointer to the virtual machine whose context will handle the return operation.
///
/// Returns:
///   StackUnderflow error if there are no stack frames to return to.
pub fn returnValue(vm: *Vm) !void {
    try vm.context.returnValue();
}

/// Evaluates a procedure with the specified number of arguments.
/// Handles operators, bytecode procedures, native procedures, and continuations,
/// managing the execution context appropriately for each type.
///
/// Args:
///   vm: Pointer to the virtual machine that will execute the procedure.
///   arg_count: Number of arguments to pass to the procedure.
///
/// Errors:
///   - May return memory allocation errors if stack frame operations fail.
///   - May return StackUnderflow if there are no stack frames to restore.
///   - May return UncaughtException if the procedure type is invalid.
pub fn evalProc(vm: *Vm, arg_count: usize) Vm.Error!void {
    const stack_start = vm.context.stack().len - arg_count;
    const proc_idx = stack_start - 1;
    const proc_val = vm.context.constStack()[proc_idx];
    switch (proc_val.repr) {
        .proc => |proc_handle| try evalBytecodeProc(vm, proc_handle, arg_count, stack_start),
        .native_proc => |proc| try evalNativeProc(vm, proc.*, stack_start),
        .continuation => |cont_handle| try evalContinuation(vm, cont_handle, arg_count),
        else => {
            try raiseWithError(vm, Val.init(vm.common_symbols.@"type-error"));
            return Vm.Error.UncaughtException;
        },
    }
}

/// Evaluates a bytecode procedure by setting up a new stack frame.
///
/// Args:
///   vm: Pointer to the virtual machine that will execute the procedure.
///   proc_handle: Handle to the bytecode procedure.
///   arg_count: Number of arguments to pass to the procedure.
///   stack_start: Starting position for the new stack frame.
///
/// Errors:
///   - May return memory allocation errors if stack frame operations fail.
///   - May return UncaughtException if procedure validation fails.
/// Evaluates a bytecode procedure by setting up a new stack frame.
///
/// Args:
///   vm: Pointer to the virtual machine that will execute the procedure.
///   proc_handle: Handle to the bytecode procedure.
///   arg_count: Number of arguments to pass to the procedure.
///   stack_start: Starting position for the new stack frame.
///
/// Errors:
///   - May return memory allocation errors if stack frame operations fail.
///   - May return UncaughtException if procedure validation fails.
inline fn evalBytecodeProc(vm: *Vm, proc_handle: Handle(Proc), arg_count: usize, stack_start: usize) !void {
    try vm.context.stack_frames.append(vm.allocator, vm.context.current_stack_frame);
    vm.context.current_stack_frame = Context.StackFrame{
        .stack_start = stack_start,
        .exception_handler = vm.context.current_stack_frame.next_exception_handler,
        .next_exception_handler = vm.context.current_stack_frame.next_exception_handler,
    };
    const proc = vm.inspector().resolve(Proc, proc_handle) catch return Vm.Error.UncaughtException;
    if (arg_count != proc.args) {
        try raiseWithError(vm, Val.init(vm.common_symbols.@"wrong-number-of-arguments"));
        return Vm.Error.UncaughtException;
    }
    try vm.context.stack_vals.appendNTimes(vm.allocator, Val.init({}), proc.locals());
    vm.context.current_stack_frame.instructions = proc.instructions;
}

/// Evaluates a native procedure by calling its function and returning the result.
///
/// Args:
///   vm: Pointer to the virtual machine that will execute the procedure.
///   proc: The native procedure to execute.
///   stack_start: Starting position for the new stack frame.
///
/// Errors:
///   - May return memory allocation errors if stack frame operations fail.
///   - May return StackUnderflow if there are no stack frames to restore.
/// Evaluates a native procedure by calling its function and returning the result.
///
/// Args:
///   vm: Pointer to the virtual machine that will execute the procedure.
///   proc: The native procedure to execute.
///   stack_start: Starting position for the new stack frame.
///
/// Errors:
///   - May return memory allocation errors if stack frame operations fail.
///   - May return StackUnderflow if there are no stack frames to restore.
inline fn evalNativeProc(vm: *Vm, proc: NativeProc.Native, stack_start: usize) !void {
    try vm.context.stack_frames.append(vm.allocator, vm.context.current_stack_frame);
    vm.context.current_stack_frame = Context.StackFrame{
        .stack_start = stack_start,
        .exception_handler = vm.context.current_stack_frame.next_exception_handler,
        .next_exception_handler = vm.context.current_stack_frame.next_exception_handler,
    };
    const return_val = try proc.func(NativeProc.NativeContext{ .vm = vm });
    try vm.context.stackPush(vm.allocator, return_val);
    try returnValue(vm);
}

/// Evaluates a continuation by restoring its execution context.
///
/// Args:
///   vm: Pointer to the virtual machine that will execute the continuation.
///   cont_handle: Handle to the continuation.
///   arg_count: Number of arguments to pass to the continuation.
///
/// Errors:
///   - May return memory allocation errors if context operations fail.
///   - May return UncaughtException if continuation validation fails.
/// Evaluates a continuation by restoring its execution context.
///
/// Args:
///   vm: Pointer to the virtual machine that will execute the continuation.
///   cont_handle: Handle to the continuation.
///   arg_count: Number of arguments to pass to the continuation.
///
/// Errors:
///   - May return memory allocation errors if context operations fail.
///   - May return UncaughtException if continuation validation fails.
inline fn evalContinuation(vm: *Vm, cont_handle: Handle(Continuation), arg_count: usize) !void {
    if (arg_count != 1) {
        try raiseWithError(vm, Val.init(vm.common_symbols.@"wrong-number-of-arguments"));
        return Vm.Error.UncaughtException;
    }
    const cont = vm.inspector().resolve(Continuation, cont_handle) catch return Vm.Error.UncaughtException;
    const local_stack = vm.context.constStack();
    const return_value = local_stack[local_stack.len - 1];
    try vm.context.copyFrom(vm.allocator, cont.context);
    try vm.context.stackPush(vm.allocator, return_value);
}

/// Raises an error by setting the given value as the VM's error state.
/// This is a convenience function for directly raising an error with a specific value.
///
/// Args:
///   vm: Pointer to the virtual machine whose error state will be set.
///   err: The error value to set as the VM's error state.
pub fn raiseWithError(vm: *Vm, err: Val) Vm.Error!void {
    try vm.context.stackPushMany(vm.allocator, &.{
        vm.context.current_stack_frame.exception_handler,
        err,
    });
    // TODO: This calls a new procedure which may then also raise an
    // exception. See what behavior makes sense. It's possible that an exception
    // should use a different handler.
    try evalProc(vm, 1);
}

/// Raises an error by popping the top value from the stack and setting it as the VM's error state.
/// This instruction is used to signal runtime errors during program execution.
///
/// Args:
///   vm: Pointer to the virtual machine whose error state will be set.
///
/// Errors:
///   - May return StackUnderflow if the stack is empty when trying to pop the error value.
pub fn raiseError(vm: *Vm) Vm.Error!void {
    const err = vm.context.stackPop() orelse return Vm.Error.StackUnderflow;
    try raiseWithError(vm, err);
}

/// Jumps by the specified offset in the instruction sequence.
/// Adds the offset to the current instruction index to change execution flow.
///
/// Args:
///   vm: Pointer to the virtual machine whose instruction pointer will be modified.
///   offset: The offset to add to the current instruction index.
pub fn jump(vm: *Vm, offset: isize) void {
    const current_idx: isize = @intCast(vm.context.current_stack_frame.instruction_idx);
    const new_idx = current_idx + offset;
    vm.context.current_stack_frame.instruction_idx = @intCast(new_idx);
}

/// Conditionally jumps by the specified offset in the instruction sequence.
/// Optionally pops a value from the stack and jumps only if the value is falsy.
///
/// Args:
///   vm: Pointer to the virtual machine whose instruction pointer will be modified.
///   offset: The offset to add to the current instruction index if jumping.
///   pop: Whether to pop a value from the stack for the condition check.
///
/// Errors:
///   - May return StackUnderflow if the stack is empty when trying to pop the condition value.
pub fn jumpIfNot(vm: *Vm, offset: isize, pop: bool) !void {
    if (vm.context.stack().len == 0) return error.StackUnderflow;
    const condition = vm.context.stack()[vm.context.stack().len - 1];
    if (!condition.isTruthy()) jump(vm, offset);
    if (pop) _ = vm.context.stackPop();
}

/// Squashes the top n values on the stack, keeping only the topmost value.
/// Takes the specified number of values from the stack and leaves only the top one.
///
/// Args:
///   vm: Pointer to the virtual machine whose stack will be modified.
///   count: The number of values to squash (must be >= 1).
///
/// Errors:
///   - May return StackUnderflow if there are fewer than count values on the stack.
pub fn squash(vm: *Vm, count: usize) !void {
    if (count == 0) return;
    if (vm.context.stack().len < count) return error.StackUnderflow;

    const top_value = vm.context.stack()[vm.context.stack().len - 1];
    const new_len = vm.context.stack().len - count + 1;
    vm.context.stack()[new_len - 1] = top_value;
    try vm.context.stack_vals.resize(vm.allocator, new_len);
}

/// Executes an operator instruction on the virtual machine.
/// Operators provide optimized implementations for common Scheme operations.
///
/// Args:
///   vm: Pointer to the virtual machine that will execute the operator.
///   operator: The specific operator to execute.
///
/// Errors:
///   - May return memory allocation errors if continuation creation or stack operations fail.
///   - May return procedure execution errors from the called procedure.
pub fn callOperator(vm: *Vm, operator: Operator) !void {
    switch (operator) {
        .call_with_cc => {
            // 1. Get the continuation
            const continuation = try vm.toVal(Continuation{ .context = try vm.context.clone(vm.allocator) });
            // 2. Set up the stack for calling the provided procedure.
            // Before: [proc]
            // After:  [proc continuation-proc]
            try vm.context.stackPush(vm.allocator, continuation);
            // 3. Call the procedure
            try evalProc(vm, 1);
        },
    }
}

test "Instruction size is 24 bytes" {
    try testing.expectEqual(24, @sizeOf(Instruction));
}

test "execute load instruction pushes value onto stack" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try Instruction.execute(.{ .load = Val.init(10) }, &vm);
    try Instruction.execute(.{ .load = Val.init(20) }, &vm);
    try Instruction.execute(.{ .load = Val.init(30) }, &vm);

    try testing.expectFmt(
        "(10 20 30)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
    );
}

test "execute eval_procedure instruction calls procedure with arguments" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc = NativeProc.Native{ .name = "test-proc", .func = struct {
        fn addTwo(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            const val1 = ctx.vm.fromVal(i64, args[0]) catch unreachable;
            const val2 = ctx.vm.fromVal(i64, args[1]) catch unreachable;
            return Val.init(val1 + val2);
        }
    }.addTwo };
    try vm.context.stackPushMany(vm.allocator, &.{
        try vm.toVal(&proc),
        Val.init(10),
        Val.init(20),
    });

    try Instruction.execute(.{ .eval_proc = 2 }, &vm);
    try testing.expectFmt(
        "(30)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
    );
}

test "execute bytecode procedure loads instructions into stack frame" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const instructions = &[_]Instruction{
        .{ .load = Val.init(5) },
        .{ .load = Val.init(7) },
    };
    const proc = try vm.toVal(Proc{
        .instructions = try vm.allocator.dupe(Instruction, instructions),
    });
    try vm.context.stackPush(vm.allocator, proc);

    // Call procedure with 0 arguments - should set up new stack frame with the bytecode instructions
    try Instruction.execute(.{ .eval_proc = 0 }, &vm);
    try testing.expectEqualDeep(
        Context.StackFrame{
            .stack_start = 1,
            .instructions = &[_]Instruction{
                .{ .load = Val.init(5) },
                .{ .load = Val.init(7) },
            },
        },
        vm.context.current_stack_frame,
    );
}

test "execute bytecode procedure with arguments sets correct stack start" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const instructions = &[_]Instruction{
        .{ .load = Val.init(42) },
    };
    const proc = Proc{
        .args = 3,
        .locals_count = 3,
        .instructions = try vm.allocator.dupe(Instruction, instructions),
    };
    try vm.context.stackPushMany(vm.allocator, &.{
        try vm.toVal(proc),
        Val.init(10),
        Val.init(20),
        Val.init(30),
    });

    try Instruction.execute(.{ .eval_proc = 3 }, &vm);
    try testing.expectEqualDeep(
        Context.StackFrame{
            .stack_start = 1,
            .instructions = proc.instructions,
        },
        vm.context.current_stack_frame,
    );
}

test "return_value restores previous stack frame and places return value on top" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const instructions = &[_]Instruction{
        .{ .load = Val.init(42) },
    };
    const proc = Proc{
        .args = 2,
        .locals_count = 2,
        .instructions = try vm.allocator.dupe(Instruction, instructions),
    };

    // Stack layout: [100, 200, proc, 10, 20]
    try vm.context.stackPushMany(vm.allocator, &.{
        Val.init(100),
        Val.init(200),
        try vm.toVal(proc),
        Val.init(10),
        Val.init(20),
    });
    try evalProc(&vm, 2);
    try vm.context.stackPush(vm.allocator, Val.init(999)); // This becomes the return value
    try testing.expectEqual(1, vm.context.stack_frames.items.len);
    try testing.expectEqualDeep(
        Context.StackFrame{ .stack_start = 3, .instructions = proc.instructions },
        vm.context.current_stack_frame,
    );

    // Return should place 999 where the procedure was (position 2) and resize stack to [100, 200, 999]
    try Instruction.execute(.return_value, &vm);
    try testing.expectEqual(0, vm.context.stack_frames.items.len);
    try testing.expectEqualDeep(Context.StackFrame{}, vm.context.current_stack_frame);
    try testing.expectFmt(
        "(100 200 999)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
    );
}

test "executeNext executes instruction and advances pointer" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    vm.context.current_stack_frame.instructions = &[_]Instruction{
        .{ .load = Val.init(42) },
        .{ .load = Val.init(99) },
    };

    try executeNext(&vm);
    try testing.expectEqual(1, vm.context.current_stack_frame.instruction_idx);
    try testing.expectFmt(
        "(42)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
    );
}

test "executeNext automatically returns when no more instructions" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up a stack frame that we can return from
    try vm.context.stack_frames.append(vm.allocator, Context.StackFrame{});
    vm.context.current_stack_frame = Context.StackFrame{
        .stack_start = 1,
        .instructions = &[_]Instruction{},
        .instruction_idx = 0,
    };
    try vm.context.stackPush(vm.allocator, Val.init(123)); // Return value

    // Should automatically execute return_value instruction when no
    // instructions left.
    try executeNext(&vm);
    try testing.expectEqual(0, vm.context.stack_frames.items.len);
    try testing.expectFmt(
        "(123)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
    );
}

test "executeNext executes multiple instructions in sequence" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    vm.context.current_stack_frame.instructions = &[_]Instruction{
        .{ .load = Val.init(1) },
        .{ .load = Val.init(2) },
        .{ .load = Val.init(3) },
    };

    try executeNext(&vm);
    try executeNext(&vm);
    try executeNext(&vm);

    try testing.expectEqual(3, vm.context.current_stack_frame.instruction_idx);
    try testing.expectEqual(3, vm.context.stack().len);
    try testing.expectFmt("(1 2 3)", "{f}", .{vm.pretty(vm.context.stack())});
}

test "executeNext handles instruction pointer at boundary correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up initial frame to return to.
    try vm.context.stack_frames.append(vm.allocator, Context.StackFrame{});
    vm.context.current_stack_frame = Context.StackFrame{
        .stack_start = 1,
        .instructions = &[_]Instruction{
            .{ .load = Val.init(100) },
        },
        .instruction_idx = 0,
    };
    try vm.context.stackPush(vm.allocator, Val.init(999)); // Return value

    // Execute the one instruction.
    try executeNext(&vm);
    try testing.expectEqual(1, vm.context.current_stack_frame.instruction_idx);

    // Next call should trigger automatic return since instruction_idx == instructions.len.
    try executeNext(&vm);
    try testing.expectEqual(0, vm.context.stack_frames.items.len);
    try testing.expectFmt(
        "(100)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
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
    try Instruction.execute(.{ .get_global = interned_symbol }, &vm);

    // Verify the value was loaded onto the stack
    try testing.expectFmt(
        "(42)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
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
    try Instruction.execute(.{ .get_global = symbol1 }, &vm);
    try Instruction.execute(.{ .get_global = symbol2 }, &vm);
    try Instruction.execute(.{ .get_global = symbol3 }, &vm);

    // Verify all values were loaded onto the stack in order
    try testing.expectFmt(
        "(10 20 30)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
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
    try Instruction.execute(.{ .get_global = bool_symbol }, &vm);
    try Instruction.execute(.{ .get_global = symbol_symbol }, &vm);

    // Verify the values were loaded correctly
    try testing.expectFmt(
        "(#t test-symbol)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
    );
}

test "execute get_local instruction loads local value onto stack" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up a stack frame with local variables
    vm.context.current_stack_frame.stack_start = 2;
    try vm.context.stackPushMany(vm.allocator, &.{
        Val.init(100), // position 0 - before frame
        Val.init(200), // position 1 - before frame
        Val.init(42), // position 2 - local variable 0
        Val.init(99), // position 3 - local variable 1
    });

    // Execute get_local instruction for local variable 0
    try Instruction.execute(.{ .get_local = 0 }, &vm);

    // Verify the local value was loaded onto the stack
    try testing.expectFmt(
        "(100 200 42 99 42)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
    );
}

test "execute get_local instruction with multiple local variables" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up a stack frame with multiple local variables
    vm.context.current_stack_frame.stack_start = 1;
    try vm.context.stackPushMany(vm.allocator, &.{
        Val.init(999), // position 0 - before frame
        Val.init(10), // position 1 - local variable 0
        Val.init(20), // position 2 - local variable 1
        Val.init(30), // position 3 - local variable 2
    });

    // Execute get_local instructions for all local variables
    try Instruction.execute(.{ .get_local = 0 }, &vm);
    try Instruction.execute(.{ .get_local = 1 }, &vm);
    try Instruction.execute(.{ .get_local = 2 }, &vm);

    // Verify all local values were loaded onto the stack in order
    try testing.expectFmt(
        "(999 10 20 30 10 20 30)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
    );
}

test "execute get_local instruction with zero stack frame start" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up a stack frame that starts at position 0
    vm.context.current_stack_frame.stack_start = 0;
    try vm.context.stackPushMany(vm.allocator, &.{
        Val.init(42), // position 0 - local variable 0
        Val.init(99), // position 1 - local variable 1
    });

    // Execute get_local instruction for local variable 1
    try Instruction.execute(.{ .get_local = 1 }, &vm);

    // Verify the local value was loaded onto the stack
    try testing.expectFmt(
        "(42 99 99)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
    );
}

test "get_local instruction works with different value types" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up a stack frame with local variables of different types
    vm.context.current_stack_frame.stack_start = 0;
    try vm.context.stackPushMany(vm.allocator, &.{
        Val.init(true), // position 0 - boolean local variable
        try vm.builder().internVal(Symbol.init("local-symbol")), // position 1 - symbol local variable
        Val.init(-123), // position 2 - negative integer local variable
    });

    // Execute get_local instructions for different types
    try Instruction.execute(.{ .get_local = 0 }, &vm);
    try Instruction.execute(.{ .get_local = 1 }, &vm);
    try Instruction.execute(.{ .get_local = 2 }, &vm);

    // Verify the values were loaded correctly
    try testing.expectFmt(
        "(#t local-symbol -123 #t local-symbol -123)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
    );
}

test "execute jump instruction modifies instruction pointer" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set initial instruction index
    vm.context.current_stack_frame.instruction_idx = 5;

    // Execute jump with positive offset
    try Instruction.execute(.{ .jump = 3 }, &vm);
    try testing.expectEqual(8, vm.context.current_stack_frame.instruction_idx);

    // Execute jump with negative offset
    try Instruction.execute(.{ .jump = -2 }, &vm);
    try testing.expectEqual(6, vm.context.current_stack_frame.instruction_idx);

    // Execute jump with zero offset
    try Instruction.execute(.{ .jump = 0 }, &vm);
    try testing.expectEqual(6, vm.context.current_stack_frame.instruction_idx);
}

test "execute jump_if_not instruction with falsy value jumps" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set initial instruction index
    vm.context.current_stack_frame.instruction_idx = 5;

    // Push falsy value (false)
    try vm.context.stackPush(vm.allocator, Val.init(false));
    try Instruction.execute(.{ .jump_if_not = .{ .steps = 3 } }, &vm);
    try testing.expectEqual(8, vm.context.current_stack_frame.instruction_idx);

    // Verify stack is empty after popping condition
    try testing.expectEqual(0, vm.context.stack().len);
}

test "execute jump_if_not instruction with truthy value does not jump" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set initial instruction index
    vm.context.current_stack_frame.instruction_idx = 5;

    // Push truthy value (true)
    try vm.context.stackPush(vm.allocator, Val.init(true));
    try Instruction.execute(.{ .jump_if_not = .{ .steps = 10 } }, &vm);
    try testing.expectEqual(5, vm.context.current_stack_frame.instruction_idx);

    // Push truthy value (any integer)
    try vm.context.stackPush(vm.allocator, Val.init(42));
    try Instruction.execute(.{ .jump_if_not = .{ .steps = -2 } }, &vm);
    try testing.expectEqual(5, vm.context.current_stack_frame.instruction_idx);

    // Push truthy value (symbol)
    try vm.context.stackPush(vm.allocator, try vm.builder().internVal(Symbol.init("test")));
    try Instruction.execute(.{ .jump_if_not = .{ .steps = 1 } }, &vm);
    try testing.expectEqual(5, vm.context.current_stack_frame.instruction_idx);

    // Verify stack is empty after popping all conditions
    try testing.expectEqual(0, vm.context.stack().len);
}

test "execute jump_if_not instruction with empty stack returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Attempt to execute jump_if_not with empty stack
    try testing.expectError(error.StackUnderflow, Instruction.execute(.{ .jump_if_not = .{ .steps = 5 } }, &vm));
}

test "execute squash instruction removes specified number of values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Load test values: (1 2 3 4)
    try vm.context.stackPushMany(vm.allocator, &.{ Val.init(1), Val.init(2), Val.init(3), Val.init(4) });

    // Test squash(1) => (1 2 3 4) - no change
    try Instruction.execute(.{ .squash = 1 }, &vm);
    try testing.expectFmt("(1 2 3 4)", "{f}", .{vm.pretty(vm.context.stack())});

    // Test squash(2) => (1 2 4) - removes 3, keeps 4
    try Instruction.execute(.{ .squash = 2 }, &vm);
    try testing.expectFmt("(1 2 4)", "{f}", .{vm.pretty(vm.context.stack())});

    // Test squash(3) => (4) - removes 1 and 2, keeps 4
    try Instruction.execute(.{ .squash = 3 }, &vm);
    try testing.expectFmt("(4)", "{f}", .{vm.pretty(vm.context.stack())});
}

test "execute squash instruction with count 4 leaves only top value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Load test values: (1 2 3 4)
    try vm.context.stackPushMany(vm.allocator, &.{ Val.init(1), Val.init(2), Val.init(3), Val.init(4) });

    // Test squash(4) => (4) - removes all but top
    try Instruction.execute(.{ .squash = 4 }, &vm);
    try testing.expectFmt("(4)", "{f}", .{vm.pretty(vm.context.stack())});
}

test "execute squash instruction with count 0 does nothing" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Load test values: (1 2 3)
    try vm.context.stackPushMany(vm.allocator, &.{ Val.init(1), Val.init(2), Val.init(3) });

    // Test squash(0) - should do nothing
    try Instruction.execute(.{ .squash = 0 }, &vm);
    try testing.expectFmt("(1 2 3)", "{f}", .{vm.pretty(vm.context.stack())});
}

test "execute squash instruction with insufficient stack returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Load only 2 values
    try vm.context.stackPushMany(vm.allocator, &.{ Val.init(1), Val.init(2) });

    // Attempt to squash 3 values - should fail
    try testing.expectError(error.StackUnderflow, Instruction.execute(.{ .squash = 3 }, &vm));
}

test "execute squash instruction with empty stack returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Attempt to squash with empty stack
    try testing.expectError(error.StackUnderflow, Instruction.execute(.{ .squash = 1 }, &vm));
}

test "execute jump_if_not with pop=false does not remove condition from stack" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set initial instruction index
    vm.context.current_stack_frame.instruction_idx = 5;

    // Push falsy value (false)
    try vm.context.stackPush(vm.allocator, Val.init(false));
    try Instruction.execute(.{ .jump_if_not = .{ .steps = 3, .pop = false } }, &vm);
    try testing.expectEqual(8, vm.context.current_stack_frame.instruction_idx);

    // Verify stack still contains the condition value
    try testing.expectEqual(1, vm.context.stack().len);
    try testing.expectFmt("(#f)", "{f}", .{vm.pretty(vm.context.stack())});
}

test "execute jump_if_not with pop=true removes condition from stack" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set initial instruction index
    vm.context.current_stack_frame.instruction_idx = 5;

    // Push falsy value (false)
    try vm.context.stackPush(vm.allocator, Val.init(false));
    try Instruction.execute(.{ .jump_if_not = .{ .steps = 3, .pop = true } }, &vm);
    try testing.expectEqual(8, vm.context.current_stack_frame.instruction_idx);

    // Verify stack is empty after popping condition
    try testing.expectEqual(0, vm.context.stack().len);
}

test "execute jump_if_not with pop=false and truthy value does not jump" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set initial instruction index
    vm.context.current_stack_frame.instruction_idx = 5;

    // Push truthy value (42)
    try vm.context.stackPush(vm.allocator, Val.init(42));
    try Instruction.execute(.{ .jump_if_not = .{ .steps = 10, .pop = false } }, &vm);
    try testing.expectEqual(5, vm.context.current_stack_frame.instruction_idx);

    // Verify stack still contains the condition value
    try testing.expectEqual(1, vm.context.stack().len);
    try testing.expectFmt("(42)", "{f}", .{vm.pretty(vm.context.stack())});
}

test "execute jump_if_not with pop=false and empty stack returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Attempt to execute jump_if_not with empty stack and pop=false
    try testing.expectError(error.StackUnderflow, Instruction.execute(.{ .jump_if_not = .{ .steps = 5, .pop = false } }, &vm));
}

test "ConditionalJumpParams default pop value is true" {
    const params = Instruction.ConditionalJumpParams{ .steps = 5 };
    try testing.expectEqual(true, params.pop);
}

test "jump_if_not instruction has default pop=true" {
    const instruction: Instruction = .{ .jump_if_not = .{ .steps = 10 } };
    try testing.expectEqual(10, instruction.jump_if_not.steps);
    try testing.expectEqual(true, instruction.jump_if_not.pop);
}

test "execute set_local instruction sets local variable from stack" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up a stack frame with local variables
    vm.context.current_stack_frame.stack_start = 2;
    try vm.context.stackPushMany(vm.allocator, &.{
        Val.init(100), // position 0 - before frame
        Val.init(200), // position 1 - before frame
        Val.init(42), // position 2 - local variable 0
        Val.init(99), // position 3 - local variable 1
    });

    // Push new value to set local variable 0 to
    try vm.context.stackPush(vm.allocator, Val.init(777));

    // Execute set_local instruction for local variable 0
    try Instruction.execute(.{ .set_local = 0 }, &vm);

    // Verify the local variable was set and the value was popped from stack
    try testing.expectFmt(
        "(100 200 777 99)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
    );
}

test "execute set_local instruction with multiple local variables" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up a stack frame with multiple local variables
    vm.context.current_stack_frame.stack_start = 1;
    try vm.context.stackPushMany(vm.allocator, &.{
        Val.init(999), // position 0 - before frame
        Val.init(10), // position 1 - local variable 0
        Val.init(20), // position 2 - local variable 1
        Val.init(30), // position 3 - local variable 2
    });

    // Set local variables in different order
    try vm.context.stackPush(vm.allocator, Val.init(111));
    try Instruction.execute(.{ .set_local = 1 }, &vm); // Set local var 1 to 111

    try vm.context.stackPush(vm.allocator, Val.init(222));
    try Instruction.execute(.{ .set_local = 0 }, &vm); // Set local var 0 to 222

    try vm.context.stackPush(vm.allocator, Val.init(333));
    try Instruction.execute(.{ .set_local = 2 }, &vm); // Set local var 2 to 333

    // Verify all local variables were set correctly
    try testing.expectFmt(
        "(999 222 111 333)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
    );
}

test "execute set_local instruction with zero stack frame start" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up a stack frame that starts at position 0
    vm.context.current_stack_frame.stack_start = 0;
    try vm.context.stackPushMany(vm.allocator, &.{
        Val.init(42), // position 0 - local variable 0
        Val.init(99), // position 1 - local variable 1
    });

    // Set local variable 1
    try vm.context.stackPush(vm.allocator, Val.init(555));
    try Instruction.execute(.{ .set_local = 1 }, &vm);

    // Verify the local variable was set
    try testing.expectFmt(
        "(42 555)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
    );
}

test "set_local instruction works with different value types" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up a stack frame with local variables of different types
    vm.context.current_stack_frame.stack_start = 0;
    try vm.context.stackPushMany(vm.allocator, &.{
        Val.init(true), // position 0 - boolean local variable
        try vm.builder().internVal(Symbol.init("local-symbol")), // position 1 - symbol local variable
        Val.init(-123), // position 2 - negative integer local variable
    });

    // Set different types of values
    try vm.context.stackPush(vm.allocator, Val.init(false));
    try Instruction.execute(.{ .set_local = 0 }, &vm); // Set boolean to false

    try vm.context.stackPush(vm.allocator, try vm.builder().internVal(Symbol.init("new-symbol")));
    try Instruction.execute(.{ .set_local = 1 }, &vm); // Set symbol to new-symbol

    try vm.context.stackPush(vm.allocator, Val.init(456));
    try Instruction.execute(.{ .set_local = 2 }, &vm); // Set integer to 456

    // Verify the values were set correctly
    try testing.expectFmt(
        "(#f new-symbol 456)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
    );
}

test "execute set_local instruction with empty stack returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up a stack frame but don't add any values to the stack
    vm.context.current_stack_frame.stack_start = 0;

    // Attempt to execute set_local with empty stack
    try testing.expectError(error.StackUnderflow, Instruction.execute(.{ .set_local = 0 }, &vm));
}

test "set_local and get_local work together" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Set up a stack frame with local variables
    vm.context.current_stack_frame.stack_start = 1;
    try vm.context.stackPushMany(vm.allocator, &.{
        Val.init(999), // position 0 - before frame
        Val.init(42), // position 1 - local variable 0
        Val.init(99), // position 2 - local variable 1
    });

    // Set local variable 0 to a new value
    try vm.context.stackPush(vm.allocator, Val.init(777));
    try Instruction.execute(.{ .set_local = 0 }, &vm);

    // Get the value back
    try Instruction.execute(.{ .get_local = 0 }, &vm);

    // Verify we got the new value
    try testing.expectFmt(
        "(999 777 99 777)",
        "{f}",
        .{vm.pretty(vm.context.stack())},
    );
}

test "bytecode procedure with locals_count > args allocates additional local variables" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc = Proc{
        .args = 2,
        .locals_count = 4, // 2 more locals than args
        .instructions = try vm.allocator.dupe(Instruction, &.{
            .{ .get_local = 0 }, // Get first argument
            .{ .get_local = 1 }, // Get second argument
            .{ .get_local = 2 }, // Get first additional local (should be {})
            .{ .get_local = 3 }, // Get second additional local (should be {})
        }),
    };
    try vm.context.stackPushMany(vm.allocator, &.{
        try vm.toVal(proc),
        Val.init(10), // first argument
        Val.init(20), // second argument
    });

    try Instruction.execute(.{ .eval_proc = 2 }, &vm);

    // Execute the get_local instructions to verify local variables
    try executeNext(&vm); // Get local 0 (arg 0)
    try executeNext(&vm); // Get local 1 (arg 1)
    try executeNext(&vm); // Get local 2 (additional local)
    try executeNext(&vm); // Get local 3 (additional local)

    try testing.expectEqual(.proc, std.meta.activeTag(vm.context.stack()[0].repr));
    try testing.expectFmt(
        "(10 20 () () 10 20 () ())",
        "{f}",
        .{vm.pretty(vm.context.stack()[1..])},
    );
}

test "bytecode procedure with locals_count equal to args works correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc = Proc{
        .args = 2,
        .locals_count = 2, // same as args
        .instructions = try vm.allocator.dupe(Instruction, &.{
            .{ .get_local = 0 },
            .{ .get_local = 1 },
        }),
    };
    try vm.context.stackPushMany(vm.allocator, &.{
        try vm.toVal(proc),
        Val.init(42),
        Val.init(99),
    });

    try Instruction.execute(.{ .eval_proc = 2 }, &vm);

    // Should work without allocating additional locals
    try testing.expectEqual(.proc, std.meta.activeTag(vm.context.stack()[0].repr));
    try testing.expectFmt(
        "(42 99)",
        "{f}",
        .{vm.pretty(vm.context.stack()[1..])},
    );
}

test "bytecode procedure with wrong argument count raises error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc = Proc{
        .args = 2,
        .locals_count = 3,
        .instructions = try vm.allocator.dupe(
            Instruction,
            &.{.{ .load = Val.init(42) }},
        ),
    };
    try vm.context.stackPushMany(vm.allocator, &.{
        try vm.toVal(proc),
        Val.init(10), // only 1 argument, but procedure expects 2
    });

    try testing.expectError(Vm.Error.UncaughtException, Instruction.execute(.{ .eval_proc = 1 }, &vm));
    // Verify error was set
    try testing.expect(vm.context.err != null);
}

test "bytecode procedure initializes additional locals to unit values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc = Proc{
        .args = 1,
        .locals_count = 2, // 1 additional local
        .instructions = try vm.allocator.dupe(Instruction, &.{
            .{ .set_local = 1 }, // Set additional local to value from stack
            .{ .get_local = 1 }, // Get the value back
        }),
    };
    try vm.context.stackPushMany(vm.allocator, &.{
        try vm.toVal(proc),
        Val.init(42), // argument
    });

    try Instruction.execute(.{ .eval_proc = 1 }, &vm);

    // Push a value to set the additional local
    try vm.context.stackPush(vm.allocator, Val.init(777));
    try executeNext(&vm); // set_local 1
    try executeNext(&vm); // get_local 1

    // Should get back the value we set
    try testing.expectEqual(.proc, std.meta.activeTag(vm.context.stack()[0].repr));
    try testing.expectFmt(
        "(42 777 777)",
        "{f}",
        .{vm.pretty(vm.context.stack()[1..])},
    );
}
