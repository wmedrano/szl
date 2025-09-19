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

/// Creates a new return_value instruction.
pub fn initReturnValue() Instruction {
    return Instruction{ .repr = .return_value };
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
    /// Instruction to evaluate a procedure with arguments from the stack.
    /// The arg_count specifies how many arguments to pass to the procedure.
    eval_procedure: usize,
    /// Instruction to return from the current procedure.
    /// Restores the previous stack frame and places the return value at the correct position.
    return_value,
};

/// Executes this instruction on the given virtual machine.
/// Dispatches to the appropriate instruction handler based on the instruction type.
///
/// Supported instructions:
///   - load: Pushes a value onto the stack
///   - get_global: Retrieves a global variable and pushes its value onto the stack
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
        .get_global => |symbol| return getGlobal(vm, symbol),
        .eval_procedure => |arg_count| return evalProcedure(vm, arg_count),
        .return_value => return returnValue(vm),
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
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try execute(Instruction.initLoad(Val.init(10)), &vm);
    try execute(Instruction.initLoad(Val.init(20)), &vm);
    try execute(Instruction.initLoad(Val.init(30)), &vm);

    try testing.expectFmt(
        "(10 20 30)",
        "{f}",
        .{vm.inspector().prettySlice(vm.stack.items)},
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
        .{vm.inspector().prettySlice(vm.stack.items)},
    );
}

test "execute bytecode procedure loads instructions into stack frame" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc = try vm.toVal(Procedure{
        .name = try vm.interner.internStatic(Symbol.init("test-bytecode")),
        .implementation = Procedure.initBytecode(&[_]Instruction{
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
        .implementation = Procedure.initBytecode(&[_]Instruction{
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
        .implementation = Procedure.initBytecode(&[_]Instruction{
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
        .{vm.inspector().prettySlice(vm.stack.items)},
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
        .{vm.inspector().prettySlice(vm.stack.items)},
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
        .{vm.inspector().prettySlice(vm.stack.items)},
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
    try testing.expectFmt("(1 2 3)", "{f}", .{vm.inspector().prettySlice(vm.stack.items)});
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
        .{vm.inspector().prettySlice(vm.stack.items)},
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
        .{vm.inspector().prettySlice(vm.stack.items)},
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
        .{vm.inspector().prettySlice(vm.stack.items)},
    );
}

test "get_global instruction works with different value types" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Define global variables of different types using Builder
    try vm.builder().define(Symbol.init("bool-var"), Val.init(true));
    try vm.builder().define(Symbol.init("symbol-var"), try vm.builder().build(Symbol.init("test-symbol")));

    const bool_symbol = try vm.interner.intern(Symbol.init("bool-var"));
    const symbol_symbol = try vm.interner.intern(Symbol.init("symbol-var"));

    // Execute get_global instructions
    try execute(Instruction.initGetGlobal(bool_symbol), &vm);
    try execute(Instruction.initGetGlobal(symbol_symbol), &vm);

    // Verify the values were loaded correctly
    try testing.expectFmt(
        "(#t test-symbol)",
        "{f}",
        .{vm.inspector().prettySlice(vm.stack.items)},
    );
}
