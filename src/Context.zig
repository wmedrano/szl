const std = @import("std");
const testing = std.testing;

const Instruction = @import("instruction.zig").Instruction;
const NativeProc = @import("NativeProc.zig");
const Proc = @import("Proc.zig");
const Val = @import("types/Val.zig");
const Vm = @import("Vm.zig");

/// A context data structure for managing Scheme values.
///
/// Provides a simple wrapper around ArrayList(Val) with convenient
/// methods for stack operations used throughout the VM.
const Context = @This();

/// The underlying stack storage for values.
stack_vals: std.ArrayList(Val),

/// Stack of procedure call frames, enabling nested procedure calls and proper scoping.
/// Each frame tracks the stack position and execution state for a procedure call.
stack_frames: std.ArrayList(StackFrame),

/// The currently executing stack frame containing execution state.
/// Tracks the current instruction pointer and stack frame boundaries.
current_stack_frame: Context.StackFrame = .{},

/// Error state for the virtual machine. When set, indicates that an error
/// has occurred during execution and should be handled or propagated.
///
/// TODO: This shouldn't exist. Errors should be handled only with exception
/// handlers.
err: ?Val = null,

const default_exception_handler = NativeProc.Native{
    .name = "szl-default-exception-handler",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const local_stack = ctx.localStack();
            ctx.vm.context.err = if (local_stack.len == 0) Val.init({}) else local_stack[0];
            return Vm.Error.UncaughtException;
        }
    }.func,
};

/// Represents a stack frame for procedure calls.
///
/// Each frame tracks the start position of its arguments on the stack, enabling
/// proper scoping and parameter access during procedure execution.
pub const StackFrame = struct {
    /// Index into the stack where this frame's local variables and arguments begin.
    /// Used as the base address for calculating absolute positions of local variables.
    stack_start: usize = 0,
    /// Bytecode instructions for this stack frame to execute.
    /// Contains the compiled procedure's instruction sequence.
    instructions: []const Instruction = &.{},
    /// Current instruction pointer within the instructions array.
    /// Points to the next instruction to be executed in this frame.
    instruction_idx: usize = 0,
    /// The currently installed exception handler.
    exception_handler: Val = Val.init(&default_exception_handler),
    /// The exception handler to install for the next call.
    next_exception_handler: Val = Val.init(&default_exception_handler),
};

/// Initializes a new Context with pre-allocated capacity for efficient operation.
///
/// Creates a new Context instance with optimized initial capacity: 1024 values
/// for the main stack and 64 stack frames for procedure calls. This reduces
/// memory allocations during typical program execution.
///
/// Args:
///   allocator: The memory allocator to use for dynamic allocations.
///
/// Returns:
///   A new Context instance ready for use, or an allocation error.
pub fn init(allocator: std.mem.Allocator) std.mem.Allocator.Error!Context {
    return Context{
        .stack_vals = try std.ArrayList(Val).initCapacity(allocator, 1024),
        .stack_frames = try std.ArrayList(StackFrame).initCapacity(allocator, 64),
    };
}

/// Deinitializes the context, freeing all allocated memory.
///
/// Args:
///   allocator: The allocator used to create the context.
pub fn deinit(self: *Context, allocator: std.mem.Allocator) void {
    self.stack_vals.deinit(allocator);
    self.stack_frames.deinit(allocator);
}

/// Creates a deep copy of the context with all its current state.
///
/// Args:
///   self: The context to clone.
///   allocator: The memory allocator to use for the new context.
///
/// Returns:
///   A new Context instance that is an exact copy of the original.
pub fn clone(self: *const Context, allocator: std.mem.Allocator) std.mem.Allocator.Error!Context {
    const new_context = Context{
        .stack_vals = try self.stack_vals.clone(allocator),
        .stack_frames = try self.stack_frames.clone(allocator),
        .current_stack_frame = self.current_stack_frame,
        .err = self.err,
    };
    return new_context;
}

/// Copies state from another context into this context.
///
/// Args:
///   self: The destination context to copy into.
///   allocator: The memory allocator to use for allocations.
///   other: The source context to copy from.
///
/// Returns:
///   An error if memory allocation fails during copying.
pub fn copyFrom(self: *Context, allocator: std.mem.Allocator, other: Context) !void {
    self.stack_vals.clearRetainingCapacity();
    self.stack_frames.clearRetainingCapacity();

    try self.stack_vals.appendSlice(allocator, other.stack_vals.items);
    try self.stack_frames.appendSlice(allocator, other.stack_frames.items);

    self.current_stack_frame = other.current_stack_frame;
    self.err = other.err;
}

/// Resets the context to its initial state, clearing all runtime data.
///
/// Clears the error state, empties the stack while retaining capacity for performance,
/// and clears all stack frames. This is typically called between program executions
/// to prepare for a fresh run.
///
/// Args:
///   self: A pointer to the Context instance to reset.
pub fn reset(self: *Context) void {
    self.err = null;
    self.stack_vals.clearRetainingCapacity();
    self.stack_frames.clearRetainingCapacity();
}

/// Returns a slice view of all values currently on the stack.
///
/// The returned slice is valid until the next stack modification operation.
///
/// Returns:
///   A slice containing all values on the stack from bottom to top.
pub fn stack(self: *Context) []Val {
    return self.stack_vals.items;
}

pub fn constStack(self: Context) []const Val {
    return self.stack_vals.items;
}

pub fn currentProc(self: Context) Val {
    const idx = self.current_stack_frame.stack_start - 1;
    return self.stack_vals.items[idx];
}

/// Appends a value to the top of the stack.
///
/// Args:
///   val: The Scheme value to push onto the stack.
///
/// Returns:
///   An error if memory allocation fails during stack growth.
pub fn stackPush(self: *Context, allocator: std.mem.Allocator, val: Val) std.mem.Allocator.Error!void {
    return self.stack_vals.append(allocator, val);
}

/// Removes and returns the value from the top of the stack.
///
/// Returns:
///   The value that was at the top of the stack, or null if the stack is empty.
pub fn stackPop(self: *Context) ?Val {
    return if (self.stack_vals.items.len == 0) null else self.stack_vals.pop();
}

/// Appends multiple values to the top of the stack.
///
/// Args:
///   values: A slice of Scheme values to push onto the stack in order.
///
/// Returns:
///   An error if memory allocation fails during stack growth.
pub fn stackPushMany(self: *Context, allocator: std.mem.Allocator, values: []const Val) std.mem.Allocator.Error!void {
    return self.stack_vals.appendSlice(allocator, values);
}

/// Returns from the current procedure call, preserving the top stack value as the return value.
///
/// Restores the previous stack frame by:
/// 1. Moving the top stack value to replace the procedure's arguments
/// 2. Shrinking the stack to remove local variables and intermediate values
/// 3. Popping the current stack frame to restore the caller's execution context
///
/// Returns:
///   StackUnderflow error if there are no stack frames to return to.
pub fn returnValue(self: *Context) !void {
    const new_stack_len = self.current_stack_frame.stack_start;
    const dst_idx = new_stack_len - 1;
    const src_idx = self.stack().len - 1;
    self.stack()[dst_idx] = self.stack()[src_idx];
    self.stack_vals.shrinkRetainingCapacity(new_stack_len);
    self.current_stack_frame = self.stack_frames.pop() orelse return error.StackUnderflow;
}

test "Context basic operations" {
    var context = Context{
        .stack_vals = try std.ArrayList(Val).initCapacity(testing.allocator, 0),
        .stack_frames = try std.ArrayList(StackFrame).initCapacity(testing.allocator, 0),
    };
    defer context.deinit(testing.allocator);

    // Test empty stack
    try testing.expect(context.stack().len == 0);
    try testing.expect(context.stackPop() == null);

    // Test stackPush and stack
    const val1 = Val.init(42);
    const val2 = Val.init(true);

    try context.stackPush(testing.allocator, val1);
    try testing.expect(context.stack().len == 1);
    try testing.expect(std.meta.eql(context.stack()[0], val1));

    try context.stackPush(testing.allocator, val2);
    try testing.expect(context.stack().len == 2);
    try testing.expect(std.meta.eql(context.stack()[0], val1));
    try testing.expect(std.meta.eql(context.stack()[1], val2));
}

test "Context pop operations" {
    var context = Context{
        .stack_vals = try std.ArrayList(Val).initCapacity(testing.allocator, 0),
        .stack_frames = try std.ArrayList(StackFrame).initCapacity(testing.allocator, 0),
    };
    defer context.deinit(testing.allocator);

    const val1 = Val.init(42);
    const val2 = Val.init(true);

    try context.stackPush(testing.allocator, val1);
    try context.stackPush(testing.allocator, val2);

    // Pop values in LIFO order
    const popped1 = context.stackPop();
    try testing.expect(popped1 != null);
    try testing.expect(std.meta.eql(popped1.?, val2));
    try testing.expect(context.stack().len == 1);

    const popped2 = context.stackPop();
    try testing.expect(popped2 != null);
    try testing.expect(std.meta.eql(popped2.?, val1));
    try testing.expect(context.stack().len == 0);

    // Pop from empty stack
    try testing.expect(context.stackPop() == null);
}

test "Context stackPushMany operations" {
    var context = Context{
        .stack_vals = try std.ArrayList(Val).initCapacity(testing.allocator, 0),
        .stack_frames = try std.ArrayList(StackFrame).initCapacity(testing.allocator, 0),
    };
    defer context.deinit(testing.allocator);

    const values = [_]Val{ Val.init(1), Val.init(2), Val.init(3) };

    try context.stackPushMany(testing.allocator, &values);
    try testing.expect(context.stack().len == 3);
    try testing.expect(std.meta.eql(context.stack()[0], Val.init(1)));
    try testing.expect(std.meta.eql(context.stack()[1], Val.init(2)));
    try testing.expect(std.meta.eql(context.stack()[2], Val.init(3)));

    // Test empty slice append
    try context.stackPushMany(testing.allocator, &[_]Val{});
    try testing.expect(context.stack().len == 3);
}
