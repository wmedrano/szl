const std = @import("std");
const testing = std.testing;

const ErrorDetails = @import("types/ErrorDetails.zig");
const Instruction = @import("instruction.zig").Instruction;
const Module = @import("types/Module.zig");
const Handle = @import("types/object_pool.zig").Handle;
const Parameter = @import("types/Parameter.zig");
const Val = @import("types/Val.zig");
const Vm = @import("Vm.zig");

const Context = @This();

stack: std.ArrayList(Val),
stack_frames: std.ArrayList(StackFrame),
/// Parameter bindings for dynamic scoping.
/// Stores saved parameter values when entering parameterize blocks.
parameter_bindings: std.ArrayList(ParameterBinding),

pub const ParameterBinding = struct {
    parameter: ?Handle(Parameter),
    val: Val,
};

pub const StackFrame = struct {
    stack_start: u32 = 0,
    arg_count: u32 = 0,
    instruction_idx: u32 = 0,
    module: ?Handle(Module) = null,
    proc: Val = Val.initUnspecified(),
    instructions: []const Instruction = &.{},
    constants: []const Val = &.{},
    /// Index into Context.parameter_bindings where this frame's bindings start.
    /// All bindings from this index until the next frame (or end) belong to this frame.
    param_bindings_start: u32 = 0,
};

pub fn init(allocator: std.mem.Allocator) !Context {
    var stack = try std.ArrayList(Val).initCapacity(allocator, 1024);
    errdefer stack.deinit(allocator);
    var stack_frames = try std.ArrayList(StackFrame).initCapacity(allocator, 64);
    errdefer stack_frames.deinit(allocator);
    var parameter_bindings = try std.ArrayList(ParameterBinding).initCapacity(allocator, 32);
    errdefer parameter_bindings.deinit(allocator);
    return Context{
        .stack = stack,
        .stack_frames = stack_frames,
        .parameter_bindings = parameter_bindings,
    };
}

pub fn deinit(self: *Context, allocator: std.mem.Allocator) void {
    self.stack.deinit(allocator);
    self.stack_frames.deinit(allocator);
    self.parameter_bindings.deinit(allocator);
}

pub fn reset(self: *Context) void {
    self.stack.clearRetainingCapacity();
    self.stack_frames.clearRetainingCapacity();
    self.parameter_bindings.clearRetainingCapacity();
}

pub fn nextInstruction(self: *Context) ?Instruction {
    if (self.stack_frames.items.len == 0) return null;
    const stack_frame_idx = self.stack_frames.items.len - 1;
    const instructions = self.stack_frames.items[stack_frame_idx].instructions;
    const idx: usize = @intCast(self.stack_frames.items[stack_frame_idx].instruction_idx);
    self.stack_frames.items[stack_frame_idx].instruction_idx += 1;
    const instruction = if (idx < instructions.len) instructions[idx] else return null;
    return instruction;
}

pub fn jump(self: *Context, n: i32) !void {
    if (self.stack_frames.items.len == 0) return;
    const frame_idx = self.stack_frames.items.len - 1;
    const old_idx: i32 = @intCast(self.stack_frames.items[frame_idx].instruction_idx);
    const new_idx = old_idx + n;
    self.stack_frames.items[frame_idx].instruction_idx = @intCast(new_idx);
}

fn exceptionHandlerParam(vm: *Vm) error{ OutOfMemory, UndefinedBehavior }!Handle(Parameter) {
    const inspector = vm.inspector();
    // TODO: We should use the (scheme base) or maybe an internal
    // environment instead of the REPL.
    var error_details = ErrorDetails{};
    defer error_details.deinit(vm.allocator());
    const repl_env_h = try inspector.getReplEnv(&error_details);
    const repl_env = try inspector.handleToModule(repl_env_h);
    const sym = try vm.builder().makeStaticSymbolHandle("%szl-exception-handler");
    const param_val = repl_env.getBySymbol(sym) orelse return error.UndefinedBehavior;
    const param = param_val.asParameter() orelse {
        return error.UndefinedBehavior;
    };
    return param;
}

pub fn pushStackFrame(
    self: *Context,
    vm: *Vm,
    arg_count: u32,
    locals_count: u32,
    proc: Val,
    module_handle: ?Handle(Module),
    instructions: []const Instruction,
    constants: []const Val,
) error{OutOfMemory}!void {
    const stack_start: u32 = @intCast(self.stack.items.len - arg_count - locals_count);
    try self.stack_frames.append(vm.allocator(), .{
        .stack_start = stack_start,
        .arg_count = arg_count,
        .module = module_handle,
        .proc = proc,
        .instructions = instructions,
        .constants = constants,
        .param_bindings_start = @intCast(self.parameter_bindings.items.len),
    });
}

pub fn setExceptionHandler(_: *Context, vm: *Vm, handler: Val) error{ OutOfMemory, UndefinedBehavior }!void {
    if (handler.isUnspecified()) return;
    try vm.context.parameter_bindings.append(
        vm.allocator(),
        .{ .parameter = try exceptionHandlerParam(vm), .val = handler },
    );
}

pub fn currentExceptionHandler(self: Context, vm: *Vm) ?Val {
    const handler = exceptionHandlerParam(vm) catch unreachable;
    const param = self.resolveParameter(vm, handler) catch unreachable;
    if (param.isUnspecified()) return null;
    return param;
}

pub fn unwindToNextExceptionHandler(self: *Context, vm: *Vm) void {
    const handler = exceptionHandlerParam(vm) catch unreachable;

    while (self.stack_frames.items.len > 0) {
        const stack_frame_idx = self.stack_frames.items.len - 1;
        const frame = &self.stack_frames.items[stack_frame_idx];
        var has_handler = false;
        for (self.parameter_bindings.items[frame.param_bindings_start..]) |*binding| {
            if (binding.parameter) |param| {
                if (!binding.val.isUnspecified() and param.eq(handler)) {
                    has_handler = true;
                    binding.val = Val.initUnspecified();
                    break;
                }
            }
        }
        self.stack_frames.items.len -= 1;
        self.parameter_bindings.items.len = frame.param_bindings_start;
        self.stack.items.len = frame.stack_start;
        if (has_handler) return;
    }
}

pub fn resolveParameter(self: Context, vm: *Vm, handle: Handle(Parameter)) !Val {
    var idx = self.parameter_bindings.items.len;
    while (idx > 0) {
        idx -= 1;
        const bindings = self.parameter_bindings.items[idx];
        if (bindings.parameter) |b| {
            if (b.eq(handle)) return bindings.val;
        }
    }
    const param_ptr = vm.objects.parameters.get(handle) orelse return Vm.Error.UndefinedBehavior;
    return param_ptr.getValue();
}

/// Describes what to do with the stack when popping a stack frame.
pub const PopReturnVal = enum {
    /// Place the top value on top of the new stack frame.
    /// Use this when returning a value from a function call.
    /// Stack effect: [caller_args... | callee_result] -> [caller_args... result]
    return_top,

    /// Discard all values from the popped frame, including the top.
    /// Use this when you don't need any return value.
    /// Stack effect: [caller_args... | callee_values...] -> [caller_args...]
    discard,

    /// Leave exactly one value on the stack after popping the frame.
    /// Use this when you want to preserve the first value pushed in the frame.
    /// Stack effect: [caller_args... | first_value other_values...] -> [caller_args... first_value]
    return_first,
};

pub fn popStackFrame(self: *Context, comptime dest: PopReturnVal) bool {
    if (self.stack_frames.items.len == 0) return false;
    const stack_frame = self.stack_frames.items[self.stack_frames.items.len - 1];

    // 1. Restore parameters.
    const bindings_start: usize = @intCast(stack_frame.param_bindings_start);
    self.parameter_bindings.shrinkRetainingCapacity(bindings_start);

    // 2. Restore stack.
    switch (dest) {
        .return_top => {
            const top_val = self.stack.items[self.stack.items.len - 1];
            self.stack.items[stack_frame.stack_start] = top_val;
            self.stack.shrinkRetainingCapacity(stack_frame.stack_start + 1);
        },
        .discard => {
            self.stack.shrinkRetainingCapacity(stack_frame.stack_start);
        },
        .return_first => {
            self.stack.shrinkRetainingCapacity(stack_frame.stack_start + 1);
        },
    }
    self.stack_frames.items.len -= 1;
    return true;
}

pub fn argCount(self: Context) u32 {
    if (self.stack_frames.items.len == 0) return 0;
    return self.stack_frames.items[self.stack_frames.items.len - 1].arg_count;
}

fn stackLocal(self: Context) []Val {
    if (self.stack_frames.items.len == 0) return &.{};
    const start: usize = @intCast(self.stack_frames.items[self.stack_frames.items.len - 1].stack_start);
    return self.stack.items[start..];
}

pub fn module(self: Context) ?Handle(Module) {
    if (self.stack_frames.items.len == 0) return null;
    return self.stack_frames.items[self.stack_frames.items.len - 1].module;
}

pub fn getProc(self: Context) Val {
    if (self.stack_frames.items.len == 0) return Val.initUnspecified();
    return self.stack_frames.items[self.stack_frames.items.len - 1].proc;
}

pub fn getArg(self: Context, idx: u32) Val {
    if (self.stack_frames.items.len == 0) return Val.initUnspecified();
    const abs_idx = self.stack_frames.items[self.stack_frames.items.len - 1].stack_start + idx;
    return self.stack.items[@intCast(abs_idx)];
}

pub fn getLocal(self: *Context, idx: u32) Val {
    if (self.stack_frames.items.len == 0) return Val.initUnspecified();
    const frame_idx = self.stack_frames.items.len - 1;
    const abs_idx = self.stack_frames.items[frame_idx].stack_start + self.stack_frames.items[frame_idx].arg_count + idx;
    return self.stack.items[@intCast(abs_idx)];
}

pub fn setArg(self: *Context, idx: u32, val: Val) void {
    const local_stack = self.stackLocal();
    local_stack[@intCast(idx)] = val;
}

pub fn setLocal(self: *Context, idx: u32, val: Val) void {
    const local_stack = self.stackLocal();
    const stack_idx = self.argCount() + idx;
    local_stack[@intCast(stack_idx)] = val;
}

pub fn getConstant(self: Context, idx: u32) Val {
    if (self.stack_frames.items.len == 0) return Val.initUnspecified();
    return self.stack_frames.items[self.stack_frames.items.len - 1].constants[@intCast(idx)];
}

pub fn stackTopN(self: Context, n: u32) []Val {
    const start = self.stackLen() - n;
    const start_idx: usize = @intCast(start);
    return self.stack.items[start_idx..];
}

/// Returns a mutable slice of the top N stack values.
/// This API is unstable - it's unclear whether exposing mutable access
/// to the internal stack is the right design choice.
pub fn stackTopNUnstable(self: Context, n: u32) []Val {
    const start = self.stackLen() - n;
    const start_idx: usize = @intCast(start);
    return self.stack.items[start_idx..];
}

pub fn stackLen(self: Context) u32 {
    return @intCast(self.stack.items.len);
}

pub fn stackVal(self: Context, idx: u32) ?Val {
    const index: usize = @intCast(idx);
    if (index < self.stack.items.len) return self.stack.items[index] else return null;
}

pub fn push(self: *Context, allocator: std.mem.Allocator, val: Val) error{OutOfMemory}!void {
    try self.stack.append(allocator, val);
}

pub fn pushMany(self: *Context, allocator: std.mem.Allocator, val: Val, n: u32) error{OutOfMemory}!void {
    try self.stack.appendNTimes(allocator, val, @intCast(n));
}

pub fn pushSlice(self: *Context, allocator: std.mem.Allocator, vals: []const Val) error{OutOfMemory}!void {
    try self.stack.appendSlice(allocator, vals);
}

pub fn pop(self: *Context) ?Val {
    return self.stack.pop();
}

pub fn popMany(self: *Context, n: u32) void {
    self.stack.items.len -= @intCast(n);
}

pub fn top_idx(self: Context) ?usize {
    const len = self.stack.items.len;
    if (len == 0) return null;
    return len - 1;
}

pub fn at_idx(self: Context, idx: usize) Val {
    return self.stack.items[idx];
}

pub fn set_idx(self: *Context, idx: usize, val: Val) void {
    self.stack.items[idx] = val;
}

pub fn top(self: *Context) ?Val {
    const len = self.stack.items.len;
    if (len == 0) return null;
    return self.stack.items[len - 1];
}

pub fn stackSquash(self: *Context, n: u32) Vm.Error!void {
    if (n < 2 or self.stack.items.len == 0)
        return Vm.Error.UndefinedBehavior;
    const top_idx_var = self.stack.items.len - 1;
    const bottom_idx = self.stack.items.len - @as(usize, @intCast(n));
    self.stack.items[bottom_idx] = self.stack.items[top_idx_var];
    self.stack.items.len = bottom_idx + 1;
}

test "stack frame structs are relatively small" {
    try testing.expectEqual(72, @sizeOf(StackFrame));
    try testing.expectEqual(24, @sizeOf(ParameterBinding));
}
