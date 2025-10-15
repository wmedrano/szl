const std = @import("std");

const Instruction = @import("instruction.zig").Instruction;
const Module = @import("types/Module.zig");
const Handle = @import("types/object_pool.zig").Handle;
const Val = @import("types/Val.zig");
const Vm = @import("Vm.zig");

const Context = @This();

stack: std.ArrayList(Val),
stack_frames: std.ArrayList(StackFrame),

pub const StackFrame = struct {
    stack_start: u32 = 0,
    arg_count: u32 = 0,
    instruction_idx: u32 = 0,
    module: ?Handle(Module) = null,
    proc: Val = Val.initEmptyList(),
    exception_handler: Val = Val.initEmptyList(),
    instructions: []const Instruction = &.{},
    constants: []const Val = &.{},

    pub inline fn exceptionHandler(self: StackFrame) ?Val {
        return if (self.exception_handler.isNull()) null else self.exception_handler;
    }
};

pub fn init(allocator: std.mem.Allocator) !Context {
    var stack = try std.ArrayList(Val).initCapacity(allocator, 1024);
    errdefer stack.deinit(allocator);
    var stack_frames = try std.ArrayList(StackFrame).initCapacity(allocator, 64);
    errdefer stack_frames.deinit(allocator);
    return Context{
        .stack = stack,
        .stack_frames = stack_frames,
    };
}

pub fn deinit(self: *Context, allocator: std.mem.Allocator) void {
    self.stack.deinit(allocator);
    self.stack_frames.deinit(allocator);
}

pub fn reset(self: *Context) void {
    self.stack.clearRetainingCapacity();
    self.stack_frames.clearRetainingCapacity();
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

pub fn pushStackFrame(self: *Context, allocator: std.mem.Allocator, stack_frame: StackFrame) error{OutOfMemory}!void {
    try self.stack_frames.append(allocator, stack_frame);
}

/// Describes what to do with the item on top.
pub const TopDestination = enum {
    /// Place it on top of the new stack frame.
    place_on_top,
    /// Do nothing with the top item.
    discard,
};

pub fn setExceptionHandler(self: *Context, handler: Val) error{UndefinedBehavior}!void {
    if (self.stack_frames.items.len == 0) return error.UndefinedBehavior;
    const stack_frame_idx = self.stack_frames.items.len - 1;
    self.stack_frames.items[stack_frame_idx].exception_handler = handler;
}

pub fn currentExceptionHandler(self: Context) ?Val {
    var idx = self.stack_frames.items.len;
    while (idx > 0) {
        idx -= 1;
        if (self.stack_frames.items[idx].exceptionHandler()) |h|
            return h;
    }
    return null;
}

pub fn unwindToNextExceptionHandler(self: *Context) void {
    while (self.stack_frames.items.len > 0) {
        const stack_frame_idx = self.stack_frames.items.len - 1;
        const frame = &self.stack_frames.items[stack_frame_idx];
        if (!frame.exception_handler.isNull()) {
            frame.exception_handler = Val.initEmptyList();
            return;
        }
        self.stack_frames.items.len -= 1;
        self.stack.items.len = frame.stack_start;
    }
}

pub fn popStackFrame(self: *Context, comptime dest: TopDestination) bool {
    if (self.stack_frames.items.len == 0) return false;
    const stack_frame = self.stack_frames.items[self.stack_frames.items.len - 1];
    switch (dest) {
        .place_on_top => {
            const top_val = self.stack.items[self.stack.items.len - 1];
            self.stack.items[stack_frame.stack_start] = top_val;
            self.stack.shrinkRetainingCapacity(stack_frame.stack_start + 1);
        },
        .discard => {
            self.stack.shrinkRetainingCapacity(stack_frame.stack_start);
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
    if (self.stack_frames.items.len == 0) return Val.initEmptyList();
    return self.stack_frames.items[self.stack_frames.items.len - 1].proc;
}

pub fn getArg(self: Context, idx: u32) Val {
    if (self.stack_frames.items.len == 0) return Val.initEmptyList();
    const abs_idx = self.stack_frames.items[self.stack_frames.items.len - 1].stack_start + idx;
    return self.stack.items[@intCast(abs_idx)];
}

pub fn getLocal(self: *Context, idx: u32) Val {
    if (self.stack_frames.items.len == 0) return Val.initEmptyList();
    const frame_idx = self.stack_frames.items.len - 1;
    const abs_idx = self.stack_frames.items[frame_idx].stack_start + self.stack_frames.items[frame_idx].arg_count + idx;
    return self.stack.items[@intCast(abs_idx)];
}

pub fn setLocal(self: *Context, idx: u32, val: Val) void {
    const local_stack = self.stackLocal();
    const stack_idx = self.argCount() + idx;
    local_stack[@intCast(stack_idx)] = val;
}

pub fn getConstant(self: Context, idx: u32) Val {
    if (self.stack_frames.items.len == 0) return Val.initEmptyList();
    return self.stack_frames.items[self.stack_frames.items.len - 1].constants[@intCast(idx)];
}

pub fn stackTopN(self: Context, n: u32) []const Val {
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

pub fn top(self: *Context) ?Val {
    const len = self.stack.items.len;
    if (len == 0) return null;
    return self.stack.items[len - 1];
}

pub fn stackSquash(self: *Context, n: u32) Vm.Error!void {
    if (n < 2 or self.stack.items.len == 0)
        return Vm.Error.UndefinedBehavior;
    const top_idx = self.stack.items.len - 1;
    const bottom_idx = self.stack.items.len - @as(usize, @intCast(n));
    self.stack.items[bottom_idx] = self.stack.items[top_idx];
    self.stack.items.len = bottom_idx + 1;
}
