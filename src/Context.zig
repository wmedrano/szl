const std = @import("std");

const Instruction = @import("instruction.zig").Instruction;
const Val = @import("types/Val.zig");
const Vm = @import("Vm.zig");

const Context = @This();

stack: std.ArrayList(Val),
stack_frame: StackFrame = StackFrame{},
stack_frames: std.ArrayList(StackFrame),

pub const StackFrame = struct {
    stack_start: u32 = 0,
    arg_count: u32 = 0,
    instruction_idx: u32 = 0,
    proc: Val = Val.initEmptyList(),
    exception_handler: Val = Val.initEmptyList(),
    instructions: []const Instruction = &.{},
    captures: []const Val = &.{},

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
    self.stack_frame = StackFrame{};
}

pub fn nextInstruction(self: *Context) ?Instruction {
    const instructions = self.stack_frame.instructions;
    const idx: usize = @intCast(self.stack_frame.instruction_idx);
    self.stack_frame.instruction_idx += 1;
    const instruction = if (idx < instructions.len) instructions[idx] else return null;
    return instruction;
}

pub fn jump(self: *Context, n: i32) !void {
    const old_idx: i32 = @intCast(self.stack_frame.instruction_idx);
    const new_idx = old_idx + n;
    self.stack_frame.instruction_idx = @intCast(new_idx);
}

pub fn pushStackFrame(self: *Context, allocator: std.mem.Allocator, stack_frame: StackFrame) error{OutOfMemory}!void {
    try self.stack_frames.append(allocator, self.stack_frame);
    self.stack_frame = stack_frame;
}

/// Describes what to do with the item on top.
pub const TopDestination = enum {
    /// Place it on top of the new stack frame.
    place_on_top,
    /// Do nothing with the top item.
    discard,
};

pub fn setExceptionHandler(self: *Context, handler: Val) error{UndefinedBehavior}!void {
    self.stack_frame.exception_handler = handler;
}

pub fn currentExceptionHandler(self: Context) ?Val {
    if (self.stack_frame.exceptionHandler()) |h| return h;
    var idx = self.stack_frames.items.len;
    while (idx > 0) {
        idx -= 1;
        if (self.stack_frames.items[idx].exceptionHandler()) |h|
            return h;
    }
    return null;
}

pub fn unwindBeforeExceptionHandler(self: *Context) Vm.Error!?Val {
    if (self.stack_frame.exceptionHandler()) |h| {
        _ = self.popStackFrame(.discard);
        return h;
    }
    while (self.stack_frames.pop()) |frame| {
        if (frame.exceptionHandler()) |h| {
            self.stack.shrinkRetainingCapacity(frame.stack_start);
            self.stack_frame = frame;
            self.stack_frame.exception_handler = Val.initEmptyList();
            return h;
        }
    }
    return null;
}

pub fn popStackFrame(self: *Context, comptime dest: TopDestination) bool {
    if (self.stack_frames.items.len == 0) return false;
    switch (dest) {
        .place_on_top => {
            const top_val = self.stack.items[self.stack.items.len - 1];
            self.stack.items[self.stack_frame.stack_start] = top_val;
            self.stack.shrinkRetainingCapacity(self.stack_frame.stack_start + 1);
            self.stack_frame = self.stack_frames.pop() orelse StackFrame{};
        },
        .discard => {
            self.stack.shrinkRetainingCapacity(self.stack_frame.stack_start);
            self.stack_frame = self.stack_frames.pop() orelse StackFrame{};
        },
    }
    return true;
}

pub fn argCount(self: Context) u32 {
    return self.stack_frame.arg_count;
}

fn stackLocal(self: Context) []Val {
    const start: usize = @intCast(self.stack_frame.stack_start);
    return self.stack.items[start..];
}

pub fn getProc(self: Context) Val {
    return self.stack_frame.proc;
}

pub fn getCapture(self: Context, idx: u32) Val {
    return self.stack_frame.captures[@intCast(idx)];
}

pub fn getArg(self: Context, idx: u32) Val {
    const abs_idx = self.stack_frame.stack_start + idx;
    return self.stack.items[@intCast(abs_idx)];
}

pub fn getLocal(self: *Context, idx: u32) Val {
    const abs_idx = self.stack_frame.stack_start + self.stack_frame.arg_count + idx;
    return self.stack.items[@intCast(abs_idx)];
}

pub fn setLocal(self: *Context, idx: u32, val: Val) void {
    const local_stack = self.stackLocal();
    const stack_idx = self.argCount() + idx;
    local_stack[@intCast(stack_idx)] = val;
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
    const len = self.stack.items.len;
    self.stack.shrinkRetainingCapacity(len - @as(usize, @intCast(n)));
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
    self.stack.shrinkRetainingCapacity(bottom_idx + 1);
}
