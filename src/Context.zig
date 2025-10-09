const std = @import("std");

const Instruction = @import("instruction.zig").Instruction;
const Val = @import("types/Val.zig");
const Vm = @import("Vm.zig");

const Context = @This();

stack: std.ArrayList(Val),
stack_frames: std.ArrayList(StackFrame),

const StackFrame = struct {
    stack_start: u32 = 0,
    arg_count: u32 = 0,
    locals_count: u32 = 0,
    instruction_idx: u32 = 0,
    exception_handler: ?Val = null,
    instructions: []const Instruction = &.{},
};

pub fn init(allocator: std.mem.Allocator) !Context {
    var stack = try std.ArrayList(Val).initCapacity(allocator, 1024);
    errdefer stack.deinit(allocator);
    return Context{
        .stack = stack,
        .stack_frames = try std.ArrayList(StackFrame).initCapacity(allocator, 64),
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
    const frame_idx = self.stack_frames.items.len - 1;
    const frame = self.stack_frames.items[frame_idx];
    const instructions = frame.instructions;
    const idx: usize = @intCast(frame.instruction_idx);
    self.stack_frames.items[frame_idx].instruction_idx += 1;
    const instruction = if (idx < instructions.len) instructions[idx] else return null;
    return instruction;
}

pub fn jump(self: *Context, n: i32) !void {
    if (self.stack_frames.items.len == 0) return error.UndefinedBehavior;
    const frame_idx = self.stack_frames.items.len - 1;
    const old_idx: i32 = @intCast(self.stack_frames.items[frame_idx].instruction_idx);
    const new_idx = old_idx + n;
    self.stack_frames.items[frame_idx].instruction_idx = @intCast(new_idx);
}

pub fn pushStackFrame(self: *Context, allocator: std.mem.Allocator, stack_frame: StackFrame) error{OutOfMemory}!void {
    return self.stack_frames.append(allocator, stack_frame);
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
    const idx = self.stack_frames.items.len - 1;
    self.stack_frames.items[idx].exception_handler = handler;
}

pub fn currentExceptionHandler(self: Context) ?Val {
    if (self.stack_frames.items.len == 0) return null;
    var idx = self.stack_frames.items.len - 1;
    while (idx > 0) {
        idx -= 1;
        if (self.stack_frames.items[idx].exception_handler) |handler| {
            return handler;
        }
    }
    return null;
}

pub fn unwindUntilException(self: *Context) Vm.Error!?Val {
    while (self.stack_frames.items.len > 0) {
        const idx = self.stack_frames.items.len - 1;
        const frame = self.stack_frames.items[idx];
        if (frame.exception_handler) |handler| return handler;
        try self.popStackFrame(.discard);
    }
    return null;
}

pub fn popStackFrame(self: *Context, comptime dest: TopDestination) Vm.Error!void {
    switch (dest) {
        .place_on_top => {
            const top_val = self.pop() orelse return Vm.Error.UndefinedBehavior;
            const frame = self.stack_frames.pop() orelse StackFrame{};
            self.stack.shrinkRetainingCapacity(frame.stack_start);
            try self.swapTop(top_val);
        },
        .discard => {
            const frame = self.stack_frames.pop() orelse StackFrame{};
            self.stack.shrinkRetainingCapacity(frame.stack_start);
        },
    }
}

pub fn argCount(self: Context) u32 {
    if (self.stack_frames.items.len == 0) return 0;
    const frame = self.stack_frames.items[self.stack_frames.items.len - 1];
    return frame.arg_count;
}

fn stackLocal(self: Context) []Val {
    if (self.stack_frames.items.len == 0) return &.{};
    const frame = self.stack_frames.items[self.stack_frames.items.len - 1];
    const start: usize = @intCast(frame.stack_start);
    return self.stack.items[start..];
}

pub fn getCapture(self: Context, idx: u32) Vm.Error!Val {
    if (self.stack_frames.items.len == 0) return Vm.Error.UndefinedBehavior;
    const frame = self.stack_frames.items[self.stack_frames.items.len - 1];
    const abs_idx = frame.stack_start + frame.arg_count + frame.locals_count + idx;
    return self.stack.items[@intCast(abs_idx)];
}

// TODO: Document negative behavior.
pub fn getArg(self: Context, idx: i32) Vm.Error!Val {
    if (self.stack_frames.items.len == 0) return Vm.Error.UndefinedBehavior;
    const frame = self.stack_frames.items[self.stack_frames.items.len - 1];
    const abs_idx = @as(i32, @intCast(frame.stack_start)) + idx;
    return self.stack.items[@intCast(abs_idx)];
}

pub fn getLocal(self: *Context, idx: u32) Vm.Error!Val {
    if (self.stack_frames.items.len == 0) return Vm.Error.UndefinedBehavior;
    const frame = self.stack_frames.items[self.stack_frames.items.len - 1];
    const abs_idx = frame.stack_start + frame.arg_count + idx;
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

pub fn swapTop(self: *Context, val: Val) Vm.Error!void {
    const len = self.stack.items.len;
    if (len == 0) return Vm.Error.UndefinedBehavior;
    self.stack.items[len - 1] = val;
}

pub fn stackSquash(self: *Context, n: u32) Vm.Error!void {
    if (n < 2 or self.stack.items.len == 0)
        return Vm.Error.UndefinedBehavior;
    const top_idx = self.stack.items.len - 1;
    const bottom_idx = self.stack.items.len - @as(usize, @intCast(n));
    self.stack.items[bottom_idx] = self.stack.items[top_idx];
    self.stack.shrinkRetainingCapacity(bottom_idx + 1);
}
