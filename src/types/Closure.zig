const std = @import("std");

const Instruction = @import("../instruction.zig").Instruction;
const Handle = @import("object_pool.zig").Handle;
const Proc = @import("Proc.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");

const Closure = @This();

instructions: []const Instruction,
captures: []const Val,
name: Symbol,
arg_count: u32,
locals_count: u32,

pub fn deinit(self: *Closure, allocator: std.mem.Allocator) void {
    allocator.free(self.instructions);
    self.instructions = &.{};
    allocator.free(self.captures);
    self.captures = &.{};
}
