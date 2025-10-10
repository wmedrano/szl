const std = @import("std");

const Instruction = @import("../instruction.zig").Instruction;
const Symbol = @import("Symbol.zig");

const Proc = @This();

instructions: []const Instruction,
name: Symbol,
arg_count: u32,
locals_count: u32,
captures_count: u32,

pub fn deinit(self: *Proc, allocator: std.mem.Allocator) void {
    allocator.free(self.instructions);
    self.instructions = &.{};
}
