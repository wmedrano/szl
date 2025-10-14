const std = @import("std");

const Instruction = @import("../instruction.zig").Instruction;
const Handle = @import("object_pool.zig").Handle;
const Module = @import("Module.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");

const Proc = @This();

instructions: []const Instruction,
constants: []const Val,
module: Handle(Module),
name: Symbol,
arg_count: u32,
locals_count: u32,
captures_count: u32,

pub fn deinit(self: *Proc, allocator: std.mem.Allocator) void {
    allocator.free(self.instructions);
    self.instructions = &.{};
    allocator.free(self.constants);
    self.constants = &.{};
}
