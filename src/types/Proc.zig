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
/// The number of arguments. If `has_rest_args` is set, then the final arg
/// should capture the rest of the args as a single list.
arg_count: u32,
locals_count: u32,
captures_count: u32,
has_rest_args: bool,

pub fn deinit(self: *Proc, allocator: std.mem.Allocator) void {
    allocator.free(self.instructions);
    self.instructions = &.{};
    allocator.free(self.constants);
    self.constants = &.{};
}
