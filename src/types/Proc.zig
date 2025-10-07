const std = @import("std");

const Instruction = @import("../instruction.zig").Instruction;
const Symbol = @import("Symbol.zig");

const Proc = @This();

instructions: []const Instruction,
name: Symbol.Interned,
arg_count: u32,
locals_count: u32,
captures_count: u32,

pub fn deinit(self: *Proc, allocator: std.mem.Allocator) void {
    allocator.free(self.instructions);
    self.instructions = &.{};
}

pub const Builtin = union(enum) {
    add,
    lte,

    pub fn name(self: Builtin) []const u8 {
        return switch (self) {
            .add => "+",
            .lte => "<=",
        };
    }
};
