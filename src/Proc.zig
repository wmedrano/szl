const std = @import("std");

const Instruction = @import("instruction.zig").Instruction;
const Symbol = @import("Symbol.zig");

const Proc = @This();

instructions: []const Instruction,
name: Symbol.Interned,

pub fn deinit(self: *Proc, allocator: std.mem.Allocator) void {
    allocator.free(self.instructions);
    self.instructions = &.{};
}

pub const Builtin = union(enum) {
    add,

    pub fn name(self: Builtin) []const u8 {
        return switch (self) {
            .add => "+",
        };
    }
};
