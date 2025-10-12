const std = @import("std");
const testing = std.testing;

const Instruction = @import("../instruction.zig").Instruction;
const Vm = @import("../Vm.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");

const NativeProc = @This();

name: []const u8,
docstring: []const u8,
unsafe_impl: *const fn (*Vm, arg_count: u32) Vm.Error!void,

pub const Result = union(enum) {
    val: Val,
    err: Vm.Error,
};

pub fn withRawArgs(T: type) NativeProc {
    return struct {
        const def = NativeProc{
            .name = T.name,
            .unsafe_impl = &wrapped,
            .docstring = if (@hasDecl(T, "docstring")) T.docstring else "",
        };

        fn wrapped(vm: *Vm, arg_count: u32) Vm.Error!void {
            const args = vm.context.stackTopN(arg_count);
            const result = T.impl(vm, args);
            const val = switch (result) {
                .val => |v| v,
                .err => |e| return e,
            };
            vm.context.popMany(arg_count);
            try vm.context.push(vm.allocator(), val);
        }
    }.def;
}
