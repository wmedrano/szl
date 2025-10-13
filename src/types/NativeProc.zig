const std = @import("std");
const testing = std.testing;

const Diagnostics = @import("../Diagnostics.zig");
const Instruction = @import("../instruction.zig").Instruction;
const Vm = @import("../Vm.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");

const NativeProc = @This();

name: []const u8,
docstring: []const u8,
unsafe_impl: *const fn (*Vm, ?*Diagnostics, arg_count: u32) Vm.Error!void,

pub const Result = union(enum) {
    val: Val,
    err: Vm.Error,
    wrong_arg_count: struct {
        expected: u32,
        got: u32,
    },
    wrong_type: struct {
        expected: []const u8,
        got: Val,
        arg_name: ?[]const u8,
        arg_position: ?u32,
    },
};

pub fn withRawArgs(T: type) NativeProc {
    return struct {
        const def = NativeProc{
            .name = T.name,
            .unsafe_impl = &wrapped,
            .docstring = if (@hasDecl(T, "docstring")) T.docstring else "",
        };

        fn wrapped(vm: *Vm, diagnostics: ?*Diagnostics, arg_count: u32) Vm.Error!void {
            const args = vm.context.stackTopN(arg_count);
            const result = T.impl(vm, args);
            const val = switch (result) {
                .val => |v| v,
                .err => |e| return e,
                .wrong_arg_count => |e| {
                    if (diagnostics) |d| {
                        d.appendWrongArgCount(.{ .expected = e.expected, .got = e.got, .proc = Val.initNativeProc(&def) });
                    }
                    return Vm.Error.UncaughtException;
                },
                .wrong_type => |e| {
                    if (diagnostics) |d| {
                        d.appendWrongArgType(.{ .expected = e.expected, .proc = Val.initNativeProc(&def), .got = e.got, .arg_name = e.arg_name, .arg_position = e.arg_position });
                    }
                    return Vm.Error.UncaughtException;
                },
            };
            _ = vm.context.popStackFrame(.discard);
            try vm.context.push(vm.allocator(), val);
        }
    }.def;
}
