const std = @import("std");
const testing = std.testing;

const Instruction = @import("../instruction.zig").Instruction;
const Module = @import("../types/Module.zig");
const NativeProc = @import("../types/NativeProc.zig");
const Handle = @import("../types/object_pool.zig").Handle;
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Builder = @import("../utils/Builder.zig");
const Vm = @import("../Vm.zig");

pub fn init(vm: *Vm) Vm.Error!Handle(Module) {
    const b = vm.builder();

    const handle = try b.makeEnvironment(&.{
        (try b.makeStaticSymbolHandle("sizzle")),
        (try b.makeStaticSymbolHandle("unstable")),
        (try b.makeStaticSymbolHandle("compiler")),
    }, &[_]Builder.Definition{
        .{
            .symbol = (try b.makeStaticSymbolHandle("proc-instructions")),
            .value = Val.initNativeProc(&proc_instructions),
        },
    });

    return handle;
}

const proc_instructions = NativeProc.withRawArgs(struct {
    pub const name = "proc-instructions";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 1) {
            return NativeProc.Result{
                .wrong_arg_count = .{ .expected = 1, .got = @intCast(args.len) },
            };
        }
        const raw_instructions = switch (args[0].data) {
            .proc => |h| blk: {
                const proc = vm.objects.procs.get(h) orelse return .{ .err = error.UndefinedBehavior };
                break :blk proc.instructions;
            },
            else => return .{ .err = error.NotImplemented },
        };
        const instructions = vm.allocator().alloc(Val, raw_instructions.len) catch |e| return .{ .err = e };
        defer vm.allocator().free(instructions);
        for (instructions, raw_instructions) |*dst, src| {
            dst.* = src.toVal(vm) catch |e| return .{ .err = e };
        }
        const list = vm.builder().makeList(instructions) catch |e| return .{ .err = e };
        return .{ .val = list };
    }
});

test "proc-instructions reveals bytecode" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval(
        "((load-arg 0) (load-const 0) (load-global <=) (eval 2) (jump-if-not 2) (load-arg 0) (ret) (load-arg 0) (load-const 1) (load-global -) (eval 2) (load-proc) (eval 1) (load-arg 0) (load-const 2) (load-global -) (eval 2) (load-proc) (eval 1) (load-global +) (eval 2) (ret))",
        \\ (define (fib n) (if (<= n 1)
        \\                   n
        \\                   (+ (fib (- n 1))
        \\                      (fib (- n 2)))))
        \\ (import (sizzle unstable compiler))
        \\ (proc-instructions fib)
        ,
    );
}
