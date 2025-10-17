const std = @import("std");
const testing = std.testing;

const Diagnostics = @import("../Diagnostics.zig");
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

fn toVal(self: Instruction, vm: *Vm) Vm.Error!Val {
    const builder = vm.builder();
    switch (self) {
        .load_const => |idx| {
            const items = [_]Val{
                try builder.makeStaticSymbol("load-const"),
                Val.initInt(idx),
            };
            return builder.makeList(&items);
        },
        .load_global => |slot| {
            const items = [_]Val{
                try builder.makeStaticSymbol("load-global"),
                Val.initInt(slot.idx),
            };
            return builder.makeList(&items);
        },
        .load_proc => {
            const items = [_]Val{
                try builder.makeStaticSymbol("load-proc"),
            };
            return builder.makeList(&items);
        },
        .load_arg => |idx| {
            const items = [_]Val{
                try builder.makeStaticSymbol("load-arg"),
                Val.initInt(@intCast(idx)),
            };
            return builder.makeList(&items);
        },
        .load_local => |idx| {
            const items = [_]Val{
                try builder.makeStaticSymbol("load-local"),
                Val.initInt(@intCast(idx)),
            };
            return builder.makeList(&items);
        },
        .set_global => |slot| {
            @branchHint(.cold);
            const items = [_]Val{
                try builder.makeStaticSymbol("set-global"),
                Val.initInt(@intCast(slot.idx)),
            };
            return builder.makeList(&items);
        },
        .set_arg => |idx| {
            const items = [_]Val{
                try builder.makeStaticSymbol("set-arg"),
                Val.initInt(@intCast(idx)),
            };
            return builder.makeList(&items);
        },
        .set_local => |idx| {
            const items = [_]Val{
                try builder.makeStaticSymbol("set-local"),
                Val.initInt(@intCast(idx)),
            };
            return builder.makeList(&items);
        },
        .jump => |n| {
            const items = [_]Val{
                try builder.makeStaticSymbol("jump"),
                Val.initInt(n),
            };
            return builder.makeList(&items);
        },
        .jump_if_not => |n| {
            const items = [_]Val{
                try builder.makeStaticSymbol("jump-if-not"),
                Val.initInt(n),
            };
            return builder.makeList(&items);
        },
        .squash => |n| {
            const items = [_]Val{
                try builder.makeStaticSymbol("squash"),
                Val.initInt(@intCast(n)),
            };
            return builder.makeList(&items);
        },
        .eval => |n| {
            const items = [_]Val{
                try builder.makeStaticSymbol("eval"),
                Val.initInt(@intCast(n)),
            };
            return builder.makeList(&items);
        },
        .make_closure => |h| {
            @branchHint(.cold);
            const items = [_]Val{
                try builder.makeStaticSymbol("make-closure"),
                Val.initProc(h),
            };
            return builder.makeList(&items);
        },
        .ret => {
            @branchHint(.unlikely);
            const items = [_]Val{
                try builder.makeStaticSymbol("ret"),
            };
            return builder.makeList(&items);
        },
    }
}

const proc_instructions = NativeProc.withRawArgs(struct {
    pub const name = "proc-instructions";
    pub inline fn impl(vm: *Vm, diagnostics: ?*Diagnostics, args: []const Val) Vm.Error!Val {
        if (args.len != 1) {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_count = .{
                    .expected = 1,
                    .got = @intCast(args.len),
                    .proc = Val.initNativeProc(&proc_instructions),
                } });
            }
            return Vm.Error.UncaughtException;
        }
        const raw_instructions = switch (args[0].data) {
            .proc => |h| blk: {
                const proc = vm.objects.procs.get(h) orelse {
                    if (diagnostics) |d| {
                        d.addDiagnostic(.{ .undefined_behavior = "Invalid procedure handle in proc-instructions" });
                    }
                    return error.UndefinedBehavior;
                };
                break :blk proc.instructions;
            },
            else => {
                if (diagnostics) |d| {
                    d.addDiagnostic(.{ .wrong_arg_type = .{
                        .expected = "procedure",
                        .got = args[0],
                        .proc = Val.initNativeProc(&proc_instructions),
                        .arg_name = "proc",
                        .arg_position = 0,
                    } });
                }
                return Vm.Error.UncaughtException;
            },
        };
        const instructions = try vm.allocator().alloc(Val, raw_instructions.len);
        defer vm.allocator().free(instructions);
        for (instructions, raw_instructions) |*dst, src| {
            dst.* = try toVal(src, vm);
        }
        const list = try vm.builder().makeList(instructions);
        return list;
    }
});

test "proc-instructions reveals bytecode" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval(
        "((load-const 0) (jump-if-not 2) (load-arg 0) (ret) (load-const 1) (ret))",
        \\ (define (test-fn n) (if #t n 0))
        \\ (import (sizzle unstable compiler))
        \\ (proc-instructions test-fn)
        ,
    );
}
