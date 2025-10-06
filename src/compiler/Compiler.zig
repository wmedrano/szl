const std = @import("std");
const testing = std.testing;

const Builder = @import("../utils/Builder.zig");
const Handle = @import("../types/object_pool.zig").Handle;
const Inspector = @import("../utils/Inspector.zig");
const Instruction = @import("../instruction.zig").Instruction;
const Ir = @import("Ir.zig");
const Module = @import("../types/Module.zig");
const ObjectPool = @import("../types/object_pool.zig").ObjectPool;
const Pair = @import("../types/Pair.zig");
const PrettyPrinter = @import("../utils/PrettyPrinter.zig");
const Proc = @import("../types/Proc.zig");
const Reader = @import("Reader.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

const Compiler = @This();

vm: *Vm,
arena: *std.heap.ArenaAllocator,
module: Handle(Module),
instructions: std.ArrayList(Instruction) = .{},

pub const Error = error{
    InvalidExpression,
    OutOfMemory,
    NotImplemented,
    UndefinedBehavior,
};

pub fn init(arena: *std.heap.ArenaAllocator, vm: *Vm, module: Handle(Module)) Compiler {
    return Compiler{
        .vm = vm,
        .arena = arena,
        .module = module,
    };
}

pub fn compile(self: *Compiler, expr: Val) Error!Val {
    // Build IR
    var ir_builder = Ir.Builder.init(self.arena, self.vm);
    const ir = try ir_builder.build(expr);
    try self.addIr(ir);
    // Build procedure
    const proc = try self.makeProc(0);
    return proc.val;
}

fn addIr(self: *Compiler, ir: Ir) Error!void {
    switch (ir.kind) {
        .push_const => |v| try self.addInstruction(.{ .push_const = v }),
        .get => |sym| try self.addInstruction(.{
            .module_get = .{ .module = self.module, .symbol = sym },
        }),
        .get_arg => |idx| try self.addInstruction(.{ .get_arg = idx }),
        .if_expr => |expr| try self.addIf(expr.test_expr.*, expr.true_expr.*, expr.false_expr.*),
        .eval => |e| {
            try self.addIr(e.proc.*);
            for (e.args) |arg| try self.addIr(arg);
            try self.addInstruction(.{ .eval = @intCast(e.args.len) });
        },
        .lambda => |l| try self.addInstruction(
            Instruction{ .push_const = try self.addLambda(l) },
        ),
        .ret => try self.addInstruction(.{ .ret = {} }),
    }
}

fn addInstruction(self: *Compiler, instruction: Instruction) !void {
    try self.instructions.append(self.arena.allocator(), instruction);
}

fn addLambda(self: *Compiler, lambda: Ir.Lambda) Error!Val {
    var sub_compiler = Compiler{
        .vm = self.vm,
        .arena = self.arena,
        .module = self.module,
    };
    if (lambda.body.len == 0)
        try sub_compiler.addIr(Ir{ .kind = .{ .push_const = Val.initEmptyList() } });
    for (lambda.body) |ir|
        try sub_compiler.addIr(ir);
    const proc = try sub_compiler.makeProc(lambda.arg_count);
    return proc.val;
}

fn jumpDistance(src: usize, dst: usize) i32 {
    const dst_i32: i32 = @intCast(dst);
    const src_i32: i32 = @intCast(src);
    return dst_i32 - src_i32;
}

fn addIf(self: *Compiler, test_expr: Ir, true_expr: Ir, false_expr: Ir) !void {
    // 1. Add expressions.
    try self.addIr(test_expr);
    const test_jump_idx = self.instructions.items.len;
    try self.addInstruction(.{ .jump_if_not = 0 });
    try self.addIr(true_expr);
    const true_jump_idx = self.instructions.items.len;
    try self.addInstruction(.{ .jump = 0 });
    const false_start_idx = self.instructions.items.len;
    try self.addIr(false_expr);
    const end_idx = self.instructions.items.len;
    // 2. Fix jump indices. The start index is after the start of the jump since
    //    the counter is always advanced once the instruction is fetched, but
    //    before it is executed.
    self.instructions.items[test_jump_idx] =
        Instruction{ .jump_if_not = jumpDistance(test_jump_idx + 1, false_start_idx) };
    self.instructions.items[true_jump_idx] =
        Instruction{ .jump = jumpDistance(true_jump_idx + 1, end_idx) };
}

pub fn makeProc(self: Compiler, arg_count: u32) Error!struct { val: Val, proc: Proc } {
    const builder = self.vm.builder();
    const name = try builder.makeSymbolInterned(Symbol.init("_"));
    var proc = Proc{
        .name = name,
        .instructions = try self.vm.allocator().dupe(Instruction, self.instructions.items),
        .arg_count = arg_count,
    };
    errdefer proc.deinit(self.vm.allocator());
    const handle = try self.vm.objects.procs.put(self.vm.allocator(), proc);
    const val = Val{ .data = .{ .proc = handle } };
    return .{ .val = val, .proc = proc };
}

test "if statement picks correct branch" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initInt(10),
        try vm.evalStr("(if #t 10 20)"),
    );
    try testing.expectEqual(
        Val.initInt(20),
        try vm.evalStr("(if #f 10 20)"),
    );
}

test "lambda is evaluated" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initInt(10),
        try vm.evalStr("((lambda () (+ 1 2 3 4)))"),
    );
}

test "lambda with args is evaluated" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initInt(10),
        try vm.evalStr("((lambda (a b c d) (+ a b c d)) 1 2 3 4)"),
    );
}

test "lambda with wrong number of args is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.NotImplemented,
        vm.evalStr("((lambda (a b c d) (+ a b c d)) 1 2 3)"),
    );
}
