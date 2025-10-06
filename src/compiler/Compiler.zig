const std = @import("std");
const testing = std.testing;

const Instruction = @import("../instruction.zig").Instruction;
const Module = @import("../types/Module.zig");
const Handle = @import("../types/object_pool.zig").Handle;
const ObjectPool = @import("../types/object_pool.zig").ObjectPool;
const Pair = @import("../types/Pair.zig");
const Proc = @import("../types/Proc.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Builder = @import("../utils/Builder.zig");
const Inspector = @import("../utils/Inspector.zig");
const PrettyPrinter = @import("../utils/PrettyPrinter.zig");
const Vm = @import("../Vm.zig");
const Ir = @import("ir.zig").Ir;
const Reader = @import("Reader.zig");

const Compiler = @This();

vm: *Vm,
arena: *std.heap.ArenaAllocator,
scope: Scope,
instructions: std.ArrayList(Instruction) = .{},

pub const Error = error{
    InvalidExpression,
    OutOfMemory,
    NotImplemented,
    UndefinedBehavior,
};

pub const Scope = struct {
    module: Handle(Module),
    args: []const Symbol.Interned = &.{},
    locals: std.ArrayList(Local) = .{},

    pub const Local = struct {
        name: Symbol.Interned,
        available: bool,
    };

    pub const Location = union(enum) {
        arg: u32,
        local: u32,
        module: struct { module: Handle(Module), name: Symbol.Interned },
    };

    pub fn resolve(self: Scope, name: Symbol.Interned) Location {
        for (self.args, 0..self.args.len) |arg, idx| {
            if (arg.eq(name)) return Location{ .arg = @intCast(idx) };
        }
        var local_idx = self.locals.items.len;
        while (local_idx > 0) {
            local_idx -= 1;
            const item = self.locals.items[local_idx];
            if (item.available and item.name.eq(name))
                return Location{ .local = @intCast(local_idx) };
        }
        return Location{ .module = .{ .module = self.module, .name = name } };
    }
};

pub fn init(arena: *std.heap.ArenaAllocator, vm: *Vm, module: Handle(Module)) Compiler {
    return Compiler{
        .vm = vm,
        .arena = arena,
        .scope = Scope{ .module = module },
    };
}

pub fn compile(self: *Compiler, expr: Val) Error!Val {
    // Build IR
    const ir = try Ir.init(self.arena, self.vm, expr);
    try self.addIr(ir);
    // Build procedure
    const proc = try self.makeProc();
    return proc.val;
}

fn addIr(self: *Compiler, ir: Ir) Error!void {
    switch (ir) {
        .push_const => |v| try self.addInstruction(.{ .push_const = v }),
        .get => |sym| try self.addGet(sym),
        .if_expr => |expr| try self.addIf(expr.test_expr.*, expr.true_expr.*, expr.false_expr.*),
        .let_expr => |expr| try self.addLet(expr.bindings, expr.body),
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

fn addIrs(self: *Compiler, irs: []const Ir) Error!void {
    for (irs) |ir| try self.addIr(ir);
    if (irs.len == 0) try self.addIr(Ir{ .push_const = Val.initEmptyList() });
}

fn addGet(self: *Compiler, sym: Symbol.Interned) Error!void {
    const instruction = switch (self.scope.resolve(sym)) {
        .arg => |idx| Instruction{ .get_arg = idx },
        .local => |idx| Instruction{ .get_local = idx },
        .module => |m| Instruction{ .get_global = .{ .module = m.module, .symbol = m.name } },
    };
    try self.addInstruction(instruction);
}

fn addInstruction(self: *Compiler, instruction: Instruction) !void {
    try self.instructions.append(self.arena.allocator(), instruction);
}

fn addLambda(self: *Compiler, lambda: Ir.Lambda) Error!Val {
    var sub_compiler = Compiler{
        .vm = self.vm,
        .arena = self.arena,
        .scope = Scope{
            .module = self.scope.module,
            .args = lambda.args,
        },
    };
    if (lambda.body.len == 0)
        try sub_compiler.addIr(Ir{ .push_const = Val.initEmptyList() });
    for (lambda.body) |ir|
        try sub_compiler.addIr(ir);
    const proc = try sub_compiler.makeProc();
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

fn addLet(self: *Compiler, bindings: []const Ir.LetBinding, body: []Ir) !void {
    // 1. Reserve bindings.
    const start_idx = self.scope.locals.items.len;
    const end_idx = self.scope.locals.items.len + bindings.len;
    try self.scope.locals.ensureUnusedCapacity(self.arena.allocator(), bindings.len);
    for (bindings) |b| {
        try self.scope.locals.appendBounded(Scope.Local{ .name = b.name, .available = false });
    }
    // 2. Compute values.
    for (bindings, 0..bindings.len) |b, idx| {
        try self.addIr(b.expr);
        const local_idx = start_idx + idx;
        try self.addInstruction(Instruction{ .set_local = @intCast(local_idx) });
    }
    // 3. Activate Bindings
    for (self.scope.locals.items[start_idx..end_idx]) |*l|
        l.available = true;
    defer for (self.scope.locals.items[start_idx..end_idx]) |*l| {
        l.available = false;
    };
    // 4. Evaluate body.
    try self.addIrs(body);
}

pub fn makeProc(self: Compiler) Error!struct { val: Val, proc: Proc } {
    const builder = self.vm.builder();
    const name = try builder.makeSymbolInterned(Symbol.init("_"));
    var proc = Proc{
        .name = name,
        .instructions = try self.vm.allocator().dupe(Instruction, self.instructions.items),
        .arg_count = @intCast(self.scope.args.len),
        .locals_count = @intCast(self.scope.locals.items.len),
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

test "let is evaluated" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initInt(3),
        try vm.evalStr("(let ((x 1) (y 2)) (+ x y))"),
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
