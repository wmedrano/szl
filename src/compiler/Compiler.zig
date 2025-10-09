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
    proc: ?Symbol.Interned = null,
    args: []const Symbol.Interned = &.{},
    locals: std.ArrayList(Local) = .{},
    captures: Captures = .{},
    captures_count: u32 = 0,

    pub const Local = struct {
        name: Symbol.Interned,
        available: bool,
    };

    /// A map from captured to symbol to its captured index. If captured index
    /// is `null`, then the variable has not been captured.
    pub const Captures = std.AutoHashMapUnmanaged(Symbol.Interned, ?u32);

    pub const Location = union(enum) {
        arg: i32,
        local: u32,
        capture: u32,
        module: struct { module: Handle(Module), name: Symbol.Interned },
    };

    pub fn resolve(self: *Scope, name: Symbol.Interned) Location {
        // Locals
        var local_idx = self.locals.items.len;
        while (local_idx > 0) {
            local_idx -= 1;
            const item = self.locals.items[local_idx];
            if (item.available and item.name.eq(name))
                return Location{ .local = @intCast(local_idx) };
        }
        // Args
        for (self.args, 0..self.args.len) |arg, idx| {
            if (arg.eq(name)) return Location{ .arg = @intCast(idx) };
        }
        // Proc
        if (self.proc) |proc_name| {
            if (proc_name.eq(name)) return Location{ .arg = -1 };
        }
        // Captures
        if (self.captures.getEntry(name)) |capture| {
            if (capture.value_ptr.*) |idx| return Location{ .capture = idx };
            const idx = self.captures_count;
            capture.value_ptr.* = self.captures_count;
            self.captures_count += 1;
            return Location{ .capture = idx };
        }
        return Location{ .module = .{ .module = self.module, .name = name } };
    }

    pub fn toCaptureCandidates(self: Scope, allocator: std.mem.Allocator) error{OutOfMemory}!Captures {
        var ret = Captures{};
        for (self.args) |arg| try ret.put(allocator, arg, null);
        for (self.locals.items) |local|
            if (local.available) try ret.put(allocator, local.name, null);
        return ret;
    }

    pub fn capturesSlice(self: Scope, allocator: std.mem.Allocator) error{OutOfMemory}![]Symbol.Interned {
        const ret = try allocator.alloc(Symbol.Interned, @intCast(self.captures_count));
        var captures_iter = self.captures.iterator();
        while (captures_iter.next()) |capture| {
            if (capture.value_ptr.*) |idx| {
                ret[@intCast(idx)] = capture.key_ptr.*;
            }
        }
        return ret;
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
    // Val -> Ir
    const ir = try Ir.init(self.arena, self.vm, expr);
    try self.addIr(ir, true);
    // Ir -> Val(Proc)
    const proc = try self.makeProc(null);
    if (proc.captures.len > 0) return Error.UndefinedBehavior;
    return Val.initProc(proc.handle);
}

fn addIr(self: *Compiler, ir: Ir, return_value: bool) Error!void {
    switch (ir) {
        .push_const => |v| try self.addInstruction(.{ .push_const = v }),
        .get => |sym| try self.addGet(sym),
        .define => |d| try self.addDefine(d.symbol, d.expr.*),
        .if_expr => |expr| try self.addIf(expr.test_expr.*, expr.true_expr.*, expr.false_expr.*, return_value),
        .let_expr => |expr| try self.addLet(expr.bindings, expr.body, return_value),
        .eval => |e| {
            try self.addIr(e.proc.*, false);
            for (e.args) |arg| try self.addIr(arg, false);
            try self.addInstruction(.{ .eval = @intCast(e.args.len) });
        },
        .lambda => |l| try self.addLambda(l),
    }
    if (return_value) try self.addInstruction(.{ .ret = {} });
}

fn addIrs(self: *Compiler, irs: []const Ir, return_value: bool) Error!void {
    if (irs.len == 0)
        return self.addIr(Ir{ .push_const = Val.initEmptyList() }, true);
    for (irs) |ir| try self.addIr(ir, false);
    if (return_value)
        try self.addInstruction(.{ .ret = {} })
    else if (irs.len > 1)
        try self.addInstruction(.{ .squash = @intCast(irs.len) });
}

fn addGet(self: *Compiler, sym: Symbol.Interned) Error!void {
    const instruction = switch (self.scope.resolve(sym)) {
        .arg => |idx| Instruction{ .get_arg = idx },
        .local => |idx| Instruction{ .get_local = idx },
        .capture => |idx| Instruction{ .get_capture = idx },
        .module => |m| Instruction{ .get_global = .{ .module = m.module, .symbol = m.name } },
    };
    try self.addInstruction(instruction);
}

fn addInstruction(self: *Compiler, instruction: Instruction) !void {
    try self.instructions.append(self.arena.allocator(), instruction);
}

fn addLambda(self: *Compiler, lambda: Ir.Lambda) Error!void {
    var sub_compiler = Compiler{
        .vm = self.vm,
        .arena = self.arena,
        .scope = Scope{
            .module = self.scope.module,
            .args = lambda.args,
            .captures = try self.scope.toCaptureCandidates(self.arena.allocator()),
        },
    };
    try sub_compiler.addIrs(lambda.body, true);
    const proc = try sub_compiler.makeProc(lambda.name);
    if (proc.captures.len == 0) {
        return self.addInstruction(.{ .push_const = Val.initProc(proc.handle) });
    }
    for (proc.captures) |capture| try self.addGet(capture);
    try self.addInstruction(.{ .make_closure = .{ .proc = proc.handle, .capture_count = proc.proc.captures_count } });
}

fn jumpDistance(src: usize, dst: usize) i32 {
    const dst_i32: i32 = @intCast(dst);
    const src_i32: i32 = @intCast(src);
    return dst_i32 - src_i32;
}

fn addDefine(self: *Compiler, symbol: Symbol.Interned, expr: Ir) Error!void {
    try self.addIr(expr, false);
    try self.addInstruction(.{ .set_global = .{ .module = self.scope.module, .symbol = symbol } });
}

fn addIf(self: *Compiler, test_expr: Ir, true_expr: Ir, false_expr: Ir, return_value: bool) !void {
    // 1. Add expressions.
    try self.addIr(test_expr, false);
    const test_jump_idx = self.instructions.items.len;
    try self.addInstruction(.{ .jump_if_not = 0 });
    try self.addIr(true_expr, return_value);
    const true_jump_idx: ?usize = if (return_value) null else self.instructions.items.len;
    if (!return_value)
        try self.addInstruction(.{ .jump = 0 });
    const false_start_idx = self.instructions.items.len;
    try self.addIr(false_expr, return_value);
    const end_idx = self.instructions.items.len;
    // 2. Fix jump indices. The start index is after the start of the jump since
    //    the counter is always advanced once the instruction is fetched, but
    //    before it is executed.
    self.instructions.items[test_jump_idx] =
        Instruction{ .jump_if_not = jumpDistance(test_jump_idx + 1, false_start_idx) };
    if (true_jump_idx) |idx| {
        self.instructions.items[idx] =
            Instruction{ .jump = jumpDistance(idx + 1, end_idx) };
    }
}

fn addLet(self: *Compiler, bindings: []const Ir.LetBinding, body: []Ir, return_value: bool) !void {
    // 1. Reserve bindings.
    const start_idx = self.scope.locals.items.len;
    const end_idx = self.scope.locals.items.len + bindings.len;
    try self.scope.locals.ensureUnusedCapacity(self.arena.allocator(), bindings.len);
    for (bindings) |b| {
        try self.scope.locals.appendBounded(Scope.Local{ .name = b.name, .available = false });
    }
    // 2. Compute values.
    for (bindings, 0..bindings.len) |b, idx| {
        try self.addIr(b.expr, false);
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
    try self.addIrs(body, return_value);
}

fn makeProc(self: Compiler, name: ?Symbol.Interned) Error!struct {
    handle: Handle(Proc),
    proc: Proc,
    captures: []Symbol.Interned,
} {
    const proc_name = name orelse try self.vm.builder().makeSymbolInterned(Symbol.init("_"));
    const captures = try self.scope.capturesSlice(self.arena.allocator());
    var proc = Proc{
        .name = proc_name,
        .instructions = try self.vm.allocator().dupe(Instruction, self.instructions.items),
        .arg_count = @intCast(self.scope.args.len),
        .locals_count = @intCast(self.scope.locals.items.len),
        .captures_count = @intCast(captures.len),
    };
    errdefer proc.deinit(self.vm.allocator());
    const handle = try self.vm.objects.procs.put(self.vm.allocator(), proc);
    return .{ .handle = handle, .proc = proc, .captures = captures };
}

test "define value can be referenced" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initInt(20),
        try vm.evalStr("(define x 20) x"),
    );
}

test "multiple defines uses latest" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(Val.initInt(20), try vm.evalStr("(define x 20)"));
    try testing.expectEqual(Val.initInt(20), try vm.evalStr("x"));
    try testing.expectEqual(Val.initInt(30), try vm.evalStr("(define x 30)"));
    try testing.expectEqual(Val.initInt(30), try vm.evalStr("x"));
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

test "let bindings can't reference themselves" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.UndefinedBehavior,
        vm.evalStr("(let ((x 1) (y x)) (+ x y))"),
    );
}

test "let variable shadows global" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initInt(101),
        try vm.evalStr("(define x 20) (let ((x 100)) (+ x 1))"),
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

test "lambda without body is empty list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initEmptyList(),
        try vm.evalStr("((lambda ()))"),
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

test "lambda can capture environment" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const source =
        \\ (let ((x 10))
        \\   (let ((proc (lambda (y) (+ x y))))
        \\     (proc 100)))
    ;
    try testing.expectEqual(
        Val.initInt(110),
        try vm.evalStr(source),
    );
}

test "define with args creates callable procedure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initInt(7),
        try vm.evalStr("(define (add a b) (+ a b)) (add 3 4)"),
    );
}

test "define without args create callable thunk" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initInt(42),
        try vm.evalStr("(define (get-answer) 42) (get-answer)"),
    );
}

test "define can call recursively" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const source =
        \\ (define (fib n)
        \\   (if (<= n 1)
        \\       n
        \\       (+ (fib (+ n -1)) (fib (+ n -2)))))
        \\ (fib 10)
    ;
    try testing.expectEqual(Val.initInt(55), try vm.evalStr(source));
}

test "call/cc that doesn't call continuation returns as normal" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try testing.expectEqual(
        Val.initInt(2),
        try vm.evalStr("(call/cc (lambda (k) 1 2))"),
    );
}

test "call/cc that calls continuation returns specified value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initInt(1),
        // The final expression is an error. We never call it so the vm is ok.
        try vm.evalStr("(call/cc (lambda (k) (k 1) (+ #f) 2))"),
    );
}
