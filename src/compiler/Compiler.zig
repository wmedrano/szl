const std = @import("std");
const testing = std.testing;

const Instruction = @import("../instruction.zig").Instruction;
const ErrorDetails = @import("../types/ErrorDetails.zig");
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
const Scope = @import("Scope.zig");

const Compiler = @This();

vm: *Vm,
arena: *std.heap.ArenaAllocator,
scope: Scope,
instructions: std.ArrayList(Instruction) = .{},
constants: std.ArrayList(Val) = .{},

pub const Error = error{
    InvalidExpression,
    OutOfMemory,
    NotImplemented,
    UndefinedBehavior,
    UncaughtException,
};

pub fn init(arena: *std.heap.ArenaAllocator, vm: *Vm, module: Handle(Module)) Compiler {
    return Compiler{
        .vm = vm,
        .arena = arena,
        .scope = Scope{ .module = module, .proc = null },
    };
}

pub fn compile(self: *Compiler, expr: Val, error_details: *ErrorDetails) Error!Val {
    // Val -> Ir
    const ir = try Ir.init(self.arena, self.vm, self.scope.module, expr, error_details);
    try self.addIr(ir, true, error_details);
    // Ir -> Val(Proc)
    const proc = try self.makeProc(null);
    if (proc.captures.len > 0) return Error.UndefinedBehavior;
    return Val.initProc(proc.handle);
}

fn addIr(self: *Compiler, ir: Ir, return_value: bool, error_details: *ErrorDetails) Error!void {
    switch (ir) {
        .push_const => |v| try self.addConst(v),
        .get => |sym| try self.addGet(sym, error_details),
        .define => |d| try self.addDefine(d.symbol, d.expr.*, error_details),
        .set => |d| try self.addSet(d.symbol, d.expr.*, error_details),
        .if_expr => |expr| return self.addIf(expr.test_expr.*, expr.true_expr.*, expr.false_expr.*, return_value, error_details),
        .let_expr => |expr| return self.addLet(expr.bindings, expr.body, return_value, error_details),
        .eval => |e| try self.addEval(e, error_details),
        .lambda => |l| try self.addLambda(l, error_details),
    }
    if (return_value) try self.addInstruction(.{ .ret = {} });
}

fn addIrs(self: *Compiler, irs: []const Ir, return_value: bool, error_details: *ErrorDetails) Error!void {
    switch (irs.len) {
        0 => return self.addIr(Ir{ .push_const = Val.initUnspecified() }, return_value, error_details),
        1 => return self.addIr(irs[0], return_value, error_details),
        else => {
            for (irs[0 .. irs.len - 1]) |ir| try self.addIr(ir, false, error_details);
            try self.addIr(irs[irs.len - 1], return_value, error_details);
            if (!return_value) {
                try self.addInstruction(.{ .squash = @intCast(irs.len) });
            }
        },
    }
}

fn addConst(self: *Compiler, val: Val) Error!void {
    const idx = self.constants.items.len;
    try self.constants.append(self.arena.allocator(), val);
    try self.addInstruction(.{ .load_const = @intCast(idx) });
}

fn addGet(self: *Compiler, sym: Symbol, error_details: *ErrorDetails) Error!void {
    const instruction = switch (self.scope.resolve(sym)) {
        .proc => Instruction{ .load_proc = {} },
        .arg => |idx| Instruction{ .load_arg = idx },
        .local => |idx| Instruction{ .load_local = idx },
        .capture => |idx| blk: {
            // Captures are encoded as negative indices during compilation,
            // then patched to positive indices in makeProc() after all constants
            // are collected. This allows captures to be stored in the constants
            // array alongside compile-time constants.
            // Formula: -idx - 1 becomes (constants.len + captures.len) - idx - 1
            // Example: capture 0 -> -1, capture 1 -> -2, etc.
            const signed_idx: i32 = @intCast(idx);
            break :blk Instruction{ .load_const = -signed_idx - 1 };
        },
        .module => |mod_sym| blk: {
            const module = try self.vm.inspector().handleToModule(self.scope.module);
            const slot = module.getSlot(mod_sym) orelse {
                @branchHint(.cold);
                error_details.addDiagnostic(self.vm.allocator(), .{
                    .undefined_variable = .{
                        .module = self.scope.module,
                        .symbol = mod_sym,
                    },
                });
                return Error.UncaughtException;
            };
            break :blk Instruction{ .load_global = slot };
        },
    };
    try self.addInstruction(instruction);
}

fn addEval(self: *Compiler, e: anytype, error_details: *ErrorDetails) Error!void {
    for (e.args) |arg| try self.addIr(arg, false, error_details);
    try self.addIr(e.proc.*, false, error_details);
    try self.addInstruction(.{ .eval = @intCast(e.args.len) });
}

fn addInstruction(self: *Compiler, instruction: Instruction) !void {
    try self.instructions.append(self.arena.allocator(), instruction);
}

fn addLambda(self: *Compiler, lambda: Ir.Lambda, error_details: *ErrorDetails) Error!void {
    var sub_compiler = Compiler{
        .vm = self.vm,
        .arena = self.arena,
        .scope = Scope{
            .module = self.scope.module,
            .proc = lambda.name,
            .args = lambda.args,
            .captures = try self.scope.toCaptureCandidates(self.arena.allocator()),
        },
    };
    try sub_compiler.addIrs(lambda.body, true, error_details);
    const proc = try sub_compiler.makeProc(lambda.name);
    if (proc.captures.len == 0) {
        return self.addConst(Val.initProc(proc.handle));
    }
    for (proc.captures) |capture| try self.addGet(capture, error_details);
    try self.addInstruction(.{ .make_closure = proc.handle });
}

fn jumpDistance(src: usize, dst: usize) i32 {
    const dst_i32: i32 = @intCast(dst);
    const src_i32: i32 = @intCast(src);
    return dst_i32 - src_i32;
}

fn addDefine(self: *Compiler, symbol: Symbol, expr: Ir, error_details: *ErrorDetails) Error!void {
    try self.addIr(expr, false, error_details);
    const module = try self.vm.inspector().handleToModule(self.scope.module);
    const slot = try module.getOrCreateSlot(self.vm.allocator(), symbol, Val.initBool(false));
    try self.addInstruction(.{ .set_global = slot });
}

fn addSet(self: *Compiler, symbol: Symbol, expr: Ir, error_details: *ErrorDetails) Error!void {
    try self.addIr(expr, false, error_details);
    switch (self.scope.resolve(symbol)) {
        .proc => return Error.UncaughtException,
        .arg => |idx| {
            // Set arg does not return anything so we make it an expression by
            // returning an unspecified value.
            try self.addInstruction(.{ .set_arg = idx });
            try self.addConst(Val.initUnspecified());
        },
        .local => |idx| {
            try self.addInstruction(.{ .set_local = idx });
            // Set local does not return anything so we make it an expression by
            // returning an unspecified value.
            try self.addConst(Val.initUnspecified());
        },
        .capture => |_| {
            @branchHint(.cold);
            error_details.addDiagnostic(self.vm.allocator(), .{ .unsupported_feature = .{
                .feature_name = "set! on captured variables",
                .hint = null,
            } });
            return Error.NotImplemented;
        },
        .module => |mod_sym| {
            const module = try self.vm.inspector().handleToModule(self.scope.module);
            const slot = module.getSlot(mod_sym) orelse {
                @branchHint(.cold);
                error_details.addDiagnostic(self.vm.allocator(), .{
                    .undefined_variable = .{
                        .module = self.scope.module,
                        .symbol = mod_sym,
                    },
                });
                return Error.UncaughtException;
            };
            try self.addInstruction(.{ .set_global = slot });
        },
    }
}

fn addIf(self: *Compiler, test_expr: Ir, true_expr: Ir, false_expr: Ir, return_value: bool, error_details: *ErrorDetails) !void {
    // 1. Add expressions.
    try self.addIr(test_expr, false, error_details);
    const test_jump_idx = self.instructions.items.len;
    try self.addInstruction(.{ .jump_if_not = 0 });
    try self.addIr(true_expr, return_value, error_details);
    const true_jump_idx: ?usize = if (return_value) null else self.instructions.items.len;
    if (!return_value)
        try self.addInstruction(.{ .jump = 0 });
    const false_start_idx = self.instructions.items.len;
    try self.addIr(false_expr, return_value, error_details);
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

fn addLet(self: *Compiler, bindings: []const Ir.LetBinding, body: []Ir, return_value: bool, error_details: *ErrorDetails) !void {
    // 1. Reserve bindings.
    const start_idx = self.scope.locals.items.len;
    const end_idx = self.scope.locals.items.len + bindings.len;
    try self.scope.locals.ensureUnusedCapacity(self.arena.allocator(), bindings.len);
    for (bindings) |b| {
        try self.scope.locals.appendBounded(Scope.Local{ .name = b.name, .available = false });
    }
    // 2. Compute values.
    for (bindings, 0..bindings.len) |b, idx| {
        try self.addIr(b.expr, false, error_details);
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
    try self.addIrs(body, return_value, error_details);
}

fn makeProc(self: Compiler, name: ?Symbol) Error!struct {
    handle: Handle(Proc),
    proc: Proc,
    captures: []Symbol,
} {
    const proc_name = name orelse try self.vm.builder().makeStaticSymbolHandle("_");
    const captures = try self.scope.capturesSlice(self.arena.allocator());

    // Patch negative capture indices to positive constants array indices.
    // Negative indices (-1, -2, ...) are temporary placeholders for captures
    // that get transformed to point to the end of the constants array.
    // Runtime check: Ensure constants array doesn't exceed i32 range (extremely unlikely).
    if (self.constants.items.len > std.math.maxInt(i32)) {
        return Error.InvalidExpression;
    }

    for (self.instructions.items) |*instruction| {
        switch (instruction.*) {
            .load_const => |idx| if (idx < 0) {
                const constants_count: i32 = @intCast(self.constants.items.len);
                instruction.* = Instruction{ .load_const = constants_count - idx - 1 };
            },
            else => {},
        }
    }
    var proc = Proc{
        .name = proc_name,
        .instructions = try self.vm.allocator().dupe(Instruction, self.instructions.items),
        .constants = try self.vm.allocator().dupe(Val, self.constants.items),
        .module = self.scope.module,
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

    try vm.expectEval("20", "(define x 20) x");
}

test "multiple defines uses latest" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#<unspecified>", "(define x 20)");
    try vm.expectEval("20", "x");
    try vm.expectEval("#<unspecified>", "(define x 30)");
    try vm.expectEval("30", "x");
}

test "if statement picks correct branch" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("10", "(if #t 10 20)");
    try vm.expectEval("20", "(if #f 10 20)");
}

test "let defines variable" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1", "(let ((y 1)) y)");
}

test "let is evaluated" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3", "(let ((x 1) (y 2)) (+ x y))");
}

test "let bindings can't reference themselves" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(error.UncaughtException, "(let ((x 1) (y x)) (+ x y))");
}

test "let variable shadows global" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("101", "(define x 20) (let ((x 100)) (+ x 1))");
}

test "lambda is evaluated" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("10", "((lambda () (+ 1 2 3 4)))");
}

test "lambda without body is empty list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#<unspecified>", "((lambda ()))");
}

test "lambda with args is evaluated" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("10", "((lambda (a b c d) (+ a b c d)) 1 2 3 4)");
}

test "lambda with wrong number of args is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(error.UncaughtException, "((lambda (a b c d) (+ a b c d)) 1 2 3)");
}

test "lambda can capture environment" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const source =
        \\ (let ((x 10))
        \\   (let ((proc (lambda (y) (+ x y))))
        \\     (proc 100)))
    ;
    try vm.expectEval("110", source);
}

test "define with args creates callable procedure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("7", "(define (add a b) (+ a b)) (add 3 4)");
}

test "define without args create callable thunk" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("42", "(define (get-answer) 42) (get-answer)");
}

test "define can call recursively" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const source =
        \\ (define (fib n)
        \\   (if (<= n 1)
        \\       n
        \\       (+ (fib (- n 1)) (fib (- n 2)))))
        \\ (fib 10)
    ;
    try vm.expectEval("55", source);
}

test "set! can set global variable" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const source =
        \\ (define x 1)
        \\ (let ((y (+ x x)))
        \\   (set! x y))
        \\ x
    ;
    try vm.expectEval("2", source);
}

test "set! can set local variable" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const source =
        \\ (define x 1)
        \\ (let ((y 10))
        \\   (set! y 100)
        \\   y)
    ;
    try vm.expectEval("100", source);
}

test "set! can set argument" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const source =
        \\ (define (add-100 x)
        \\   (set! x (+ 100 x))
        \\   x)
        \\ (add-100 1)
    ;
    try vm.expectEval("101", source);
}

test "set! on local doesn't affect original binding" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const source =
        \\ (define (do-not-add-100 x)
        \\   (let ((binding x))
        \\     (set! binding (+ binding x))
        \\     x))
        \\ (do-not-add-100 42)
    ;
    try vm.expectEval("42", source);
}

test "set! not available on captured binding" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const source =
        \\ (let ((x 10))
        \\   (lambda (y) (set! x 10) (+ x y)))
    ;
    try vm.expectError(error.NotImplemented, source);
}

test "set! requires symbol as first argument" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(error.InvalidExpression, "(set! 1 2)");
    try vm.expectError(error.InvalidExpression, "(set! \"x\" 2)");
    try vm.expectError(error.InvalidExpression, "(set! (quote x) 2)");
}

test "call/cc that doesn't call continuation returns as normal" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try vm.expectEval("2", "(call/cc (lambda (k) 1 2))");
}

test "call/cc that calls continuation returns specified value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // The final expression is an error. We never call it so the vm is ok.
    try vm.expectEval("1", "(call/cc (lambda (k) (k 1) (+ #f) 2))");
}
