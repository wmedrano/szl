//! A compiler that transforms Lisp expressions into bytecode instructions.
//!
//! The compiler builds a sequence of instructions that can be executed by the
//! virtual machine.
const std = @import("std");
const testing = std.testing;

const define_fn = @import("../builtins/define.zig").define_fn;
const Inspector = @import("../Inspector.zig");
const Operator = @import("../instruction.zig").Operator;
const Instruction = @import("../instruction.zig").Instruction;
const Proc = @import("../Proc.zig");
const Pair = @import("../types/Pair.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");
const IrBuilder = @import("IrBuilder.zig");
const Ir = IrBuilder.Ir;
const IfDef = IrBuilder.IfDef;
const LambdaDef = IrBuilder.LambdaDef;
const LetDef = IrBuilder.LetDef;
const Scope = @import("Scope.zig");

const Compiler = @This();

/// The virtual machine instance to compile for.
vm: *Vm,
/// The arena to allocate objects onto.
arena: *std.heap.ArenaAllocator,
/// Accumulated instructions during compilation.
instructions: std.ArrayList(Instruction) = .{},
/// Local bindings for managing local variable bindings during compilation.
scope: Scope = .{},
/// Compilation options that control compiler behavior.
options: Options = .{},

/// Compilation options that control compiler behavior.
pub const Options = struct {
    /// Whether to inline global variable values directly as constants when their
    /// values are known at compile time. This can improve performance by avoiding
    /// global lookups, but may cause issues if globals are redefined at runtime.
    inline_globals: bool = false,
};

/// Errors that can occur during compilation.
pub const Error = error{
    /// The expression cannot be compiled (e.g., nil values).
    InvalidExpression,
    /// Memory allocation failed.
    OutOfMemory,
};

/// Compiles a Lisp expression into a procedure containing bytecode instructions.
///
/// Args:
///   vm: The virtual machine instance to compile for.
///   expr: The expression to compile.
///
/// Returns:
///   A procedure value containing the compiled bytecode, or an error if compilation fails.
pub fn compile(vm: *Vm, arena: *std.heap.ArenaAllocator, expr: Val, options: Options) Error!Val {
    var compiler = Compiler{
        .vm = vm,
        .arena = arena,
        .options = options,
    };

    const ir_builder = IrBuilder.init(vm, arena);
    const ir = try ir_builder.build(expr);

    return compiler.compileIr(null, ir);
}

/// Compiles intermediate representation (IR) into a procedure value.
/// Converts IR nodes into bytecode instructions and creates a procedure containing them.
///
/// Args:
///   self: The compiler instance.
///   name: Optional name for the procedure (used for recursive calls).
///   ir: The IR to compile into bytecode.
///
/// Returns:
///   A procedure value containing the compiled bytecode.
///
/// Errors:
///   - OutOfMemory if allocation fails.
///   - InvalidExpression if the IR cannot be compiled.
fn compileIr(self: *Compiler, name: ?Symbol.Interned, ir: Ir) Error!Val {
    try self.addIr(ir);
    const stats = self.scope.stats();
    const proc = Proc{
        .name = name,
        .args = stats.args,
        .locals = stats.locals,
        .instructions = try self.vm.allocator.dupe(Instruction, self.instructions.items),
    };
    return try self.vm.builder().build(proc);
}

/// Adds an instruction to the compiler's instruction list.
/// Instructions are appended to maintain execution order.
///
/// Args:
///   self: The compiler instance.
///   instruction: The instruction to add.
///
/// Returns:
///   An error if memory allocation fails.
fn addInstruction(self: *Compiler, ins: Instruction) Error!void {
    try self.instructions.append(self.arena.allocator(), ins);
}

/// Generates instructions to retrieve a symbol's value.
/// Emits GetLocal if the symbol is bound locally, otherwise GetGlobal.
///
/// Args:
///   self: The compiler instance.
///   symbol: The symbol to retrieve.
///
/// Errors:
///   - OutOfMemory if instruction allocation fails.
fn get(self: *Compiler, symbol: Symbol.Interned) !void {
    if (self.scope.resolve(symbol)) |binding|
        return self.addInstruction(.{ .get_local = binding.index });
    if (self.options.inline_globals) {
        if (self.vm.inspector().get(symbol)) |val|
            return self.addInstruction(.{ .load = val });
    }
    try self.addInstruction(.{ .get_global = symbol });
}

/// Calculates the jump distance between two instruction positions.
/// Used for generating jump instructions with correct offsets.
///
/// Args:
///   start: The starting instruction index.
///   end: The ending instruction index.
///
/// Returns:
///   The signed distance between the positions.
fn jump(start: usize, end: usize) i32 {
    return @as(i32, @intCast(end)) - @as(i32, @intCast(start));
}

/// Compiles an if conditional IR node into bytecode instructions.
/// Generates conditional jumps and handles both true and false branches.
///
/// Args:
///   self: The compiler instance.
///   if_def: The if IR node containing test, true, and false expressions.
///
/// Errors:
///   - OutOfMemory if instruction allocation fails.
///   - InvalidExpression if the IR cannot be compiled.
fn addIf(self: *Compiler, if_def: IfDef) Error!void {
    try self.addIr(if_def.@"test".*);
    const test_jump_idx = self.instructions.items.len;
    try self.addInstruction(.{ .jump_if_not = .{ .pop = true, .steps = 0 } });
    try self.addIr(if_def.true.*);
    const true_jump_idx = self.instructions.items.len;
    try self.addInstruction(.{ .jump = 0 });
    try self.addIr(if_def.false.*);
    const end_idx = self.instructions.items.len;
    self.instructions.items[test_jump_idx] =
        .{ .jump_if_not = .{ .pop = true, .steps = jump(test_jump_idx, true_jump_idx) } };
    self.instructions.items[true_jump_idx] =
        .{ .jump = jump(true_jump_idx + 1, end_idx) };
}

/// Compiles a lambda (procedure definition) IR node into bytecode instructions.
/// Creates a sub-compiler with proper scope for the lambda's parameters and body.
///
/// Args:
///   self: The compiler instance.
///   lambda: The lambda IR node containing name, args, and body.
///
/// Errors:
///   - OutOfMemory if instruction allocation fails.
///   - InvalidExpression if the IR cannot be compiled.
fn addLambda(self: *Compiler, lambda: LambdaDef) Error!void {
    var sub_compiler = Compiler{ .vm = self.vm, .arena = self.arena };
    if (lambda.name) |name| {
        _ = try sub_compiler.scope.addBinding(
            self.arena.allocator(),
            Scope.Binding{ .name = name, .index = -1, .type = .proc },
        );
    }
    for (lambda.args, 0..lambda.args.len) |param, index| {
        _ = try sub_compiler.scope.addBinding(
            self.arena.allocator(),
            Scope.Binding{
                .name = param,
                .index = @intCast(index),
                .type = .argument,
            },
        );
    }
    const proc = try sub_compiler.compileIr(lambda.name, lambda.body.*);
    try self.addInstruction(.{ .load = proc });
}

/// Compiles a let binding construct IR node into bytecode instructions.
/// Handles both parallel (let) and sequential (let*) binding semantics.
///
/// Args:
///   self: The compiler instance.
///   let_def: The let IR node containing star flag, bindings, and body.
///
/// Errors:
///   - OutOfMemory if instruction allocation fails.
///   - InvalidExpression if the IR cannot be compiled.
fn addLet(self: *Compiler, let_def: LetDef) Error!void {
    var bindings = std.ArrayList(Scope.Id){};
    defer for (bindings.items) |binding| self.scope.clear(binding);
    const initial_state: Scope.Type = if (let_def.star) .local else .hidden;
    for (let_def.bindings) |binding| {
        const id = try self.scope.addLocal(self.arena.allocator(), binding.name, initial_state);
        try bindings.append(self.arena.allocator(), id);
        try self.addIr(binding.expr);
        try self.addInstruction(.{ .set_local = self.scope.getBinding(id).?.index });
    }
    if (!let_def.star) {
        for (bindings.items) |binding| {
            self.scope.getBinding(binding).?.type = .local;
        }
    }
    try self.addIr(let_def.body.*);
}

/// Compiles a procedure call IR node into bytecode instructions.
/// Handles both regular procedure calls and special cases like call-with-current-continuation.
///
/// Args:
///   self: The compiler instance.
///   proc_call: The procedure call IR node containing procedure and arguments.
///
/// Errors:
///   - OutOfMemory if instruction allocation fails.
///   - InvalidExpression if the IR cannot be compiled.
fn addProc(self: *Compiler, proc_call: IrBuilder.ProcCall) Error!void {
    // Special case for call-with-current-continuation
    switch (proc_call.proc.*) {
        .get => |sym| {
            const is_call_with_cc = self.vm.common_symbols.@"call-with-current-continuation".eql(sym) or
                self.vm.common_symbols.@"call/cc".eql(sym);
            if (is_call_with_cc) {
                if (proc_call.args.len != 1) return Error.InvalidExpression;
                try self.addIr(proc_call.args[0]);
                return self.addInstruction(.{ .call_operator = Operator.call_with_cc });
            }
        },
        else => {},
    }
    try self.addIr(proc_call.proc.*);
    for (proc_call.args) |arg| try self.addIr(arg);
    try self.addInstruction(.{ .eval_proc = @intCast(proc_call.args.len) });
}

/// Recursively adds IR nodes to the compiler as bytecode instructions.
/// This function handles all IR node types and converts them to instructions.
///
/// Args:
///   self: The compiler instance.
///   ir: The IR node to compile.
///
/// Errors:
///   - OutOfMemory if instruction allocation fails.
///   - InvalidExpression if the IR cannot be compiled.
fn addIr(self: *Compiler, ir: Ir) Error!void {
    switch (ir) {
        .const_val => |val| try self.addInstruction(.{ .load = val }),
        .get => |sym| try self.get(sym),
        .proc_call => |p| try self.addProc(p),
        .@"if" => |def| try self.addIf(def),
        .body => |body| {
            for (body) |expr| try self.addIr(expr);
            switch (body.len) {
                0 => try self.addInstruction(.{ .load = Val.init({}) }),
                1 => {},
                else => try self.addInstruction(.{ .squash = @intCast(body.len) }),
            }
        },
        .define => |def| {
            try self.addInstruction(.{ .load = Val.init(&define_fn) });
            try self.addInstruction(.{ .load = Val.init(def.symbol) });
            try self.addIr(def.expr.*);
            try self.addInstruction(.{ .eval_proc = 2 });
        },
        .lambda => |lambda| try self.addLambda(lambda),
        .let => |let_def| try self.addLet(let_def),
    }
}

/// Test helper for verifying compiled instruction sequences.
/// Compiles an expression and compares the resulting instructions to expected instructions.
///
/// Args:
///   expected_instructions: The expected instruction sequence.
///   vm: The virtual machine instance to use for compilation.
///   source: The Scheme source code to compile.
///
/// Errors:
///   - Any errors from compilation.
///   - Test failure if the instructions don't match the expected sequence.
pub fn expectInstructions(expected_instructions: []const Instruction, vm: *Vm, source: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(vm.allocator);
    defer arena.deinit();
    const expr = try vm.builder().readOne(source);
    const proc_val = try compile(vm, &arena, expr, .{});
    const proc = try vm.fromVal(Proc, proc_val);

    try testing.expectEqualDeep(
        expected_instructions,
        proc.instructions,
    );
}

test "Instruction is small" {
    try testing.expectEqual(24, @sizeOf(Instruction));
}

test "compile with expression is function call" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try expectInstructions(&[_]Instruction{
        .{ .get_global = try vm.builder().internStatic(Symbol.init("+")) },
        .{ .load = Val.init(1) },
        .{ .load = Val.init(2) },
        Instruction{ .eval_proc = 2 },
    }, &vm, "(+ 1 2)");
}

test "compile with nested expression is multiple function calls" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try expectInstructions(&[_]Instruction{
        .{ .get_global = try vm.builder().internStatic(Symbol.init("foo")) },
        .{ .load = Val.init(1) },
        .{ .load = Val.init(2) },
        .{ .get_global = try vm.builder().internStatic(Symbol.init("bar")) },
        .{ .load = Val.init(3) },
        .{ .load = Val.init(4) },
        Instruction{ .eval_proc = 2 },
        .{ .eval_proc = 3 },
    }, &vm, "(foo 1 2 (bar 3 4))");
}

test "compile define expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try expectInstructions(&[_]Instruction{
        Instruction{ .load = Val.init(&define_fn) },
        .{ .load = try vm.builder().internStaticVal(Symbol.init("x")) },
        .{ .load = Val.init(42) },
        Instruction{ .eval_proc = 2 },
    }, &vm, "(define x 42)");
}

test "compile define procedure expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(vm.allocator);
    defer arena.deinit();
    const expr = try vm.builder().readOne("(define (foo) 42)");
    const proc_val = try compile(&vm, &arena, expr, .{});
    const proc = try vm.fromVal(Proc, proc_val);

    const instructions = proc.instructions;

    try testing.expectEqual(4, instructions.len);
    try testing.expectEqual(
        Instruction{ .load = Val.init(&define_fn) },
        instructions[0],
    );
    try testing.expectEqual(
        Instruction{ .load = try vm.builder().internStaticVal(Symbol.init("foo")) },
        instructions[1],
    );
    try testing.expectEqual(
        .proc,
        std.meta.activeTag(instructions[2].load.repr),
    );
    try testing.expectEqual(
        Instruction{ .eval_proc = 2 },
        instructions[3],
    );
}

test "compile define procedure with recursive call" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(vm.allocator);
    defer arena.deinit();
    const expr = try vm.builder().readOne("(define (foo x) (foo x))");
    const proc = try vm.fromVal(Proc, try compile(&vm, &arena, expr, .{}));

    const instructions = proc.instructions;

    try testing.expectEqual(4, instructions.len);
    try testing.expectEqual(
        Instruction{ .load = Val.init(&define_fn) },
        instructions[0],
    );
    try testing.expectEqual(
        Instruction{ .load = try vm.builder().internStaticVal(Symbol.init("foo")) },
        instructions[1],
    );
    const foo_proc = try vm.fromVal(Proc, proc.instructions[2].load);
    try testing.expectEqualDeep(
        &[_]Instruction{
            .{ .get_local = -1 },
            .{ .get_local = 0 },
            .{ .eval_proc = 1 },
        },
        foo_proc.instructions,
    );
    try testing.expectEqual(
        Instruction{ .eval_proc = 2 },
        instructions[3],
    );
}

test "compile if expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    const plus = try vm.builder().internStatic(Symbol.init("+"));

    try expectInstructions(&[_]Instruction{
        .{ .load = Val.init(true) },
        .{ .jump_if_not = .{ .steps = 5 } },
        // True Branch
        .{ .get_global = plus },
        .{ .load = Val.init(1) },
        .{ .load = Val.init(2) },
        Instruction{ .eval_proc = 2 },
        .{ .jump = 6 },
        // False branch
        .{ .get_global = plus },
        .{ .load = Val.init(3) },
        .{ .load = Val.init(4) },
        .{ .load = Val.init(5) },
        .{ .load = Val.init(6) },
        .{ .eval_proc = 4 },
    }, &vm, "(if #t (+ 1 2) (+ 3 4 5 6))");
}

test "compile if expression with missing false branch uses nil" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    const plus = try vm.builder().internStatic(Symbol.init("+"));

    try expectInstructions(&[_]Instruction{
        .{ .load = Val.init(true) },
        .{ .jump_if_not = .{ .steps = 5 } },
        // True Branch
        .{ .get_global = plus },
        .{ .load = Val.init(1) },
        .{ .load = Val.init(2) },
        Instruction{ .eval_proc = 2 },
        .{ .jump = 1 },
        // False branch (nil)
        .{ .load = Val.init({}) },
    }, &vm, "(if #t (+ 1 2))");
}

test "if with true predicate evaluates true branch" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try vm.expectEval("10", "(if #t (+ 1 2 3 4) (+ 1 2))");
    try vm.expectEval("10", "(if #t (+ 1 2 3 4))");
}

test "if with false predicate evaluates false branch" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try vm.expectEval("10", "(if #t (+ 1 2 3 4) (+ 1 2))");
}

test "if with missing false branch uses nil also branch" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        .{ .load = Val.init(false) },
        .{ .jump_if_not = .{ .steps = 2 } },
        .{ .load = Val.init(42) },
        .{ .jump = 1 },
        .{ .load = Val.init({}) },
    }, &vm, "(if #f 42)");
}

test "compile quote with integer" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(vm.allocator);
    defer arena.deinit();

    const proc_val = try compile(&vm, &arena, try vm.builder().readOne("'42"), .{});
    const proc = try vm.fromVal(Proc, proc_val);

    try testing.expectEqual(1, proc.instructions.len);
    try testing.expectEqual(
        Instruction{ .load = Val.init(42) },
        proc.instructions[0],
    );
}

test "compile quote with symbol" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try expectInstructions(
        &[_]Instruction{
            Instruction{ .load = try vm.builder().internStaticVal(Symbol.init("foo")) },
        },
        &vm,
        "'foo",
    );
}

test "compile quote with boolean" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try expectInstructions(
        &[_]Instruction{.{ .load = Val.init(true) }},
        &vm,
        "'#t",
    );
}

test "compile quote with list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(vm.allocator);
    defer arena.deinit();

    const proc_val = try compile(&vm, &arena, try vm.builder().readOne("'(1 2 3)"), .{});
    const proc = try vm.fromVal(Proc, proc_val);

    try testing.expectEqual(1, proc.instructions.len);
    const instruction = proc.instructions[0];
    try testing.expectEqual(.load, std.meta.activeTag(instruction));

    const loaded_val = instruction.load;
    try testing.expectFmt(
        "(1 2 3)",
        "{f}",
        .{vm.pretty(loaded_val)},
    );
}

test "compile quote with empty list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try expectInstructions(
        &[_]Instruction{.{ .load = Val.init({}) }},
        &vm,
        "'()",
    );
}

test "compile quote with no arguments fails" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(vm.allocator);
    defer arena.deinit();

    try testing.expectError(
        Error.InvalidExpression,
        compile(&vm, &arena, try vm.builder().readOne("(quote)"), .{}),
    );
    try testing.expectError(
        error.BadExpression,
        vm.builder().readOne("'"),
    );
}

test "compile quote with multiple arguments fails" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(vm.allocator);
    defer arena.deinit();

    try testing.expectError(
        Error.InvalidExpression,
        compile(&vm, &arena, try vm.builder().readOne("(quote 1 2)"), .{}),
    );
}

test "compile begin with no arguments returns nil" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        .{ .load = Val.init({}) },
    }, &vm, "(begin)");
}

test "compile begin with single expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        .{ .load = Val.init(42) },
    }, &vm, "(begin 42)");
}

test "compile begin with multiple expressions" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        .{ .load = Val.init(1) },
        .{ .load = Val.init(2) },
        .{ .load = Val.init(3) },
        .{ .squash = 3 },
    }, &vm, "(begin 1 2 3)");
}

test "compile begin with function calls" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        .{ .get_global = try vm.builder().internStatic(Symbol.init("+")) },
        .{ .load = Val.init(1) },
        .{ .load = Val.init(2) },
        Instruction{ .eval_proc = 2 },
        .{ .get_global = try vm.builder().internStatic(Symbol.init("+")) },
        .{ .load = Val.init(3) },
        .{ .load = Val.init(4) },
        Instruction{ .eval_proc = 2 },
        .{ .squash = 2 },
    }, &vm, "(begin (+ 1 2) (+ 3 4))");
}

test "define" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try vm.expectEval("*unspecified*", "(define x 28)");
    try vm.expectEval("28", "x");
}

test "quote" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try vm.expectEval("a", "(quote a)");
    try vm.expectEval("(+ 1 2)", "(quote (+ 1 2))");
}

test "if" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("yes", "(if (> 3 2) 'yes 'no)");
    try vm.expectEval("no", "(if (> 2 3) 'yes 'no)");
    try vm.expectEval("1", "(if (> 3 2) (- 3 2) (+ 3 2))");
}

test "let" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEvalErr("undefined-variable", "(let ((x 1) (y x)) y)");
    try vm.expectEval("3", "(let ((x 1) (y 2)) (+ x y))");
    try vm.expectEval("x", "(let ((x 'x)) x)");
    try vm.expectEval("3", "(let ((x 2)) (let ((x 3)) x))");
    try vm.expectEval("5", "(let ((x 2) (y 3)) (+ x y))");
}

test "let*" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1", "(let* ((x 1) (y x)) y)");
    try vm.expectEval("3", "(let* ((x 1) (y 2)) (+ x y))");
    try vm.expectEval("x", "(let* ((x 'x)) x)");
    try vm.expectEval("3", "(let* ((x 2)) (let* ((x 3)) x))");
    try vm.expectEval("5", "(let* ((x 2) (y 3)) (+ x y))");
}

test "compile lambda with multiple parameters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(vm.allocator);
    defer arena.deinit();

    const expr = try vm.builder().readOne("(lambda (x y z) (+ x y z))");
    const proc_val = try compile(&vm, &arena, expr, .{});
    const proc = try vm.fromVal(Proc, proc_val);

    try testing.expectEqual(1, proc.instructions.len);
    try testing.expectEqual(.load, std.meta.activeTag(proc.instructions[0]));

    const lambda_proc = try vm.fromVal(Proc, proc.instructions[0].load);
    try testing.expectEqual(3, lambda_proc.args);
    try testing.expectEqual(0, lambda_proc.locals);
}

test "compile lambda with no parameters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(vm.allocator);
    defer arena.deinit();

    const expr = try vm.builder().readOne("(lambda () 42)");
    const proc_val = try compile(&vm, &arena, expr, .{});
    const proc = try vm.fromVal(Proc, proc_val);

    try testing.expectEqual(1, proc.instructions.len);
    try testing.expectEqual(.load, std.meta.activeTag(proc.instructions[0]));

    const lambda_proc = try vm.fromVal(Proc, proc.instructions[0].load);
    try testing.expectEqual(0, lambda_proc.args);
    try testing.expectEqualDeep(
        &[_]Instruction{.{ .load = Val.init(42) }},
        lambda_proc.instructions,
    );
}

test "compile nested lambdas" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(vm.allocator);
    defer arena.deinit();

    const expr = try vm.builder().readOne("(lambda (x) (lambda (y) (+ x y)))");
    const proc_val = try compile(&vm, &arena, expr, .{});
    const proc = try vm.fromVal(Proc, proc_val);

    const outer_lambda = try vm.fromVal(Proc, proc.instructions[0].load);
    try testing.expectEqual(1, outer_lambda.args);

    try testing.expectEqual(.load, std.meta.activeTag(outer_lambda.instructions[0]));
    const inner_lambda = try vm.fromVal(Proc, outer_lambda.instructions[0].load);
    try testing.expectEqual(1, inner_lambda.args);
}

test "compile empty let bindings" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        .{ .load = Val.init(42) },
    }, &vm, "(let () 42)");
}

test "compile let with multiple expressions in body" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const plus = try vm.builder().internStatic(Symbol.init("+"));
    const mult = try vm.builder().internStatic(Symbol.init("*"));
    try expectInstructions(&[_]Instruction{
        .{ .load = Val.init(1) },
        .{ .set_local = 0 },
        .{ .get_global = plus },
        .{ .get_local = 0 },
        .{ .load = Val.init(1) },
        Instruction{ .eval_proc = 2 },
        .{ .get_global = mult },
        .{ .get_local = 0 },
        .{ .load = Val.init(2) },
        Instruction{ .eval_proc = 2 },
        .{ .squash = 2 },
    }, &vm, "(let ((x 1)) (+ x 1) (* x 2))");
}

test "compile variable shadowing in nested let" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3", "(let ((x 1)) (let ((x 3)) x))");
    try vm.expectEval("1", "(let ((x 1)) (let ((y 3)) x))");
}

test "compile deeply nested if expressions" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("nested", "(if #t (if #t (if #t 'nested 'no) 'no) 'no)");
    try vm.expectEval("42", "(if #f 1 (if #t 42 99))");
}

test "compile call-with-current-continuation expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        .{ .get_global = try vm.builder().internStatic(Symbol.init("foo")) },
        .{ .call_operator = Operator.call_with_cc },
    }, &vm, "(call-with-current-continuation foo)");
}

test "call-with-current-continuation execution" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test that call-with-current-continuation works with a simple lambda
    try vm.expectEval("42", "(call-with-current-continuation (lambda (k) 42))");
}

test "call-with-current-continuation execution unwinds stack" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval(
        "43",
        "(call-with-current-continuation (lambda (k) (k 43) 42))",
    );
}

test "compile mixed special forms" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("2", "(let ((x 1)) (if x ((lambda () 2)) 3))");
    try vm.expectEval("5", "(if #t (let ((x 5)) x) (let ((y 10)) y))");
}

test "compile large argument list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const plus = try vm.builder().internStatic(Symbol.init("+"));
    try expectInstructions(&[_]Instruction{
        .{ .get_global = plus },
        .{ .load = Val.init(1) },
        .{ .load = Val.init(2) },
        .{ .load = Val.init(3) },
        .{ .load = Val.init(4) },
        .{ .load = Val.init(5) },
        .{ .load = Val.init(6) },
        .{ .load = Val.init(7) },
        .{ .load = Val.init(8) },
        .{ .eval_proc = 8 },
    }, &vm, "(+ 1 2 3 4 5 6 7 8)");
}

test "compile lambda with local variable access" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(vm.allocator);
    defer arena.deinit();
    const expr = try vm.builder().readOne("(lambda (x) x)");
    const proc_val = try compile(&vm, &arena, expr, .{});
    const proc = try vm.fromVal(Proc, proc_val);

    const lambda_proc = try vm.fromVal(Proc, proc.instructions[0].load);
    try testing.expectEqual(1, lambda_proc.args);
    try testing.expectEqualDeep(
        &[_]Instruction{.{ .get_local = 0 }},
        lambda_proc.instructions,
    );
}

test "compile empty body returns nil" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        .{ .load = Val.init({}) },
    }, &vm, "(begin)");

    var arena = std.heap.ArenaAllocator.init(vm.allocator);
    defer arena.deinit();
    const expr = try vm.builder().readOne("(lambda ())");
    const proc_val = try compile(&vm, &arena, expr, .{});
    const proc = try vm.fromVal(Proc, proc_val);
    const lambda_proc = try vm.fromVal(Proc, proc.instructions[0].load);
    try testing.expectEqualDeep(
        &[_]Instruction{.{ .load = Val.init({}) }},
        lambda_proc.instructions,
    );
}

test "compile single expression body no squash" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        .{ .load = Val.init(42) },
    }, &vm, "(begin 42)");
}

test "compile complex jump calculations" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const source = "(if #t (if #f 1 2) (if #t 3 4))";
    var arena = std.heap.ArenaAllocator.init(vm.allocator);
    defer arena.deinit();
    const expr = try vm.builder().readOne(source);
    const proc_val = try compile(&vm, &arena, expr, .{});
    const proc = try vm.fromVal(Proc, proc_val);

    var jump_count: u32 = 0;
    for (proc.instructions) |instr| {
        switch (instr) {
            .jump, .jump_if_not => jump_count += 1,
            else => {},
        }
    }
    try testing.expect(jump_count >= 4);
}

test "compile quote with nested list structure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(vm.allocator);
    defer arena.deinit();

    const proc_val = try compile(&vm, &arena, try vm.builder().readOne("'((1 2) (3 4))"), .{});
    const proc = try vm.fromVal(Proc, proc_val);

    try testing.expectEqual(1, proc.instructions.len);
    try testing.expectEqual(.load, std.meta.activeTag(proc.instructions[0]));
}

test "compile let* with complex dependencies" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("10", "(let* ((x 1) (y (+ x 2)) (z (* y 3))) (+ x z))");
}
