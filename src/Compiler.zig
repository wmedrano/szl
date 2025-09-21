//! A compiler that transforms Lisp expressions into bytecode instructions.
//!
//! The compiler builds a sequence of instructions that can be executed by the
//! virtual machine.
const std = @import("std");
const testing = std.testing;

const Inspector = @import("Inspector.zig");
const Instruction = @import("instruction.zig").Instruction;
const LexicalScope = @import("LexicalScope.zig");
const Pair = @import("Pair.zig");
const Procedure = @import("Procedure.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const Compiler = @This();

/// The virtual machine instance to compile for.
vm: *Vm,
/// Accumulated instructions during compilation.
instructions: std.ArrayList(Instruction) = .{},
/// Lexical scope for managing local variable bindings during compilation.
scope: LexicalScope = .{},

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
pub fn compile(vm: *Vm, expr: Val) Error!Val {
    var compiler = Compiler{
        .vm = vm,
    };
    defer compiler.deinit();
    try compiler.compileOne(expr);
    const proc = try compiler.toProcedure(null);
    return try vm.builder().build(proc);
}

/// Converts the compiler's accumulated instructions into a Procedure.
/// Takes ownership of the instruction list and creates a bytecode procedure
/// with the specified name, argument count, and local variable count from the lexical scope.
///
/// Args:
///   self: The compiler instance containing instructions and scope information.
///   name: Optional name for the procedure (used for debugging/introspection).
///
/// Returns:
///   A Procedure containing the compiled bytecode instructions, or an error if
///   memory allocation fails when transferring ownership of instructions.
fn toProcedure(self: *Compiler, name: ?Symbol.Interned) Error!Procedure {
    const instructions = try self.instructions.toOwnedSlice(self.vm.allocator);
    return Procedure{
        .name = name,
        .implementation = .{
            .bytecode = .{
                .args = self.scope.procedureArgCount(),
                .locals_count = self.scope.count(),
                .instructions = instructions,
            },
        },
    };
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
    const expr = try vm.builder().readOne(source);
    const proc_val = try compile(vm, expr);
    const proc = try vm.fromVal(Procedure, proc_val);

    try testing.expectEqualDeep(
        expected_instructions,
        proc.implementation.bytecode.instructions,
    );
}

/// Deallocates resources used by the compiler.
/// Cleans up the instruction list and lexical scope.
///
/// Args:
///   self: The compiler instance to clean up.
fn deinit(self: *Compiler) void {
    self.instructions.deinit(self.vm.allocator);
    self.scope.deinit(self.vm.allocator);
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
fn addInstruction(self: *Compiler, instruction: Instruction) Error!void {
    try self.instructions.append(self.vm.allocator, instruction);
}

/// Behavior when compiling multiple expressions.
const ExpressionBehavior = struct {
    allow_zero: bool = false,
    squash: bool = false,
};

/// Compiles multiple expressions from a list iterator into bytecode instructions.
/// Each expression is compiled in sequence, with results left on the stack.
///
/// Args:
///   self: The compiler instance.
///   iter: Iterator over the expressions to compile.
///   behavior: How to handle expression compilation and stack management.
///
/// Returns:
///   The number of expressions compiled, or an error if any expression cannot be compiled.
fn compileMany(self: *Compiler, iter: *Inspector.ListIterator, comptime behavior: ExpressionBehavior) Error!usize {
    var count: usize = 0;
    while (iter.next() catch return Error.InvalidExpression) |expr| {
        try self.compileOne(expr);
        count += 1;
    }
    switch (count) {
        0 => {
            if (!behavior.allow_zero) return Error.InvalidExpression;
            if (behavior.squash)
                try self.addInstruction(Instruction.initLoad(Val.init({})));
        },
        1 => {},
        else => if (behavior.squash) {
            try self.addInstruction(Instruction.initSquash(count));
        },
    }
    return count;
}

/// Compiles a single expression into bytecode instructions.
/// Dispatches to appropriate compilation method based on expression type.
/// Literals generate Load instructions, symbols generate GetGlobal/GetLocal,
/// and pairs are treated as function calls or special forms.
///
/// Args:
///   self: The compiler instance.
///   expr: The expression to compile.
///
/// Returns:
///   An error if the expression cannot be compiled.
fn compileOne(self: *Compiler, expr: Val) Error!void {
    switch (expr.repr) {
        .boolean, .i64, .f64, .char, .proc => try self.addInstruction(Instruction.initLoad(expr)),
        .nil => return error.InvalidExpression,
        .symbol => |s| return self.compileSymbol(s),
        .pair => |p| {
            const pair = self.vm.inspector().resolve(Pair, p) catch return Error.InvalidExpression;
            var args_iter = self.vm.inspector().iterList(pair.cdr) catch return Error.InvalidExpression;
            try self.compileExpression(pair.car, &args_iter);
        },
    }
}

/// Compiles a symbol by generating appropriate lookup instruction.
/// First checks if the symbol is a local binding (GetLocal),
/// otherwise generates a global lookup (GetGlobal).
///
/// Args:
///   self: The compiler instance.
///   symbol: The interned symbol to look up.
///
/// Returns:
///   An error if memory allocation fails.
fn compileSymbol(self: *Compiler, symbol: Symbol.Interned) Error!void {
    if (self.scope.resolve(symbol)) |binding|
        return self.addInstruction(Instruction.initGetLocal(binding.index));
    try self.addInstruction(Instruction.initGetGlobal(symbol));
}

/// Compiles a function call expression with arguments.
/// First checks if the leading value is a special form (define, if),
/// otherwise treats it as a regular function call with argument evaluation.
/// For regular calls: compiles function, then arguments, then EvalProcedure.
///
/// Args:
///   self: The compiler instance.
///   leading: The function or operator to call.
///   args_iter: Iterator over the arguments.
///
/// Returns:
///   An error if any part of the expression cannot be compiled.
fn compileExpression(self: *Compiler, leading: Val, args_iter: *Inspector.ListIterator) Error!void {
    if (self.vm.fromVal(Symbol.Interned, leading) catch null) |sym| {
        if (self.vm.common_symbols.@"if".eql(sym))
            return self.compileIf(args_iter);
        if (self.vm.common_symbols.begin.eql(sym))
            return self.compileBegin(args_iter);
        if (self.vm.common_symbols.cond.eql(sym))
            return self.compileCond(args_iter);
        if (self.vm.common_symbols.define.eql(sym))
            return self.compileDefine(args_iter);
        if (self.vm.common_symbols.let.eql(sym))
            return self.compileLet(args_iter);
        if (self.vm.common_symbols.quote.eql(sym))
            return self.compileQuote(args_iter);
    }
    try self.compileOne(leading);
    const arg_count = try self.compileMany(args_iter, .{ .allow_zero = true });
    try self.addInstruction(Instruction.initEvalProcedure(arg_count));
}

/// Compiles a begin expression into bytecode.
/// Begin expressions evaluate all their arguments in sequence and return the result of the last one.
/// If no arguments are provided, returns nil.
///
/// Args:
///   self: The compiler instance.
///   iter: Iterator over the arguments to the begin expression.
///
/// Returns:
///   An error if compilation fails.
fn compileBegin(self: *Compiler, iter: *Inspector.ListIterator) Error!void {
    _ = try self.compileMany(iter, .{ .allow_zero = true, .squash = true });
}

/// Calculates the relative jump distance between two instruction positions.
/// Used for conditional jumps and control flow in if statements.
/// Returns positive values for forward jumps, negative for backward jumps.
///
/// Args:
///   start: The starting instruction index.
///   target: The target instruction index.
///
/// Returns:
///   The relative jump distance as a signed integer.
fn jumpDistance(start: usize, target: usize) isize {
    return @as(isize, @intCast(target)) - @as(isize, @intCast(start));
}

/// Represents the jump indices needed for a single cond clause.
const ClauseJumps = struct {
    jump_if_not_pop: bool,
    jump_if_not_index: usize,
    jump_to_end_index: usize,
};

/// Compiles a single clause in a cond expression, returning jump indices for
/// later patching.
///
/// Returns null if this is an 'else' clause that should terminate clause processing.
fn compileCondClause(self: *Compiler, clause_val: Val) Error!?ClauseJumps {
    var clause_iter = self.vm.inspector().iterList(clause_val) catch return Error.InvalidExpression;

    // Extract and compile the predicate
    const predicate = clause_iter.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;

    // Handle 'else' clause specially
    if (std.meta.eql(predicate, Val.init(self.vm.common_symbols.@"else"))) {
        _ = try self.compileMany(&clause_iter, .{ .squash = true });
        return null; // Signal that this was an else clause
    }

    // Compile predicate and add conditional jump
    try self.compileOne(predicate);
    const jump_if_not_index = self.instructions.items.len;
    try self.addInstruction(Instruction.initJumpIfNot(.{ .steps = 0 })); // Placeholder, will be patched later

    // Check for arrow syntax (=>)
    const next_expr = clause_iter.peek() catch return Error.InvalidExpression;
    var uses_arrow_syntax = false;
    if (next_expr) |expr| {
        uses_arrow_syntax = std.meta.eql(expr, Val.init(self.vm.common_symbols.@"=>"));
        if (uses_arrow_syntax)
            _ = clause_iter.next() catch return Error.InvalidExpression; // Consume the '=>' symbol
    }

    if (!uses_arrow_syntax and clause_iter.isEmpty()) {
        try self.addInstruction(Instruction.initJump(0));
        return ClauseJumps{
            .jump_if_not_pop = false,
            .jump_if_not_index = jump_if_not_index,
            .jump_to_end_index = self.instructions.items.len - 1,
        };
    }

    // Compile the clause body
    const expression_count = try self.compileMany(&clause_iter, .{ .squash = true });
    if (uses_arrow_syntax and expression_count != 1) {
        return Error.InvalidExpression;
    }

    // Add jump to end after successful clause execution
    const jump_to_end_index = self.instructions.items.len;
    try self.addInstruction(Instruction.initJump(0)); // Placeholder, will be patched later
    return ClauseJumps{
        .jump_if_not_pop = true,
        .jump_if_not_index = jump_if_not_index,
        .jump_to_end_index = jump_to_end_index,
    };
}

fn compileCond(self: *Compiler, iter: *Inspector.ListIterator) Error!void {
    var clause_jumps = std.ArrayList(ClauseJumps){};
    defer clause_jumps.deinit(self.vm.allocator);
    var has_else_clause = false;

    // Process each clause in the cond expression
    while (iter.next() catch return Error.InvalidExpression) |clause_val| {
        if (try self.compileCondClause(clause_val)) |jump| {
            try clause_jumps.append(self.vm.allocator, jump);
        } else {
            // This was an 'else' clause
            has_else_clause = true;
            break;
        }
    }

    // Validate that we've processed all clauses
    if (!iter.isEmpty()) return Error.InvalidExpression;

    // Add default fallback if no else clause was provided
    if (!has_else_clause) {
        if (clause_jumps.items.len == 0) return Error.InvalidExpression;
        try self.addInstruction(Instruction.initLoad(Val.init({})));
    }

    // Patch all jump instructions with correct offsets
    const final_instruction_index = self.instructions.items.len;
    for (clause_jumps.items) |jumps| {
        // Patch the conditional jump to skip to next clause
        self.instructions.items[jumps.jump_if_not_index] =
            Instruction.initJumpIfNot(.{
                .pop = jumps.jump_if_not_pop,
                .steps = jumpDistance(jumps.jump_if_not_index, jumps.jump_to_end_index),
            });
        // Patch the unconditional jump to end
        self.instructions.items[jumps.jump_to_end_index] =
            Instruction.initJump(jumpDistance(jumps.jump_to_end_index + 1, final_instruction_index));
    }
}

/// Compiles an if expression into bytecode with conditional jumps.
/// Generates: predicate -> JumpIfNot -> true_branch -> Jump -> false_branch
/// Uses jump patching to fill in correct relative offsets after code generation.
/// Missing false branch defaults to nil.
///
/// Args:
///   self: The compiler instance.
///   iter: Iterator over if arguments (predicate, true_branch, optional false_branch).
///
/// Returns:
///   An error if the arguments are invalid or compilation fails.
fn compileIf(self: *Compiler, iter: *Inspector.ListIterator) Error!void {
    const pred = iter.next() catch return Error.InvalidExpression;
    const true_branch = iter.next() catch return Error.InvalidExpression;
    const false_branch = iter.next() catch return Error.InvalidExpression;
    if (!iter.isEmpty()) return Error.InvalidExpression;

    // Predicate
    try self.compileOne(pred orelse return Error.InvalidExpression);
    const jump_if_not_idx = self.instructions.items.len;
    try self.addInstruction(Instruction.initJumpIfNot(.{ .steps = 0 }));

    // True branch
    try self.compileOne(true_branch orelse return Error.InvalidExpression);
    const jump_idx = self.instructions.items.len;
    try self.addInstruction(Instruction.initJump(0));

    // False branch
    if (false_branch) |b|
        try self.compileOne(b)
    else
        try self.addInstruction(Instruction.initLoad(Val.init({})));

    // Fix jump calculations
    self.instructions.items[jump_if_not_idx] =
        Instruction.initJumpIfNot(.{ .steps = jumpDistance(jump_if_not_idx, jump_idx) });
    self.instructions.items[jump_idx] =
        Instruction.initJump(jumpDistance(jump_idx + 1, self.instructions.items.len));
}

/// Compiles a define expression by pattern matching on the signature.
/// Handles both variable definitions (define symbol value) and
/// procedure definitions (define (name args...) body...).
/// Dispatches to appropriate compilation method based on signature type.
///
/// Args:
///   self: The compiler instance.
///   iter: Iterator over the arguments to the define expression.
///
/// Returns:
///   An error if the arguments are invalid or compilation fails.
fn compileDefine(self: *Compiler, iter: *Inspector.ListIterator) Error!void {
    const signature_val = iter.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;
    switch (signature_val.repr) {
        .symbol => |sym| return self.compileDefineVal(sym, iter),
        .pair => {
            var signature = self.vm.inspector().iterList(signature_val) catch
                return Error.InvalidExpression;
            return self.compileDefineProc(&signature, iter);
        },
        else => return Error.InvalidExpression,
    }
}

/// Compiles a variable definition (define symbol value).
/// Generates instructions to call szl-define with the symbol and value.
///
/// Args:
///   self: The compiler instance.
///   symbol: The symbol being defined.
///   body: Iterator containing the value expression.
///
/// Returns:
///   An error if the arguments are invalid or compilation fails.
fn compileDefineVal(self: *Compiler, symbol: Symbol.Interned, body: *Inspector.ListIterator) Error!void {
    const expr = body.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;
    if (!body.isEmpty()) return Error.InvalidExpression;

    try self.addInstruction(Instruction.initGetGlobal(self.vm.common_symbols.@"szl-define"));
    try self.addInstruction(Instruction.initLoad(Val.init(symbol)));
    try self.compileOne(expr);
    try self.addInstruction(Instruction.initEvalProcedure(2));
}

/// Compiles a procedure definition into bytecode.
/// Creates a sub-compiler with lexical bindings for parameters,
/// compiles the procedure body, then wraps it in a Procedure value.
/// The resulting procedure is defined globally using szl-define.
///
/// Args:
///   self: The compiler instance.
///   signature: Iterator over the procedure signature (name and parameters).
///   body: Iterator over the procedure body expressions.
///
/// Returns:
///   An error if the arguments are invalid or compilation fails.
fn compileDefineProc(self: *Compiler, signature: *Inspector.ListIterator, body: *Inspector.ListIterator) Error!void {
    var sub_compiler = Compiler{
        .vm = self.vm,
    };
    defer sub_compiler.deinit();
    const name = signature.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;
    const name_sym = self.vm.fromVal(Symbol.Interned, name) catch
        return Error.InvalidExpression;
    var arg_count: usize = 0;
    while (signature.next() catch return Error.InvalidExpression) |arg_val| {
        const arg = self.vm.fromVal(Symbol.Interned, arg_val) catch
            return Error.InvalidExpression;
        _ = try sub_compiler.scope.addBinding(
            self.vm.allocator,
            LexicalScope.Binding{
                .name = arg,
                .index = arg_count,
                .type = .argument,
            },
        );
        arg_count += 1;
    }

    _ = try sub_compiler.compileMany(body, .{});
    const proc = try sub_compiler.toProcedure(name_sym);
    const proc_val = self.vm.builder().build(proc) catch return Error.InvalidExpression;

    try self.addInstruction(Instruction.initGetGlobal(self.vm.common_symbols.@"szl-define"));
    try self.addInstruction(Instruction.initLoad(name));
    try self.addInstruction(Instruction.initLoad(proc_val));
    try self.addInstruction(Instruction.initEvalProcedure(2));
}

/// Compiles a single let binding (name expression) pair.
///
/// Parses a binding in the form (name expression), compiles the expression,
/// and creates a hidden binding that will store the result. The binding starts
/// as 'hidden' so it's not available to other binding expressions in the same let.
///
/// Args:
///   self: The compiler instance.
///   binding: The binding pair in the form (name expression).
///
/// Returns:
///   The binding ID for later activation, or an error if compilation fails.
fn compileLetBinding(self: *Compiler, binding: Val) Error!LexicalScope.Id {
    const inspector = self.vm.inspector();
    var parts = inspector.iterList(binding) catch return Error.InvalidExpression;

    // Extract variable name from binding
    const name_val = parts.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;

    const name = inspector.to(Symbol.Interned, name_val) catch return Error.InvalidExpression;

    // Create hidden binding for the variable (not yet accessible to other bindings)
    const id = try self.scope.addVariable(self.vm.allocator, name, .hidden);

    // Extract and compile the initialization expression
    const expr_val = parts.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;

    if (!parts.isEmpty()) return Error.InvalidExpression;

    // Compile the expression and store result in the local variable
    try self.compileOne(expr_val);
    try self.addInstruction(
        Instruction.initSetLocal(self.scope.getBinding(id).?.index),
    );

    return id;
}

/// Compiles all let bindings in sequence, collecting their IDs.
/// Each binding is compiled, but remains hidden until activation.
///
/// Args:
///   self: The compiler instance.
///   bindings_expr: The list of binding pairs.
///
/// Returns:
///   ArrayList of binding IDs for later activation.
fn compileLetBindings(self: *Compiler, bindings_expr: Val) Error!std.ArrayList(LexicalScope.Id) {
    var ids = std.ArrayList(LexicalScope.Id){};
    const inspector = self.vm.inspector();
    var bindings = inspector.iterList(bindings_expr) catch return Error.InvalidExpression;

    while (bindings.next() catch return Error.InvalidExpression) |binding| {
        const id = try self.compileLetBinding(binding);
        try ids.append(self.vm.allocator, id);
    }

    return ids;
}

/// Activates hidden let bindings by changing their type to local variables.
/// This makes the bindings available for use in the let body.
///
/// Args:
///   self: The compiler instance.
///   ids: List of binding IDs to activate.
fn activateLetBindings(self: *Compiler, ids: []const LexicalScope.Id) void {
    for (ids) |id|
        self.scope.getBinding(id).?.type = .local;
}

/// Compiles a let expression into bytecode.
///
/// Let expressions bind variables locally and evaluate expressions in that scope.
/// Uses a two-phase binding approach to ensure proper semantics:
/// 1. All binding expressions are compiled with bindings in 'hidden' state
/// 2. Bindings are activated (changed to 'local') before compiling the body
///
/// This prevents bindings from referencing themselves or later bindings during
/// their initialization, which matches standard let semantics.
///
/// Args:
///   self: The compiler instance.
///   iter: Iterator over the let arguments (bindings and body expressions).
///
/// Returns:
///   An error if compilation fails.
fn compileLet(self: *Compiler, iter: *Inspector.ListIterator) Error!void {
    // Extract the bindings list from the let expression
    const bindings_expr = iter.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;

    // Phase 1: Compile all binding expressions while bindings are hidden
    var let_binding_ids = try self.compileLetBindings(bindings_expr);
    defer {
        // Clean up bindings when exiting let scope to prevent variable leakage
        for (let_binding_ids.items) |id| self.scope.clear(id);
        let_binding_ids.deinit(self.vm.allocator);
    }

    // Phase 2: Activate bindings (convert hidden -> local) before evaluating body
    self.activateLetBindings(let_binding_ids.items);

    // Compile body expressions with activated bindings available
    _ = try self.compileMany(iter, .{ .allow_zero = true, .squash = true });
}

/// Compiles a quote expression into bytecode.
/// Quote expressions take exactly one argument and return it without evaluation.
/// Generates a Load instruction with the quoted value.
///
/// Args:
///   self: The compiler instance.
///   iter: Iterator over the arguments to the quote expression.
///
/// Returns:
///   An error if the arguments are invalid (not exactly one argument).
fn compileQuote(self: *Compiler, iter: *Inspector.ListIterator) Error!void {
    const expr = iter.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;
    if (!iter.isEmpty()) return Error.InvalidExpression;

    try self.addInstruction(Instruction.initLoad(expr));
}

test "compile with expression is function call" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try expectInstructions(&[_]Instruction{
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("+"))),
        Instruction.initLoad(Val.init(1)),
        Instruction.initLoad(Val.init(2)),
        Instruction.initEvalProcedure(2),
    }, &vm, "(+ 1 2)");
}

test "compile with nested expression is multiple function calls" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try expectInstructions(&[_]Instruction{
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("foo"))),
        Instruction.initLoad(Val.init(1)),
        Instruction.initLoad(Val.init(2)),
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("bar"))),
        Instruction.initLoad(Val.init(3)),
        Instruction.initLoad(Val.init(4)),
        Instruction.initEvalProcedure(2),
        Instruction.initEvalProcedure(3),
    }, &vm, "(foo 1 2 (bar 3 4))");
}

test "compile define expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try expectInstructions(&[_]Instruction{
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("szl-define"))),
        Instruction.initLoad(try vm.builder().internStaticVal(Symbol.init("x"))),
        Instruction.initLoad(Val.init(42)),
        Instruction.initEvalProcedure(2),
    }, &vm, "(define x 42)");
}

test "compile define procedure expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    const expr = try vm.builder().readOne("(define (foo) 42)");
    const proc = try vm.fromVal(Procedure, try compile(&vm, expr));

    const instructions = proc.implementation.bytecode.instructions;

    try testing.expectEqual(4, instructions.len);
    try testing.expectEqual(
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("szl-define"))),
        instructions[0],
    );
    try testing.expectEqual(
        Instruction.initLoad(try vm.builder().internStaticVal(Symbol.init("foo"))),
        instructions[1],
    );
    try testing.expectEqual(
        .proc,
        std.meta.activeTag(instructions[2].load.repr),
    );
    try testing.expectEqual(
        Instruction.initEvalProcedure(2),
        instructions[3],
    );
}

test "compile if expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    const plus = try vm.builder().internStatic(Symbol.init("+"));

    try expectInstructions(&[_]Instruction{
        Instruction.initLoad(Val.init(true)),
        Instruction.initJumpIfNot(.{ .steps = 5 }),
        // True Branch
        Instruction.initGetGlobal(plus),
        Instruction.initLoad(Val.init(1)),
        Instruction.initLoad(Val.init(2)),
        Instruction.initEvalProcedure(2),
        Instruction.initJump(6),
        // False branch
        Instruction.initGetGlobal(plus),
        Instruction.initLoad(Val.init(3)),
        Instruction.initLoad(Val.init(4)),
        Instruction.initLoad(Val.init(5)),
        Instruction.initLoad(Val.init(6)),
        Instruction.initEvalProcedure(4),
    }, &vm, "(if #t (+ 1 2) (+ 3 4 5 6))");
}

test "compile if expression with missing false branch uses nil" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    const plus = try vm.builder().internStatic(Symbol.init("+"));

    try expectInstructions(&[_]Instruction{
        Instruction.initLoad(Val.init(true)),
        Instruction.initJumpIfNot(.{ .steps = 5 }),
        // True Branch
        Instruction.initGetGlobal(plus),
        Instruction.initLoad(Val.init(1)),
        Instruction.initLoad(Val.init(2)),
        Instruction.initEvalProcedure(2),
        Instruction.initJump(1),
        // False branch (nil)
        Instruction.initLoad(Val.init({})),
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

    // Verify the compilation generates the right instructions with jump_if_not
    try expectInstructions(&[_]Instruction{
        Instruction.initLoad(Val.init(false)),
        Instruction.initJumpIfNot(.{ .steps = 2 }),
        Instruction.initLoad(Val.init(42)),
        Instruction.initJump(1),
        Instruction.initLoad(Val.init({})),
    }, &vm, "(if #f 42)");
}

test "cond with single clause" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        // #t branch
        Instruction.initLoad(Val.init(true)),
        Instruction.initJumpIfNot(.{ .steps = 2 }),
        Instruction.initLoad(Val.init(42)),
        Instruction.initJump(1),
        // fallback branch
        Instruction.initLoad(Val.init({})),
    }, &vm, "(cond (#t 42))");
}

test "cond clause with single test fails" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        Instruction.initLoad(Val.init(true)),
        Instruction.initJumpIfNot(.{ .pop = false, .steps = 1 }),
        Instruction.initJump(1),
        Instruction.initLoad(Val.init({})),
    }, &vm, "(cond (#t))");
}

test "cond with => ignores =>" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        // #t branch
        Instruction.initLoad(Val.init(true)),
        Instruction.initJumpIfNot(.{ .steps = 2 }),
        Instruction.initLoad(Val.init(42)),
        Instruction.initJump(1),
        // fallback branch
        Instruction.initLoad(Val.init({})),
    }, &vm, "(cond (#t => 42))");
}

test "cond with => fails if more than one expression fails" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Error.InvalidExpression,
        compile(
            &vm,
            try vm.builder().readOne("(cond (#t => 42 43))"),
        ),
    );
}

test "cond with multiple clauses" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        // #t branch
        Instruction.initLoad(Val.init(true)),
        Instruction.initJumpIfNot(.{ .steps = 2 }),
        Instruction.initLoad(Val.init(1)),
        Instruction.initJump(11),
        // #f branch
        Instruction.initLoad(Val.init(false)),
        Instruction.initJumpIfNot(.{ .steps = 2 }),
        Instruction.initLoad(Val.init(2)),
        Instruction.initJump(7),
        // 3 branch
        Instruction.initLoad(Val.init(3)),
        Instruction.initJumpIfNot(.{ .steps = 4 }),
        Instruction.initLoad(Val.init(4)),
        Instruction.initLoad(Val.init(5)),
        Instruction.initSquash(2),
        Instruction.initJump(1),
        // fallback
        Instruction.initLoad(Val.init({})),
    }, &vm, "(cond (#t 1) (#f 2) (3 4 5))");
}

test "cond without clauses is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        error.InvalidExpression,
        compile(&vm, try vm.builder().readOne("(cond)")),
    );
    try testing.expectError(
        error.InvalidExpression,
        compile(&vm, try vm.builder().readOne("(cond #t)")),
    );
}

test "cond with else uses it as fallback" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        // #t branch
        Instruction.initLoad(Val.init(true)),
        Instruction.initJumpIfNot(.{ .steps = 2 }),
        Instruction.initLoad(Val.init(42)),
        Instruction.initJump(1),
        // fallback branch
        Instruction.initLoad(Val.init(43)),
    }, &vm, "(cond (#t => 42) (else 43))");
}

test "cond with only else expression is expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        Instruction.initLoad(Val.init(43)),
    }, &vm, "(cond (else 43))");
}

test "compile quote with integer" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc_val = try compile(&vm, try vm.builder().readOne("'42"));
    const proc = try vm.fromVal(Procedure, proc_val);

    try testing.expectEqual(1, proc.implementation.bytecode.instructions.len);
    try testing.expectEqual(
        Instruction.initLoad(Val.init(42)),
        proc.implementation.bytecode.instructions[0],
    );
}

test "compile quote with symbol" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try expectInstructions(
        &[_]Instruction{
            Instruction.initLoad(try vm.builder().internStaticVal(Symbol.init("foo"))),
        },
        &vm,
        "'foo",
    );
}

test "compile quote with boolean" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try expectInstructions(
        &[_]Instruction{Instruction.initLoad(Val.init(true))},
        &vm,
        "'#t",
    );
}

test "compile quote with list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc_val = try compile(&vm, try vm.builder().readOne("'(1 2 3)"));
    const proc = try vm.fromVal(Procedure, proc_val);

    try testing.expectEqual(1, proc.implementation.bytecode.instructions.len);
    const instruction = proc.implementation.bytecode.instructions[0];
    try testing.expectEqual(.load, std.meta.activeTag(instruction));

    // Check that the loaded value is the list (1 2 3)
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
        &[_]Instruction{Instruction.initLoad(Val.init({}))},
        &vm,
        "'()",
    );
}

test "compile quote with no arguments fails" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Error.InvalidExpression,
        compile(&vm, try vm.builder().readOne("(quote)")),
    );
    try testing.expectError(
        error.BadExpression,
        vm.builder().readOne("'"),
    );
}

test "compile quote with multiple arguments fails" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Error.InvalidExpression,
        compile(&vm, try vm.builder().readOne("(quote 1 2)")),
    );
}

test "compile begin with no arguments returns nil" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        Instruction.initLoad(Val.init({})),
    }, &vm, "(begin)");
}

test "compile begin with single expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        Instruction.initLoad(Val.init(42)),
    }, &vm, "(begin 42)");
}

test "compile begin with multiple expressions" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        Instruction.initLoad(Val.init(1)),
        Instruction.initLoad(Val.init(2)),
        Instruction.initLoad(Val.init(3)),
        Instruction.initSquash(3),
    }, &vm, "(begin 1 2 3)");
}

test "compile begin with function calls" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("+"))),
        Instruction.initLoad(Val.init(1)),
        Instruction.initLoad(Val.init(2)),
        Instruction.initEvalProcedure(2),
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("+"))),
        Instruction.initLoad(Val.init(3)),
        Instruction.initLoad(Val.init(4)),
        Instruction.initEvalProcedure(2),
        Instruction.initSquash(2),
    }, &vm, "(begin (+ 1 2) (+ 3 4))");
}

test "compile let with single binding" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        Instruction.initLoad(Val.init(42)),
        Instruction.initSetLocal(0),
        Instruction.initGetLocal(0),
    }, &vm, "(let ((x 42)) x)");
}

test "compile let with multiple bindings" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const src =
        \\ (let ((x 10)
        \\       (y 20))
        \\   (+ x y))
    ;
    try expectInstructions(&[_]Instruction{
        // Bind x
        Instruction.initLoad(Val.init(10)),
        Instruction.initSetLocal(0),
        // Bind y
        Instruction.initLoad(Val.init(20)),
        Instruction.initSetLocal(1),
        // Evaluate body
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("+"))),
        Instruction.initGetLocal(0),
        Instruction.initGetLocal(1),
        Instruction.initEvalProcedure(2),
    }, &vm, src);
}

test "compile let with no bindings" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        Instruction.initLoad(Val.init(42)),
    }, &vm, "(let () 42)");
}

test "compile let with multiple body expressions returns last" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        // Bind x
        Instruction.initLoad(Val.init(5)),
        Instruction.initSetLocal(0),
        // Evaluate 1st body expression
        Instruction.initGetLocal(0),
        // Evaluate 2nd body expression
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("+"))),
        Instruction.initGetLocal(0),
        Instruction.initLoad(Val.init(1)),
        Instruction.initEvalProcedure(2),
        // Keep only the last value
        Instruction.initSquash(2),
    }, &vm, "(let ((x 5)) x (+ x 1))");
}

test "compile let with expression in binding" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        // Bind x
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("+"))),
        Instruction.initLoad(Val.init(3)),
        Instruction.initLoad(Val.init(4)),
        Instruction.initEvalProcedure(2),
        Instruction.initSetLocal(0),
        // Evaluate body
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("*"))),
        Instruction.initGetLocal(0),
        Instruction.initLoad(Val.init(2)),
        Instruction.initEvalProcedure(2),
    }, &vm, "(let ((x (+ 3 4))) (* x 2))");
}

test "compile let variable scoping" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test that let variables are not available in their own binding expressions
    // This should compile y as a global variable reference since x is not yet available
    try expectInstructions(&[_]Instruction{
        // Bind x
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("y"))),
        Instruction.initSetLocal(0),
        // Bind y
        Instruction.initLoad(Val.init(20)),
        Instruction.initSetLocal(1),
        // Evaluate body
        Instruction.initGetLocal(0),
        Instruction.initGetLocal(1),
        Instruction.initSquash(2),
    }, &vm, "(let ((x y) (y 20)) x y)");
}

test "compile nested let expressions" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        // Bind outer x
        Instruction.initLoad(Val.init(10)),
        Instruction.initSetLocal(0),
        // Bind inner y
        Instruction.initLoad(Val.init(5)),
        Instruction.initSetLocal(1),
        // Evaluate inner body
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("+"))),
        Instruction.initGetLocal(0),
        Instruction.initGetLocal(1),
        Instruction.initEvalProcedure(2),
    }, &vm, "(let ((x 10)) (let ((y 5)) (+ x y)))");
}

test "compile let with invalid binding format fails" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test invalid binding with too many elements
    try testing.expectError(
        Error.InvalidExpression,
        compile(&vm, try vm.builder().readOne("(let ((x 1 2)) x)")),
    );

    // Test invalid binding with no elements
    try testing.expectError(
        Error.InvalidExpression,
        compile(&vm, try vm.builder().readOne("(let (()) x)")),
    );

    // Test invalid binding with non-symbol name
    try testing.expectError(
        Error.InvalidExpression,
        compile(&vm, try vm.builder().readOne("(let ((42 1)) x)")),
    );
}

test "compile let with no body returns nil" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        Instruction.initLoad(Val.init(1)),
        Instruction.initSetLocal(0),
        Instruction.initLoad(Val.init({})),
    }, &vm, "(let ((x 1)))");
}

test "compile let with missing bindings fails" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Error.InvalidExpression,
        compile(&vm, try vm.builder().readOne("(let)")),
    );
}
