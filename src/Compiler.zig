//! A compiler that transforms Lisp expressions into bytecode instructions.
//!
//! The compiler builds a sequence of instructions that can be executed by the
//! virtual machine.
const std = @import("std");
const testing = std.testing;

const Inspector = @import("Inspector.zig");
const Instruction = @import("Instruction.zig");
const Pair = @import("Pair.zig");
const Procedure = @import("Procedure.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const Compiler = @This();

/// The virtual machine instance to compile for.
vm: *Vm,
/// Cached interned symbols used during compilation for efficient lookups.
symbols: SymbolTable,
/// Accumulated instructions during compilation.
instructions: std.ArrayList(Instruction) = .{},
/// List of lexical bindings (local variables) visible in the current compilation scope.
/// These bindings are used to resolve local variable references during compilation.
bindings: std.ArrayList(LexicalBind) = .{},

/// Represents a lexical binding that maps a symbol name to a local variable index.
/// Used for resolving local variable references during procedure compilation.
const LexicalBind = struct {
    /// The interned symbol name for this binding.
    name: Symbol.Interned,
    /// The local variable index in the stack frame where this binding's value is stored.
    index: usize,
};

/// Table of commonly used symbols that are pre-interned for efficient compilation.
///
/// This struct contains interned versions of symbols that the compiler needs
/// to recognize and handle specially during compilation. By pre-interning these
/// symbols, the compiler can perform fast symbol comparisons without needing
/// to intern symbols repeatedly during compilation.
const SymbolTable = struct {
    @"=>": Symbol.Interned,
    @"if": Symbol.Interned,
    @"szl-define": Symbol.Interned,
    cond: Symbol.Interned,
    @"else": Symbol.Interned,
    define: Symbol.Interned,
    quote: Symbol.Interned,
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
pub fn compile(vm: *Vm, expr: Val) Error!Val {
    var compiler = Compiler{
        .vm = vm,
        .symbols = try vm.builder().symbolTable(SymbolTable),
    };
    defer compiler.deinit();
    try compiler.compileOne(expr);
    const instructions = try compiler.instructions.toOwnedSlice(vm.allocator);
    const proc = Procedure{
        .name = null,
        .implementation = Procedure.initBytecode(instructions),
    };
    return try vm.builder().build(proc);
}

/// Deallocates resources used by the compiler.
/// Cleans up the instruction list and lexical bindings.
///
/// Args:
///   self: The compiler instance to clean up.
fn deinit(self: *Compiler) void {
    self.instructions.deinit(self.vm.allocator);
    self.bindings.deinit(self.vm.allocator);
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

/// Adds a lexical binding to the compiler's binding list.
/// Creates a mapping from a symbol name to a local variable index,
/// enabling the compiler to resolve local variable references within procedure scope.
/// Bindings are added in order to support proper lexical scoping.
///
/// Args:
///   self: The compiler instance.
///   binding: The lexical binding to add, containing the symbol name and stack index.
///
/// Returns:
///   An error if memory allocation fails.
fn addBinding(self: *Compiler, binding: LexicalBind) Error!void {
    try self.bindings.append(self.vm.allocator, binding);
}

/// Finds the stack index of a lexical binding by symbol name.
/// Searches bindings in reverse order to implement proper lexical scoping
/// where inner bindings shadow outer ones (most recent binding wins).
///
/// Args:
///   self: The compiler instance.
///   symbol: The interned symbol to search for.
///
/// Returns:
///   The stack index of the binding if found, or null if not found.
fn findBinding(self: *Compiler, symbol: Symbol.Interned) ?usize {
    var i = self.bindings.items.len;
    while (i > 0) {
        i -= 1;
        if (self.bindings.items[i].name.eql(symbol))
            return self.bindings.items[i].index;
    }
    return null;
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
        .boolean, .i64, .f64, .procedure => try self.addInstruction(Instruction.initLoad(expr)),
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
    if (self.findBinding(symbol)) |idx|
        return self.addInstruction(Instruction.initGetLocal(idx));
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
        if (self.symbols.@"if".eql(sym))
            return self.compileIf(args_iter);
        if (self.symbols.cond.eql(sym))
            return self.compileCond(args_iter);
        if (self.symbols.define.eql(sym))
            return self.compileDefine(args_iter);
        if (self.symbols.quote.eql(sym))
            return self.compileQuote(args_iter);
    }
    try self.compileOne(leading);
    const arg_count = try self.compileMany(args_iter, .{ .allow_zero = true });
    try self.addInstruction(Instruction.initEvalProcedure(arg_count));
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
    if (std.meta.eql(predicate, Val.init(self.symbols.@"else"))) {
        _ = try self.compileMany(&clause_iter, .{ .squash = true });
        return null; // Signal that this was an else clause
    }

    // Compile predicate and add conditional jump
    try self.compileOne(predicate);
    const jump_if_not_index = self.instructions.items.len;
    try self.addInstruction(Instruction.initJumpIfNot(0)); // Placeholder, will be patched later

    // Check for arrow syntax (=>)
    const next_expr = clause_iter.peek() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;
    const uses_arrow_syntax = std.meta.eql(next_expr, Val.init(self.symbols.@"=>"));

    if (uses_arrow_syntax) {
        _ = clause_iter.next() catch return Error.InvalidExpression; // Consume the '=>' symbol
    }

    // Compile the clause body
    const expression_count = try self.compileMany(&clause_iter, .{ .squash = true });
    if (uses_arrow_syntax and expression_count > 1) {
        return Error.InvalidExpression;
    }

    // Add jump to end after successful clause execution
    const jump_to_end_index = self.instructions.items.len;
    try self.addInstruction(Instruction.initJump(0)); // Placeholder, will be patched later
    return ClauseJumps{
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
    if (!iter.isEmpty())
        return Error.InvalidExpression;

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
            Instruction.initJumpIfNot(jumpDistance(jumps.jump_if_not_index, jumps.jump_to_end_index));
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
    try self.addInstruction(Instruction.initJumpIfNot(0));

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
    self.instructions.items[jump_if_not_idx] = Instruction.initJumpIfNot(jumpDistance(jump_if_not_idx, jump_idx + 1));
    self.instructions.items[jump_idx] = Instruction.initJump(
        jumpDistance(jump_idx + 1, self.instructions.items.len),
    );
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

    try self.addInstruction(Instruction.initGetGlobal(self.symbols.@"szl-define"));
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
        .symbols = self.symbols,
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
        try sub_compiler.addBinding(LexicalBind{ .name = arg, .index = arg_count });
        arg_count += 1;
    }

    _ = try sub_compiler.compileMany(body, .{});
    const instructions = try sub_compiler.instructions.toOwnedSlice(self.vm.allocator);
    const proc = Procedure{
        .name = name_sym,
        .implementation = Procedure.initBytecode(instructions),
    };
    const proc_val = self.vm.builder().build(proc) catch return Error.InvalidExpression;

    try self.addInstruction(Instruction.initGetGlobal(self.symbols.@"szl-define"));
    try self.addInstruction(Instruction.initLoad(name));
    try self.addInstruction(Instruction.initLoad(proc_val));
    try self.addInstruction(Instruction.initEvalProcedure(2));
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
    const expr = try vm.builder().readOne("(+ 1 2)");
    const proc = try vm.fromVal(Procedure, try compile(&vm, expr));

    try testing.expectEqualDeep(
        &[_]Instruction{
            Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("+"))),
            Instruction.initLoad(Val.init(1)),
            Instruction.initLoad(Val.init(2)),
            Instruction.initEvalProcedure(2),
        },
        proc.implementation.bytecode.instructions,
    );
}

test "compile with nested expression is multiple function calls" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    const expr = try vm.builder().readOne("(foo 1 2 (bar 3 4))");
    const proc = try vm.fromVal(Procedure, try compile(&vm, expr));

    try testing.expectEqualDeep(
        &[_]Instruction{
            Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("foo"))),
            Instruction.initLoad(Val.init(1)),
            Instruction.initLoad(Val.init(2)),
            Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("bar"))),
            Instruction.initLoad(Val.init(3)),
            Instruction.initLoad(Val.init(4)),
            Instruction.initEvalProcedure(2),
            Instruction.initEvalProcedure(3),
        },
        proc.implementation.bytecode.instructions,
    );
}

test "compile define expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    const expr = try vm.builder().readOne("(define x 42)");
    const proc = try vm.fromVal(Procedure, try compile(&vm, expr));

    try testing.expectEqualDeep(
        &[_]Instruction{
            Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("szl-define"))),
            Instruction.initLoad(try vm.builder().internStaticVal(Symbol.init("x"))),
            Instruction.initLoad(Val.init(42)),
            Instruction.initEvalProcedure(2),
        },
        proc.implementation.bytecode.instructions,
    );
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
        .procedure,
        std.meta.activeTag(instructions[2].repr.load.repr),
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
    const expr = try vm.builder().readOne("(if #t (+ 1 2) (+ 3 4 5 6))");
    const proc = try vm.fromVal(Procedure, try compile(&vm, expr));

    try testing.expectEqualDeep(
        &[_]Instruction{
            Instruction.initLoad(Val.init(true)),
            Instruction.initJumpIfNot(6),
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
        },
        proc.implementation.bytecode.instructions,
    );
}

test "compile if expression with missing false branch uses nil" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    const plus = try vm.builder().internStatic(Symbol.init("+"));
    const expr = try vm.builder().readOne("(if #t (+ 1 2))");
    const proc = try vm.fromVal(Procedure, try compile(&vm, expr));

    try testing.expectEqualDeep(
        &[_]Instruction{
            Instruction.initLoad(Val.init(true)),
            Instruction.initJumpIfNot(6),
            // True Branch
            Instruction.initGetGlobal(plus),
            Instruction.initLoad(Val.init(1)),
            Instruction.initLoad(Val.init(2)),
            Instruction.initEvalProcedure(2),
            Instruction.initJump(1),
            // False branch (nil)
            Instruction.initLoad(Val.init({})),
        },
        proc.implementation.bytecode.instructions,
    );
}

test "if with true predicate evaluates true branch" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try testing.expectEqual(
        try vm.evalStr("(if #t (+ 1 2 3 4) (+ 1 2))"),
        Val.init(10),
    );
    try testing.expectEqual(
        try vm.evalStr("(if #t (+ 1 2 3 4))"),
        Val.init(10),
    );
}

test "if with false predicate evaluates false branch" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try testing.expectEqual(
        try vm.evalStr("(if #t (+ 1 2 3 4) (+ 1 2))"),
        Val.init(10),
    );
}

test "if with missing false branch uses nil also branch" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Verify the compilation generates the right instructions with jump_if_not
    const expr = try vm.builder().readOne("(if #f 42)");
    const proc_val = try compile(&vm, expr);
    const proc = try vm.fromVal(Procedure, proc_val);

    try testing.expectEqualDeep(
        &[_]Instruction{
            Instruction.initLoad(Val.init(false)),
            Instruction.initJumpIfNot(3),
            Instruction.initLoad(Val.init(42)),
            Instruction.initJump(1),
            Instruction.initLoad(Val.init({})),
        },
        proc.implementation.bytecode.instructions,
    );
}

test "cond with single clause" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc_val = try compile(&vm, try vm.builder().readOne("(cond (#t 42))"));
    const proc = try vm.fromVal(Procedure, proc_val);
    try testing.expectEqualDeep(
        &[_]Instruction{
            // #t branch
            Instruction.initLoad(Val.init(true)),
            Instruction.initJumpIfNot(2),
            Instruction.initLoad(Val.init(42)),
            Instruction.initJump(1),
            // fallback branch
            Instruction.initLoad(Val.init({})),
        },
        proc.implementation.bytecode.instructions,
    );
}

// TODO: This should actually return the result of the test.
test "cond clause with single test fails" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Error.InvalidExpression,
        compile(&vm, try vm.builder().readOne("(cond (#t))")),
    );
}

test "cond with => ignores =>" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc_val = try compile(&vm, try vm.builder().readOne("(cond (#t => 42))"));
    const proc = try vm.fromVal(Procedure, proc_val);
    try testing.expectEqualDeep(
        &[_]Instruction{
            // #t branch
            Instruction.initLoad(Val.init(true)),
            Instruction.initJumpIfNot(2),
            Instruction.initLoad(Val.init(42)),
            Instruction.initJump(1),
            // fallback branch
            Instruction.initLoad(Val.init({})),
        },
        proc.implementation.bytecode.instructions,
    );
}

test "cond with => fails if more than one expression fails" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Error.InvalidExpression,
        compile(&vm, try vm.builder().readOne("(cond (#t => 42 43))")),
    );
}

test "cond with multiple clauses" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc_val = try compile(&vm, try vm.builder().readOne("(cond (#t 1) (#f 2) (3 4 5))"));
    const proc = try vm.fromVal(Procedure, proc_val);
    try testing.expectEqualDeep(
        &[_]Instruction{
            // #t branch
            Instruction.initLoad(Val.init(true)),
            Instruction.initJumpIfNot(2),
            Instruction.initLoad(Val.init(1)),
            Instruction.initJump(11),
            // #f branch
            Instruction.initLoad(Val.init(false)),
            Instruction.initJumpIfNot(2),
            Instruction.initLoad(Val.init(2)),
            Instruction.initJump(7),
            // 3 branch
            Instruction.initLoad(Val.init(3)),
            Instruction.initJumpIfNot(4),
            Instruction.initLoad(Val.init(4)),
            Instruction.initLoad(Val.init(5)),
            Instruction.initSquash(2),
            Instruction.initJump(1),
            // fallback
            Instruction.initLoad(Val.init({})),
        },
        proc.implementation.bytecode.instructions,
    );
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

    const proc_val = try compile(&vm, try vm.builder().readOne("(cond (#t => 42) (else 43))"));
    const proc = try vm.fromVal(Procedure, proc_val);
    try testing.expectEqualDeep(
        &[_]Instruction{
            // #t branch
            Instruction.initLoad(Val.init(true)),
            Instruction.initJumpIfNot(2),
            Instruction.initLoad(Val.init(42)),
            Instruction.initJump(1),
            // fallback branch
            Instruction.initLoad(Val.init(43)),
        },
        proc.implementation.bytecode.instructions,
    );
}

test "cond with only else expression is expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc_val = try compile(&vm, try vm.builder().readOne("(cond (else 43))"));
    const proc = try vm.fromVal(Procedure, proc_val);
    try testing.expectEqualDeep(
        &[_]Instruction{
            Instruction.initLoad(Val.init(43)),
        },
        proc.implementation.bytecode.instructions,
    );
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

    const proc_val = try compile(&vm, try vm.builder().readOne("'foo"));
    const proc = try vm.fromVal(Procedure, proc_val);

    try testing.expectEqual(1, proc.implementation.bytecode.instructions.len);
    const instruction = proc.implementation.bytecode.instructions[0];
    try testing.expectEqual(.load, std.meta.activeTag(instruction.repr));

    // Check that the loaded value is the symbol 'foo'
    const loaded_val = instruction.repr.load;
    try testing.expectEqual(.symbol, std.meta.activeTag(loaded_val.repr));
    try testing.expectFmt(
        "foo",
        "{f}",
        .{vm.pretty(loaded_val)},
    );
}

test "compile quote with boolean" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc_val = try compile(&vm, try vm.builder().readOne("#t"));
    const proc = try vm.fromVal(Procedure, proc_val);

    try testing.expectEqual(1, proc.implementation.bytecode.instructions.len);
    try testing.expectEqual(
        Instruction.initLoad(Val.init(true)),
        proc.implementation.bytecode.instructions[0],
    );
}

test "compile quote with list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc_val = try compile(&vm, try vm.builder().readOne("'(1 2 3)"));
    const proc = try vm.fromVal(Procedure, proc_val);

    try testing.expectEqual(1, proc.implementation.bytecode.instructions.len);
    const instruction = proc.implementation.bytecode.instructions[0];
    try testing.expectEqual(.load, std.meta.activeTag(instruction.repr));

    // Check that the loaded value is the list (1 2 3)
    const loaded_val = instruction.repr.load;
    try testing.expectFmt(
        "(1 2 3)",
        "{f}",
        .{vm.pretty(loaded_val)},
    );
}

test "compile quote with empty list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc_val = try compile(&vm, try vm.builder().readOne("'()"));
    const proc = try vm.fromVal(Procedure, proc_val);

    try testing.expectEqual(1, proc.implementation.bytecode.instructions.len);
    const instruction = proc.implementation.bytecode.instructions[0];
    try testing.expectEqual(.load, std.meta.activeTag(instruction.repr));

    // Check that the loaded value is nil (empty list)
    const loaded_val = instruction.repr.load;
    try testing.expectFmt(
        "()",
        "{f}",
        .{vm.pretty(loaded_val)},
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
