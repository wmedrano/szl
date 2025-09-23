//! A compiler that transforms Lisp expressions into bytecode instructions.
//!
//! The compiler builds a sequence of instructions that can be executed by the
//! virtual machine.
const std = @import("std");
const testing = std.testing;

const Inspector = @import("../Inspector.zig");
const Instruction = @import("../instruction.zig").Instruction;
const Procedure = @import("../Procedure.zig");
const Pair = @import("../types/Pair.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");
const Scope = @import("Scope.zig");

const Compiler = @This();

/// The virtual machine instance to compile for.
vm: *Vm,
/// Accumulated instructions during compilation.
instructions: std.ArrayList(Instruction) = .{},
/// Local bindings for managing local variable bindings during compilation.
scope: Scope = .{},

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
        .args = self.scope.argCount(),
        .locals_count = self.scope.count(),
        .instructions = instructions,
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
        proc.instructions,
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
        .boolean,
        .i64,
        .f64,
        .char,
        .string,
        .proc,
        .native_proc,
        .vector,
        .bytevector,
        .record,
        .record_type_descriptor,
        => try self.addInstruction(Instruction.initLoad(expr)),
        .nil => return Error.InvalidExpression,
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
        if (self.vm.common_symbols.@"let*".eql(sym))
            return self.compileLet(true, args_iter);
        if (self.vm.common_symbols.begin.eql(sym))
            return self.compileBegin(args_iter);
        if (self.vm.common_symbols.cond.eql(sym))
            return self.compileCond(args_iter);
        if (self.vm.common_symbols.define.eql(sym))
            return self.compileDefine(args_iter);
        if (self.vm.common_symbols.lambda.eql(sym))
            return self.compileLambda(args_iter);
        if (self.vm.common_symbols.let.eql(sym))
            return self.compileLet(false, args_iter);
        if (self.vm.common_symbols.quote.eql(sym))
            return self.compileQuote(args_iter);
    }
    try self.compileOne(leading);
    const arg_count = try self.compileMany(args_iter, .{ .allow_zero = true });
    try self.addInstruction(Instruction.initEvalProc(arg_count));
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

/// Backpatches all jumps with correct distances
fn backpatchJumps(
    self: *Compiler,
    end_jumps: []const usize,
    conditional_jumps: []const usize,
    pop_flags: []const bool,
) void {
    // Backpatch all jump-to-end instructions
    for (end_jumps) |jump_idx| {
        self.instructions.items[jump_idx] =
            Instruction.initJump(jumpDistance(jump_idx + 1, self.instructions.items.len));
    }

    // Backpatch all conditional jumps
    for (conditional_jumps, 0..) |conditional_jump_idx, i| {
        if (i < end_jumps.len and i < pop_flags.len) {
            const target_jump_idx = end_jumps[i];
            const pop_flag = pop_flags[i];
            self.instructions.items[conditional_jump_idx] =
                Instruction.initJumpIfNot(.{ .steps = jumpDistance(conditional_jump_idx, target_jump_idx), .pop = pop_flag });
        }
    }
}

fn compileCond(self: *Compiler, iter: *Inspector.ListIterator) Error!void {
    if (iter.isEmpty()) return Error.InvalidExpression;

    // Track jump locations for backpatching
    var end_jumps = std.ArrayList(usize){};
    defer end_jumps.deinit(self.vm.allocator);
    var conditional_jumps = std.ArrayList(usize){};
    defer conditional_jumps.deinit(self.vm.allocator);
    var pop_flags = std.ArrayList(bool){};
    defer pop_flags.deinit(self.vm.allocator);
    var has_else = false;

    while (iter.next() catch return Error.InvalidExpression) |clause| {
        var clause_iter = self.vm.inspector().iterList(clause) catch return Error.InvalidExpression;
        const test_expr = (clause_iter.next() catch return Error.InvalidExpression) orelse return Error.InvalidExpression;

        // Handle else clause
        if (self.vm.fromVal(Symbol.Interned, test_expr) catch null) |sym| {
            if (self.vm.common_symbols.@"else".eql(sym)) {
                has_else = true;
                _ = try self.compileMany(&clause_iter, .{ .squash = true });
                break;
            }
        }

        // Compile test expression
        try self.compileOne(test_expr);

        // Check for arrow syntax: (test => proc)
        const preview_next = clause_iter.peek() catch {
            return Error.InvalidExpression;
        } orelse Val.init({});
        const is_arrow = if (self.vm.fromVal(Symbol.Interned, preview_next) catch null) |sym|
            self.vm.common_symbols.@"=>".eql(sym)
        else
            false;
        if (is_arrow) {
            _ = clause_iter.next() catch unreachable orelse unreachable;
            const proc_expr = (clause_iter.next() catch return Error.InvalidExpression) orelse return Error.InvalidExpression;
            if (!clause_iter.isEmpty()) return Error.InvalidExpression;

            const jump_if_not_idx = self.instructions.items.len;
            try conditional_jumps.append(self.vm.allocator, jump_if_not_idx);
            try pop_flags.append(self.vm.allocator, false);
            try self.addInstruction(Instruction.initJumpIfNot(.{ .steps = 0, .pop = false }));

            // Compile procedure and apply to test result
            try self.compileOne(proc_expr);
            try self.addInstruction(Instruction.initSwap());
            try self.addInstruction(Instruction.initEvalProc(1));

            try end_jumps.append(self.vm.allocator, self.instructions.items.len);
            try self.addInstruction(Instruction.initJump(0));
        } else {
            const jump_if_not_idx = self.instructions.items.len;
            try conditional_jumps.append(self.vm.allocator, jump_if_not_idx);
            const eval_to_predicate = clause_iter.isEmpty();
            try pop_flags.append(self.vm.allocator, !eval_to_predicate);
            try self.addInstruction(Instruction.initJumpIfNot(.{ .steps = 0 }));

            // Compile body expressions
            if (!eval_to_predicate) {
                _ = try self.compileMany(&clause_iter, .{ .allow_zero = false, .squash = true });
            }

            try end_jumps.append(self.vm.allocator, self.instructions.items.len);
            try self.addInstruction(Instruction.initJump(0));
        }
    }
    if (!iter.isEmpty()) return Error.InvalidExpression;

    // Add fallback when no clause matches (only if no else clause)
    if (!has_else) {
        try self.addInstruction(Instruction.initLoad(Val.init(self.vm.common_symbols.@"*unspecified*")));
    }

    // Backpatch all jumps
    self.backpatchJumps(end_jumps.items, conditional_jumps.items, pop_flags.items);
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

/// Compiles a define expression by pattern matching on the parameters.
/// Handles both variable definitions (define symbol value) and
/// procedure definitions (define (name args...) body...).
/// Dispatches to appropriate compilation method based on parameters type.
///
/// Args:
///   self: The compiler instance.
///   iter: Iterator over the arguments to the define expression.
///
/// Returns:
///   An error if the arguments are invalid or compilation fails.
fn compileDefine(self: *Compiler, iter: *Inspector.ListIterator) Error!void {
    const parameters_val = iter.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;
    switch (parameters_val.repr) {
        .symbol => |sym| return self.compileDefineVal(sym, iter),
        .pair => {
            var parameters = self.vm.inspector().iterList(parameters_val) catch
                return Error.InvalidExpression;
            return self.compileDefineProc(&parameters, iter);
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
    try self.addInstruction(Instruction.initEvalProc(2));
}

/// Compiles a procedure definition into bytecode.
/// Creates a sub-compiler with lexical bindings for parameters,
/// compiles the procedure body, then wraps it in a Procedure value.
/// The resulting procedure is defined globally using szl-define.
///
/// Args:
///   self: The compiler instance.
///   parameters: Iterator over the procedure parameters (name and parameters).
///   body: Iterator over the procedure body expressions.
///
/// Returns:
///   An error if the arguments are invalid or compilation fails.
fn compileDefineProc(self: *Compiler, parameters: *Inspector.ListIterator, body: *Inspector.ListIterator) Error!void {
    var sub_compiler = Compiler{ .vm = self.vm };
    defer sub_compiler.deinit();
    const name = parameters.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;
    const name_sym = self.vm.fromVal(Symbol.Interned, name) catch
        return Error.InvalidExpression;
    _ = try sub_compiler.scope.addBinding(self.vm.allocator, Scope.Binding{
        .name = name_sym,
        .index = -1,
        .type = .proc,
    });
    try self.addInstruction(Instruction.initGetGlobal(self.vm.common_symbols.@"szl-define"));
    try self.addInstruction(Instruction.initLoad(Val.init(name)));
    try self.compileProc(name_sym, parameters, body);
    try self.addInstruction(Instruction.initEvalProc(2));
}

/// Compiles a procedure expression into bytecode.
///
/// This function sets up a new compiler instance with its own lexical scope.
/// It adds each parameter in the parameters as a local argument binding,
/// compiles the body expressions, then converts the result into a `Procedure`.
/// The procedure is then built and loaded as a value for further use.
///
/// This function is used both for compiling `lambda` expressions and named
/// procedures in `define`.
///
/// Args:
///   self: The compiler instance.
///   name: Optional symbol for the procedure name (used in `define`, nil for `lambda`).
///   parameters: Iterator over the procedure’s formal parameters.
///   body: Iterator over the procedure body expressions.
///
/// Returns:
///   An error if parameters or body expressions are invalid or compilation fails.
fn compileProc(self: *Compiler, name: ?Symbol.Interned, parameters: *Inspector.ListIterator, body: *Inspector.ListIterator) Error!void {
    var sub_compiler = Compiler{ .vm = self.vm };
    defer sub_compiler.deinit();
    if (name) |n| {
        _ = try sub_compiler.scope.addBinding(self.vm.allocator, Scope.Binding{
            .name = n,
            .index = -1,
            .type = .proc,
        });
    }
    var arg_count: isize = 0;
    while (parameters.next() catch return Error.InvalidExpression) |arg_val| {
        const arg = self.vm.fromVal(Symbol.Interned, arg_val) catch
            return Error.InvalidExpression;
        _ = try sub_compiler.scope.addBinding(self.vm.allocator, Scope.Binding{
            .name = arg,
            .index = arg_count,
            .type = .argument,
        });
        arg_count += 1;
    }

    _ = try sub_compiler.compileMany(body, .{});
    const proc = try sub_compiler.toProcedure(name);
    const proc_val = self.vm.builder().build(proc) catch return Error.InvalidExpression;

    try self.addInstruction(Instruction.initLoad(proc_val));
}

/// Compiles a lambda expression into bytecode.
/// Creates a sub-compiler with lexical bindings for parameters,
/// compiles the lambda body, then wraps it in a Procedure value.
/// Unlike define procedures, lambdas create anonymous procedures that
/// are loaded directly onto the stack as values.
///
/// Args:
///   self: The compiler instance.
///   iter: Iterator over the lambda arguments (parameter list and body expressions).
///
/// Returns:
///   An error if the arguments are invalid or compilation fails.
fn compileLambda(self: *Compiler, iter: *Inspector.ListIterator) Error!void {
    var sub_compiler = Compiler{ .vm = self.vm };
    defer sub_compiler.deinit();
    const args = iter.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;

    var parameters = self.vm.inspector().iterList(args) catch
        return Error.InvalidExpression;
    try self.compileProc(null, &parameters, iter);
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
fn compileLetBinding(self: *Compiler, binding: Val) Error!Scope.Id {
    const inspector = self.vm.inspector();
    var parts = inspector.iterList(binding) catch return Error.InvalidExpression;

    // Extract variable name from binding
    const name_val = parts.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;

    const name = inspector.to(Symbol.Interned, name_val) catch return Error.InvalidExpression;

    // Create hidden binding for the variable (not yet accessible to other bindings)
    const id = try self.scope.addLocal(self.vm.allocator, name, .hidden);

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
fn compileLet(self: *Compiler, star: bool, iter: *Inspector.ListIterator) Error!void {
    // Extract the bindings list from the let expression
    const bindings_expr = iter.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;

    var let_binding_ids = std.ArrayList(Scope.Id){};
    defer {
        // Clean up bindings when exiting let scope to prevent variable leakage
        for (let_binding_ids.items) |id| self.scope.clear(id);
        let_binding_ids.deinit(self.vm.allocator);
    }

    // Phase 1: Compile all binding expressions while bindings are hidden
    var bindings = self.vm.inspector().iterList(bindings_expr) catch
        return Error.InvalidExpression;
    while (bindings.next() catch return Error.InvalidExpression) |binding| {
        const id = try self.compileLetBinding(binding);
        try let_binding_ids.append(self.vm.allocator, id);
        if (star) self.scope.getBinding(id).?.type = .local;
    }

    // Phase 2: Activate bindings (convert hidden -> local) before evaluating body
    for (let_binding_ids.items) |id| self.scope.getBinding(id).?.type = .local;

    // Compile body expressions with activated bindings available
    _ = try self.compileMany(iter, .{ .squash = true });
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
        Instruction.initEvalProc(2),
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
        Instruction.initEvalProc(2),
        Instruction.initEvalProc(3),
    }, &vm, "(foo 1 2 (bar 3 4))");
}

////////////////////////////////////////////////////////////////////////////////
// Define
////////////////////////////////////////////////////////////////////////////////

test "compile define expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try expectInstructions(&[_]Instruction{
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("szl-define"))),
        Instruction.initLoad(try vm.builder().internStaticVal(Symbol.init("x"))),
        Instruction.initLoad(Val.init(42)),
        Instruction.initEvalProc(2),
    }, &vm, "(define x 42)");
}

test "compile define procedure expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    const expr = try vm.builder().readOne("(define (foo) 42)");
    const proc = try vm.fromVal(Procedure, try compile(&vm, expr));

    const instructions = proc.instructions;

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
        Instruction.initEvalProc(2),
        instructions[3],
    );
}

test "compile define procedure with recursive call" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    const expr = try vm.builder().readOne("(define (foo x) (foo x))");
    const proc = try vm.fromVal(Procedure, try compile(&vm, expr));

    const instructions = proc.instructions;

    try testing.expectEqual(4, instructions.len);
    try testing.expectEqual(
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("szl-define"))),
        instructions[0],
    );
    try testing.expectEqual(
        Instruction.initLoad(try vm.builder().internStaticVal(Symbol.init("foo"))),
        instructions[1],
    );
    const foo_proc = try vm.fromVal(Procedure, proc.instructions[2].load);
    try testing.expectEqualDeep(
        &.{
            Instruction.initGetLocal(-1),
            Instruction.initGetLocal(0),
            Instruction.initEvalProc(1),
        },
        foo_proc.instructions,
    );
    try testing.expectEqual(
        Instruction.initEvalProc(2),
        instructions[3],
    );
}

////////////////////////////////////////////////////////////////////////////////
// If
////////////////////////////////////////////////////////////////////////////////

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
        Instruction.initEvalProc(2),
        Instruction.initJump(6),
        // False branch
        Instruction.initGetGlobal(plus),
        Instruction.initLoad(Val.init(3)),
        Instruction.initLoad(Val.init(4)),
        Instruction.initLoad(Val.init(5)),
        Instruction.initLoad(Val.init(6)),
        Instruction.initEvalProc(4),
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
        Instruction.initEvalProc(2),
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

test "cond without clauses is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Error.InvalidExpression,
        compile(&vm, try vm.builder().readOne("(cond)")),
    );
    try testing.expectError(
        Error.InvalidExpression,
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
    }, &vm, "(cond (#t 42) (else 43))");
}

test "cond with only else expression is expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try expectInstructions(&[_]Instruction{
        Instruction.initLoad(Val.init(43)),
    }, &vm, "(cond (else 43))");
}

////////////////////////////////////////////////////////////////////////////////
// Quote
////////////////////////////////////////////////////////////////////////////////

test "compile quote with integer" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc_val = try compile(&vm, try vm.builder().readOne("'42"));
    const proc = try vm.fromVal(Procedure, proc_val);

    try testing.expectEqual(1, proc.instructions.len);
    try testing.expectEqual(
        Instruction.initLoad(Val.init(42)),
        proc.instructions[0],
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

    try testing.expectEqual(1, proc.instructions.len);
    const instruction = proc.instructions[0];
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

////////////////////////////////////////////////////////////////////////////////
// Begin
////////////////////////////////////////////////////////////////////////////////

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
        Instruction.initEvalProc(2),
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("+"))),
        Instruction.initLoad(Val.init(3)),
        Instruction.initLoad(Val.init(4)),
        Instruction.initEvalProc(2),
        Instruction.initSquash(2),
    }, &vm, "(begin (+ 1 2) (+ 3 4))");
}

////////////////////////////////////////////////////////////////////////////////
// Let
////////////////////////////////////////////////////////////////////////////////

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
        Instruction.initEvalProc(2),
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
        Instruction.initEvalProc(2),
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
        Instruction.initEvalProc(2),
        Instruction.initSetLocal(0),
        // Evaluate body
        Instruction.initGetGlobal(try vm.builder().internStatic(Symbol.init("*"))),
        Instruction.initGetLocal(0),
        Instruction.initLoad(Val.init(2)),
        Instruction.initEvalProc(2),
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
        Instruction.initEvalProc(2),
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

test "compile let with no body is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Error.InvalidExpression,
        compile(&vm, try vm.builder().readOne("(let ((x 1)))")),
    );
}

test "compile let with missing bindings fails" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Error.InvalidExpression,
        compile(&vm, try vm.builder().readOne("(let)")),
    );
}

////////////////////////////////////////////////////////////////////////////////
// Lambda
////////////////////////////////////////////////////////////////////////////////

test "compile lambda with single parameter" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc_val = try compile(&vm, try vm.builder().readOne("(lambda (x) x)"));
    const proc = try vm.fromVal(Procedure, proc_val);

    // Should generate one Load instruction containing a procedure
    try testing.expectEqual(1, proc.instructions.len);
    try testing.expectEqual(.load, std.meta.activeTag(proc.instructions[0]));
    try testing.expectEqual(.proc, std.meta.activeTag(proc.instructions[0].load.repr));
}

test "compile lambda with multiple parameters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc_val = try compile(&vm, try vm.builder().readOne("(lambda (x y) (+ x y))"));
    const proc = try vm.fromVal(Procedure, proc_val);

    // Should generate one Load instruction containing a procedure
    try testing.expectEqual(1, proc.instructions.len);
    try testing.expectEqual(.load, std.meta.activeTag(proc.instructions[0]));
    try testing.expectEqual(.proc, std.meta.activeTag(proc.instructions[0].load.repr));

    // Check the loaded procedure has correct properties
    const loaded_proc = try vm.fromVal(Procedure, proc.instructions[0].load);
    try testing.expectEqual(@as(usize, 2), loaded_proc.args);
    try testing.expectEqual(@as(usize, 2), loaded_proc.locals_count);
}

test "compile lambda with no parameters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc_val = try compile(&vm, try vm.builder().readOne("(lambda () 42)"));
    const proc = try vm.fromVal(Procedure, proc_val);

    // Should have one instruction that loads the procedure
    try testing.expectEqual(1, proc.instructions.len);
    try testing.expectEqual(.load, std.meta.activeTag(proc.instructions[0]));
}

test "compile lambda used in function position" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc_val = try compile(&vm, try vm.builder().readOne("((lambda (x) (+ x 1)) 5)"));
    const proc = try vm.fromVal(Procedure, proc_val);

    // Should compile the lambda, load 5, then evaluate
    try testing.expect(proc.instructions.len > 2);
    const last_instruction = proc.instructions[proc.instructions.len - 1];
    try testing.expectEqual(.eval_proc, std.meta.activeTag(last_instruction));
    try testing.expectEqual(@as(usize, 1), last_instruction.eval_proc);
}

test "compile lambda with multiple body expressions" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const proc_val = try compile(&vm, try vm.builder().readOne("(lambda (x) x (+ x 1))"));
    const proc = try vm.fromVal(Procedure, proc_val);

    try testing.expectEqual(1, proc.instructions.len);
    try testing.expectEqual(.load, std.meta.activeTag(proc.instructions[0]));
}

test "compile lambda with invalid parameter list fails" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test lambda with non-symbol parameter
    try testing.expectError(
        Error.InvalidExpression,
        compile(&vm, try vm.builder().readOne("(lambda (42) x)")),
    );
}

test "compile lambda with missing parameter list fails" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Error.InvalidExpression,
        compile(&vm, try vm.builder().readOne("(lambda)")),
    );
}

test "compile lambda with missing body returns InvalidExpression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Error.InvalidExpression,
        compile(&vm, try vm.builder().readOne("(lambda (x))")),
    );
}

test "lambda expressions integrate with evaluation" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test simple lambda evaluation
    try vm.expectEval("6", "((lambda (x) (+ x 1)) 5)");

    // Test lambda with multiple parameters
    try vm.expectEval("11", "((lambda (x y) (+ x y)) 5 6)");

    // Test lambda with no parameters
    try vm.expectEval("42", "((lambda () 42))");
}

////////////////////////////////////////////////////////////////////////////////
// E2E
////////////////////////////////////////////////////////////////////////////////
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

test "cond" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1", "(cond ((> 3 2) 1) ((< 3 2) 2))");
    try vm.expectEval("2", "(cond ((< 3 2) 1) ((> 3 2) 2))");
    try vm.expectEval("else-branch", "(cond ((< 3 2) 'no) (else 'else-branch))");
    try vm.expectEval("*unspecified*", "(cond ((< 1 0) 'no))");
    try vm.expectEval("ok", "(cond ((> 3 2) 'not-ok 'ok) (else 'fail))");
    try vm.expectEval("GT", "(cond ((> 5 3) => (lambda (x) 'GT)) (else 'fail))");
    try vm.expectEval("inner", "(cond ((= 1 1) (cond ((= 2 2) 'inner))))");
    try vm.expectEval("8", "(cond ((+ 3 5) => (lambda (val) val)))");
    try vm.expectEval("test", "(cond ('test))");
}

test "let" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEvalErr("undefined-variable", "(let ((x 1) (y x)) y)");
    try vm.expectEval("3", "(let ((x 1) (y 2)) (+ x y))"); // 1 + 2 = 3
    try vm.expectEval("x", "(let ((x 'x)) x)"); // Symbol x
    try vm.expectEval("3", "(let ((x 2)) (let ((x 3)) x))"); // inner x = 3
    try vm.expectEval("5", "(let ((x 2) (y 3)) (+ x y))");
}

test "let*" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEvalErr("undefined-variable", "(let* ((x z)) x)");
    try vm.expectEval("3", "(let* ((x 1) (y (+ x 2))) y)"); // y = 1 + 2 = 3
    try vm.expectEval("6", "(let* ((a 1) (b 2) (c (+ a b)) (d (* c 2))) d)"); // d = (1+2)*2 = 6
    try vm.expectEval("x", "(let* ((x 'x)) x)"); // symbol x
}
