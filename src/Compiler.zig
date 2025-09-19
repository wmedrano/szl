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

/// Table of commonly used symbols that are pre-interned for efficient compilation.
///
/// This struct contains interned versions of symbols that the compiler needs
/// to recognize and handle specially during compilation. By pre-interning these
/// symbols, the compiler can perform fast symbol comparisons without needing
/// to intern symbols repeatedly during compilation.
const SymbolTable = struct {
    define: Symbol.Interned,
    @"szl-define": Symbol.Interned,
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
///
/// Args:
///   self: The compiler instance to clean up.
fn deinit(self: *Compiler) void {
    self.instructions.deinit(self.vm.allocator);
}

/// Adds an instruction to the compiler's instruction list.
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

/// Compiles a single expression into bytecode instructions.
///
/// Args:
///   self: The compiler instance.
///   expr: The expression to compile.
///
/// Returns:
///   An error if the expression cannot be compiled.
fn compileOne(self: *Compiler, expr: Val) Error!void {
    switch (expr.repr) {
        .boolean, .i64, .procedure => try self.addInstruction(Instruction.initLoad(expr)),
        .nil => return error.InvalidExpression,
        .symbol => |s| return self.compileSymbol(s),
        .pair => |p| {
            const pair = self.vm.inspector().resolve(Pair, p) catch return Error.InvalidExpression;
            var args_iter = self.vm.inspector().iterList(pair.cdr) catch return Error.InvalidExpression;
            try self.compileExpression(pair.car, &args_iter);
        },
    }
}

/// Compiles a symbol by generating a GetGlobal instruction.
///
/// Args:
///   self: The compiler instance.
///   symbol: The interned symbol to look up.
///
/// Returns:
///   An error if memory allocation fails.
fn compileSymbol(self: *Compiler, symbol: Symbol.Interned) Error!void {
    try self.addInstruction(Instruction.initGetGlobal(symbol));
}

/// Compiles a function call expression with arguments.
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
        if (self.symbols.define.eql(sym)) return self.compileDefine(args_iter);
    }
    try self.compileOne(leading);
    var arg_count: usize = 0;
    while (args_iter.next() catch return Error.InvalidExpression) |arg| {
        try self.compileOne(arg);
        arg_count += 1;
    }
    try self.addInstruction(Instruction.initEvalProcedure(arg_count));
}

/// Compiles a define expression into bytecode instructions.
///
/// Args:
///   self: The compiler instance.
///   iter: Iterator over the arguments to the define expression (symbol and value).
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

fn compileDefineProc(self: *Compiler, signature: *Inspector.ListIterator, body: *Inspector.ListIterator) Error!void {
    const name = signature.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;
    const name_sym = self.vm.fromVal(Symbol.Interned, name) catch
        return Error.InvalidExpression;
    // TODO: Support arguments.
    if (!signature.isEmpty()) return Error.InvalidExpression;

    var sub_compiler = Compiler{
        .vm = self.vm,
        .symbols = self.symbols,
    };
    defer sub_compiler.deinit();
    while (body.next() catch return Error.InvalidExpression) |expr| {
        try sub_compiler.compileOne(expr);
    }
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
