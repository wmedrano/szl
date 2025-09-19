//! A compiler that transforms Lisp expressions into bytecode instructions.
//!
//! The compiler builds a sequence of instructions that can be executed by the
//! virtual machine.
const std = @import("std");
const testing = std.testing;

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
        .pair => {
            const pair = self.vm.inspector().to(Pair, expr) catch return Error.InvalidExpression;
            try self.compileExpression(pair.car, pair.cdr);
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
///   args: The arguments as a linked list of pairs.
///
/// Returns:
///   An error if any part of the expression cannot be compiled.
fn compileExpression(self: *Compiler, leading: Val, args: Val) Error!void {
    if (self.vm.fromVal(Symbol.Interned, leading) catch null) |sym| {
        if (self.symbols.define.eql(sym)) return self.compileDefine(args);
    }
    try self.compileOne(leading);
    var next_args = args;
    var arg_count: usize = 0;
    while (!next_args.isNil()) {
        const next_pair = self.vm.inspector().to(Pair, next_args) catch return Error.InvalidExpression;
        const next = next_pair.car;
        next_args = next_pair.cdr;
        try self.compileOne(next);
        arg_count += 1;
    }
    try self.addInstruction(Instruction.initEvalProcedure(arg_count));
}

/// Compiles a define expression into bytecode instructions.
///
/// Args:
///   self: The compiler instance.
///   args: The arguments to the define expression (symbol and value).
///
/// Returns:
///   An error if the arguments are invalid or compilation fails.
fn compileDefine(self: *Compiler, args: Val) Error!void {
    var pair = self.vm.fromVal(Pair, args) catch return Error.InvalidExpression;
    const define_sym = self.vm.fromVal(Symbol.Interned, pair.car) catch return Error.InvalidExpression;
    pair = self.vm.fromVal(Pair, pair.cdr) catch return Error.InvalidExpression;
    const define_expr = pair.car;
    if (!pair.cdr.isNil()) return Error.InvalidExpression;
    try self.addInstruction(Instruction.initGetGlobal(self.symbols.@"szl-define"));
    try self.addInstruction(Instruction.initLoad(Val.init(define_sym)));
    try self.compileOne(define_expr);
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
