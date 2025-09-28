//! Intermediate Representation (IR) builder for the Scheme compiler.
//!
//! The IrBuilder transforms parsed Scheme expressions into an intermediate
//! representation that can be more easily compiled to bytecode instructions.
//! It handles special forms like if, begin, define, lambda, let, and quote,
//! converting them to structured IR nodes.

const std = @import("std");
const testing = std.testing;

const Inspector = @import("../Inspector.zig");
const Char = @import("../types/Char.zig");
const Pair = @import("../types/Pair.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

const IrBuilder = @This();

/// The virtual machine instance to compile for.
vm: *Vm,
/// The arena allocator for all `Ir` allocations.
arena: *std.heap.ArenaAllocator,

/// Errors that can occur during IR building.
pub const Error = error{
    /// The expression cannot be compiled (e.g., nil values or malformed expressions).
    InvalidExpression,
    /// Memory allocation failed during IR construction.
    OutOfMemory,
};

/// A procedure call expression definition.
pub const ProcCall = struct {
    /// The procedure expression to call.
    proc: *const Ir,
    /// The argument expressions to pass to the procedure.
    args: []const Ir,
};

// Forward declare Ir for use in the struct definitions
pub const Ir = union(enum) {
    const_val: Val,
    get: Symbol.Interned,
    proc_call: ProcCall,
    @"if": IfDef,
    body: []const Ir,
    define: struct {
        symbol: Symbol.Interned,
        expr: *const Ir,
    },
    lambda: LambdaDef,
    let: LetDef,
};

/// A binding in a let or let* expression.
pub const LetBinding = struct {
    /// The variable name being bound.
    name: Symbol.Interned,
    /// The expression whose value will be bound to the variable.
    expr: Ir,
};

/// An if conditional expression definition.
pub const IfDef = struct {
    /// The test expression to evaluate for truthiness.
    @"test": *const Ir,
    /// The expression to evaluate if the test is truthy.
    true: *const Ir,
    /// The expression to evaluate if the test is falsy.
    false: *const Ir,
};

/// A lambda (procedure) definition.
pub const LambdaDef = struct {
    /// Optional name for the procedure (used for recursion).
    name: ?Symbol.Interned,
    /// Parameter names for the procedure.
    args: []const Symbol.Interned,
    /// The procedure body expression.
    body: *const Ir,
};

/// A let or let* binding construct definition.
pub const LetDef = struct {
    /// True if this is `let*` (sequential binding) as opposed to `let` (parallel binding).
    star: bool,
    /// The variable bindings to establish.
    bindings: []const LetBinding,
    /// The body expression to evaluate with the bindings in scope.
    body: *const Ir,
};

/// Creates a new IrBuilder instance.
///
/// Args:
///   vm: The virtual machine instance to use for compilation.
///   arena: The arena allocator for temporary allocations during IR building.
///
/// Returns:
///   A new IrBuilder instance ready for use.
pub fn init(vm: *Vm, arena: *std.heap.ArenaAllocator) IrBuilder {
    return IrBuilder{ .vm = vm, .arena = arena };
}

/// Allocates an IR node on the arena and returns a pointer to it.
///
/// Args:
///   self: The IrBuilder instance.
///   ir: The IR node to allocate.
///
/// Returns:
///   A pointer to the allocated IR node.
///
/// Errors:
///   - OutOfMemory if allocation fails.
fn allocIr(self: IrBuilder, ir: Ir) !*Ir {
    const ptr = try self.arena.allocator().create(Ir);
    ptr.* = ir;
    return ptr;
}

/// Builds an IR node from a parsed Scheme expression.
///
/// This is the main entry point for converting parsed Scheme expressions into
/// intermediate representation. It handles different types of values:
/// - Literal values (booleans, numbers, strings, etc.) become const_val nodes
/// - Symbols become get nodes for variable references
/// - Lists are processed as expressions (procedure calls or special forms)
/// - nil values are invalid and return an error
///
/// Args:
///   self: The IrBuilder instance.
///   expr: The parsed Scheme expression to convert to IR.
///
/// Returns:
///   An IR node representing the expression.
///
/// Errors:
///   - InvalidExpression if the expression cannot be converted to valid IR.
///   - OutOfMemory if allocation fails during IR construction.
pub fn build(self: IrBuilder, expr: Val) Error!Ir {
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
        .restore_continuation,
        .operator,
        => return Ir{ .const_val = expr },
        .nil => return Error.InvalidExpression,
        .symbol => |s| return Ir{ .get = s },
        .pair => |p| {
            const pair = self.vm.inspector().resolve(Pair, p) catch return Error.InvalidExpression;
            var args_iter = self.vm.inspector().iterList(pair.cdr) catch return Error.InvalidExpression;
            return self.buildExpression(pair.car, &args_iter);
        },
    }
}

/// Builds a slice of IR nodes from a list of expressions.
///
/// Args:
///   self: The IrBuilder instance.
///   iter: Iterator over the list of expressions to build.
///
/// Returns:
///   A slice of IR nodes allocated on the arena.
///
/// Errors:
///   - InvalidExpression if any expression in the list is invalid.
///   - OutOfMemory if allocation fails.
fn buildManySlice(self: IrBuilder, iter: *Inspector.ListIterator) Error![]Ir {
    var irs = std.ArrayList(Ir){};
    while (iter.next() catch return Error.InvalidExpression) |expr| {
        const arg = try self.build(expr);
        try irs.append(self.arena.allocator(), arg);
    }
    return irs.items;
}

/// Builds a body IR node from a sequence of expressions.
///
/// Args:
///   self: The IrBuilder instance.
///   iter: Iterator over the expressions to include in the body.
///
/// Returns:
///   A body IR node containing the sequence of expressions.
///
/// Errors:
///   - InvalidExpression if any expression in the list is invalid.
///   - OutOfMemory if allocation fails.
fn buildBegin(self: IrBuilder, iter: *Inspector.ListIterator) Error!Ir {
    return Ir{ .body = try self.buildManySlice(iter) };
}

/// Builds an IR node for an expression starting with a leading value and arguments.
///
/// This function handles special forms (if, begin, define, lambda, let, let*, quote)
/// as well as regular procedure calls. It examines the leading value to determine
/// the type of expression and delegates to appropriate specialized builders.
///
/// Args:
///   self: The IrBuilder instance.
///   leading: The first element of the expression (usually a symbol or procedure).
///   args_iter: Iterator over the arguments/rest of the expression.
///
/// Returns:
///   An IR node representing the expression.
///
/// Errors:
///   - InvalidExpression if the expression is malformed or unsupported.
///   - OutOfMemory if allocation fails during construction.
fn buildExpression(self: IrBuilder, leading: Val, args_iter: *Inspector.ListIterator) Error!Ir {
    if (self.vm.fromVal(Symbol.Interned, leading) catch null) |sym| {
        if (self.vm.common_symbols.cond.eql(sym))
            return try self.buildCond(args_iter);
        if (self.vm.common_symbols.@"if".eql(sym))
            return try self.buildIf(args_iter);
        if (self.vm.common_symbols.begin.eql(sym))
            return try self.buildBegin(args_iter);
        if (self.vm.common_symbols.define.eql(sym))
            return self.buildDefine(args_iter);
        if (self.vm.common_symbols.lambda.eql(sym)) {
            const args = args_iter.next() catch {
                return Error.InvalidExpression;
            } orelse return Error.InvalidExpression;
            var parameters = self.vm.inspector().iterList(args) catch
                return Error.InvalidExpression;
            return self.buildLambda(null, &parameters, args_iter);
        }
        if (self.vm.common_symbols.let.eql(sym))
            return self.buildLet(false, args_iter);
        if (self.vm.common_symbols.@"let*".eql(sym))
            return self.buildLet(true, args_iter);
        if (self.vm.common_symbols.quote.eql(sym)) {
            const val = args_iter.next() catch {
                return Error.InvalidExpression;
            } orelse return Error.InvalidExpression;
            if (!args_iter.isEmpty()) return Error.InvalidExpression;
            return Ir{ .const_val = val };
        }
    }

    return Ir{
        .proc_call = .{
            .proc = try self.allocIr(try self.build(leading)),
            .args = try self.buildManySlice(args_iter),
        },
    };
}

fn buildCond(self: IrBuilder, clauses: *Inspector.ListIterator) Error!Ir {
    const test_sym = self.vm.builder().internStatic(Symbol.init("szl-internal-test-result")) catch
        return Error.InvalidExpression;
    const clause = clauses.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;
    var clause_iter = self.vm.inspector().iterList(clause) catch {
        return Error.InvalidExpression;
    };
    const test_val = clause_iter.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;
    // Case 1: Else clause
    if (test_val.eq(self.vm.common_symbols.@"else".toVal())) {
        // Must be the final clause.
        if (clauses.next() catch return Error.InvalidExpression) |_|
            return Error.InvalidExpression;
        // Case 1.1: Else statement with an =>.
        if (clause_iter.takeIfEq(self.vm.common_symbols.@"=>".toVal()) catch return Error.InvalidExpression) {
            const proc_expr = clause_iter.next() catch {
                return Error.InvalidExpression;
            } orelse return Error.InvalidExpression;
            if (!clause_iter.isEmpty()) return Error.InvalidExpression;
            return Ir{
                .proc_call = .{
                    .proc = try self.allocIr(try self.build(proc_expr)),
                    .args = &.{},
                },
            };
        }
        // Case 1.2: Else statement followed by expression.
        return self.buildBegin(&clause_iter);
    }
    const test_ir = try self.build(test_val);
    // Case 2: Single test statement
    if (clause_iter.isEmpty()) {
        // TODO: Do not make a named temporary variable. This should be trivial
        // once an "or" Ir variant is implemented.
        return Ir{
            .let = .{
                .star = false,
                .bindings = try self.arena.allocator().dupe(LetBinding, &.{
                    .{ .name = test_sym, .expr = test_ir },
                }),
                .body = try self.allocIr(
                    Ir{
                        .@"if" = .{
                            .@"test" = try self.allocIr(Ir{ .get = test_sym }),
                            .true = try self.allocIr(Ir{ .get = test_sym }),
                            .false = try self.allocIr(try self.buildCond(clauses)),
                        },
                    },
                ),
            },
        };
    }
    // Case 3: Test with =>
    if (clause_iter.takeIfEq(self.vm.common_symbols.@"=>".toVal()) catch return Error.InvalidExpression) {
        const proc_expr = clause_iter.next() catch {
            return Error.InvalidExpression;
        } orelse return Error.InvalidExpression;
        if (!clause_iter.isEmpty()) return Error.InvalidExpression;
        return Ir{
            .let = .{
                .star = false,
                .bindings = try self.arena.allocator().dupe(LetBinding, &.{
                    .{ .name = test_sym, .expr = test_ir },
                }),
                .body = try self.allocIr(
                    Ir{
                        .@"if" = .{
                            // TODO: Do not make a named temporary variable.
                            .@"test" = try self.allocIr(Ir{ .get = test_sym }),
                            .true = try self.allocIr(Ir{
                                .proc_call = .{
                                    .proc = try self.allocIr(try self.build(proc_expr)),
                                    .args = &.{Ir{ .get = test_sym }},
                                },
                            }),
                            .false = try self.allocIr(try self.buildCond(clauses)),
                        },
                    },
                ),
            },
        };
    }
    // Case 4: Test with expressions.
    return Ir{
        .@"if" = .{
            .@"test" = try self.allocIr(test_ir),
            .true = try self.allocIr(try self.buildBegin(&clause_iter)),
            .false = try self.allocIr(try self.buildCond(clauses)),
        },
    };
}

/// Builds an if conditional IR node.
///
/// Args:
///   self: The IrBuilder instance.
///   args_iter: Iterator over the if expression arguments (test, true, false).
///
/// Returns:
///   An if IR node containing the test, true, and false expressions.
///
/// Errors:
///   - InvalidExpression if the arguments are malformed.
///   - OutOfMemory if allocation fails.
fn buildIf(self: IrBuilder, args_iter: *Inspector.ListIterator) Error!Ir {
    const test_expr = args_iter.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;
    const true_expr = args_iter.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;
    const false_expr = args_iter.next() catch {
        return Error.InvalidExpression;
    };
    const false_ir = if (false_expr) |expr|
        try self.build(expr)
    else
        Ir{ .const_val = Val.init({}) };
    return Ir{
        .@"if" = .{
            .@"test" = try self.allocIr(try self.build(test_expr)),
            .true = try self.allocIr(try self.build(true_expr)),
            .false = try self.allocIr(false_ir),
        },
    };
}

/// Builds a lambda (procedure definition) IR node.
///
/// Args:
///   self: The IrBuilder instance.
///   name: Optional name for the procedure (used for recursive calls).
///   parameters: Iterator over the parameter symbols for the lambda.
///   body: Iterator over the expressions forming the lambda body.
///
/// Returns:
///   A lambda IR node containing the procedure definition.
///
/// Errors:
///   - InvalidExpression if the parameter list or body is malformed.
///   - OutOfMemory if allocation fails.
fn buildLambda(self: IrBuilder, name: ?Symbol.Interned, parameters: *Inspector.ListIterator, body: *Inspector.ListIterator) Error!Ir {
    var args_list = std.ArrayList(Symbol.Interned){};
    while (parameters.next() catch return Error.InvalidExpression) |v| {
        const symbol = self.vm.inspector().to(Symbol.Interned, v) catch
            return Error.InvalidExpression;
        try args_list.append(self.arena.allocator(), symbol);
    }
    return Ir{
        .lambda = .{
            .name = name,
            .args = args_list.items,
            .body = try self.allocIr(try self.buildBegin(body)),
        },
    };
}

/// Builds a let or let* binding construct IR node.
///
/// Args:
///   self: The IrBuilder instance.
///   star: True if this is a let* (sequential binding), false for let (parallel binding).
///   expr_iter: Iterator over the let expression arguments (bindings and body).
///
/// Returns:
///   A let IR node containing the bindings and body.
///
/// Errors:
///   - InvalidExpression if the bindings or body are malformed.
///   - OutOfMemory if allocation fails.
fn buildLet(self: IrBuilder, star: bool, expr_iter: *Inspector.ListIterator) !Ir {
    const inspector = self.vm.inspector();
    var bindings = std.ArrayList(LetBinding){};

    const bindings_val = expr_iter.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;

    // Bindings
    var bindings_iter = inspector.iterList(bindings_val) catch
        return Error.InvalidExpression;
    while (bindings_iter.next() catch return Error.InvalidExpression) |binding_val| {
        var binding_iter = inspector.iterList(binding_val) catch
            return Error.InvalidExpression;
        const sym_val = binding_iter.next() catch {
            return Error.InvalidExpression;
        } orelse return Error.InvalidExpression;
        const expr_val = binding_iter.next() catch {
            return Error.InvalidExpression;
        } orelse return Error.InvalidExpression;
        if (!binding_iter.isEmpty()) return Error.InvalidExpression;
        try bindings.append(
            self.arena.allocator(),
            LetBinding{
                .name = inspector.to(Symbol.Interned, sym_val) catch return Error.InvalidExpression,
                .expr = try self.build(expr_val),
            },
        );
    }

    // Body
    const body = try self.buildBegin(expr_iter);
    return Ir{
        .let = .{
            .star = star,
            .bindings = bindings.items,
            .body = try self.allocIr(body),
        },
    };
}

/// Builds a define expression IR node.
///
/// Handles both variable definitions (define x value) and function definitions
/// (define (name args...) body...). Function definitions are converted to
/// lambda expressions internally.
///
/// Args:
///   self: The IrBuilder instance.
///   args_iter: Iterator over the define arguments.
///
/// Returns:
///   A define IR node containing the symbol and expression.
///
/// Errors:
///   - InvalidExpression if the definition is malformed.
///   - OutOfMemory if allocation fails.
fn buildDefine(self: IrBuilder, args_iter: *Inspector.ListIterator) Error!Ir {
    const leading_val = args_iter.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;
    switch (leading_val.repr) {
        .symbol => |s| {
            const expr_val = args_iter.next() catch {
                return Error.InvalidExpression;
            } orelse return Error.InvalidExpression;
            if (!args_iter.isEmpty()) return Error.InvalidExpression;
            return Ir{
                .define = .{
                    .symbol = s,
                    .expr = try self.allocIr(try self.build(expr_val)),
                },
            };
        },
        else => {
            return self.buildDefineProc(leading_val, args_iter);
        },
    }
}

/// Builds a procedure definition from a define form.
///
/// Handles function definitions of the form (define (name args...) body...).
/// Extracts the procedure name from the parameter list and builds a lambda
/// with the remaining parameters.
///
/// Args:
///   self: The IrBuilder instance.
///   params: The parameter list starting with the function name.
///   body: Iterator over the function body expressions.
///
/// Returns:
///   A define IR node containing the function name and a lambda expression.
///
/// Errors:
///   - InvalidExpression if the parameter list or body is malformed.
///   - OutOfMemory if allocation fails.
fn buildDefineProc(self: IrBuilder, params: Val, body: *Inspector.ListIterator) Error!Ir {
    const inspector = self.vm.inspector();
    var params_iter = inspector.iterList(params) catch return Error.InvalidExpression;
    const name = params_iter.next() catch {
        return Error.InvalidExpression;
    } orelse return Error.InvalidExpression;
    const name_sym = inspector.to(Symbol.Interned, name) catch return Error.InvalidExpression;
    const lambda = try self.buildLambda(name_sym, &params_iter, body);
    return Ir{
        .define = .{
            .symbol = name_sym,
            .expr = try self.allocIr(lambda),
        },
    };
}

test "build if expression with missing false branch uses nil" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const expr = try vm.builder().readOne("(if #t 42)");
    const builder = IrBuilder.init(&vm, &arena);
    const ir = try builder.build(expr);
    try std.testing.expectEqualDeep(
        Ir{
            .@"if" = .{
                .@"test" = &Ir{ .const_val = Val.init(true) },
                .true = &Ir{ .const_val = Val.init(42) },
                .false = &Ir{ .const_val = Val.init({}) },
            },
        },
        ir,
    );
}

test "build let expression is let Ir" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const expr = try vm.builder().readOne("(let ((x 1) (y 2)) y)");
    const builder = IrBuilder.init(&vm, &arena);
    const ir = try builder.build(expr);
    try testing.expectEqualDeep(
        Ir{
            .let = .{
                .star = false,
                .bindings = &.{
                    LetBinding{
                        .name = try vm.builder().internStatic(Symbol.init("x")),
                        .expr = Ir{ .const_val = Val.init(1) },
                    },
                    LetBinding{
                        .name = try vm.builder().internStatic(Symbol.init("y")),
                        .expr = Ir{ .const_val = Val.init(2) },
                    },
                },
                .body = &Ir{
                    .body = &.{
                        Ir{ .get = try vm.builder().internStatic(Symbol.init("y")) },
                    },
                },
            },
        },
        ir,
    );
}

test "build let* expression has star set" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const expr = try vm.builder().readOne("(let* ((x 1) (y 2)) y)");
    const builder = IrBuilder.init(&vm, &arena);
    const ir = try builder.build(expr);
    try testing.expectEqualDeep(
        Ir{
            .let = .{
                .star = true,
                .bindings = &.{
                    LetBinding{
                        .name = try vm.builder().internStatic(Symbol.init("x")),
                        .expr = Ir{ .const_val = Val.init(1) },
                    },
                    LetBinding{
                        .name = try vm.builder().internStatic(Symbol.init("y")),
                        .expr = Ir{ .const_val = Val.init(2) },
                    },
                },
                .body = &Ir{
                    .body = &.{
                        Ir{ .get = try vm.builder().internStatic(Symbol.init("y")) },
                    },
                },
            },
        },
        ir,
    );
}

// ========== Constant Value Tests ==========

test "build boolean constant" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const true_expr = try vm.builder().readOne("#t");
    const true_ir = try builder.build(true_expr);
    try testing.expectEqualDeep(Ir{ .const_val = Val.init(true) }, true_ir);

    const false_expr = try vm.builder().readOne("#f");
    const false_ir = try builder.build(false_expr);
    try testing.expectEqualDeep(Ir{ .const_val = Val.init(false) }, false_ir);
}

test "build integer constant" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("42");
    const ir = try builder.build(expr);
    try testing.expectEqualDeep(Ir{ .const_val = Val.init(42) }, ir);
}

test "build float constant" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("3.14");
    const ir = try builder.build(expr);
    try testing.expectEqualDeep(Ir{ .const_val = Val.init(3.14) }, ir);
}

test "build character constant" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("#\\a");
    const ir = try builder.build(expr);
    try testing.expectEqualDeep(
        Ir{ .const_val = Val.init(Char.init('a')) },
        ir,
    );
}

test "build string constant" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("\"hello\"");
    const ir = try builder.build(expr);
    try testing.expect(ir == .const_val);
    try testing.expect(ir.const_val.repr == .string);
}

test "build nil returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);
    const nil_val = Val.init({});
    try testing.expectError(Error.InvalidExpression, builder.build(nil_val));
}

// ========== Symbol Reference Tests ==========

test "build symbol reference" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("x");
    const ir = try builder.build(expr);

    const expected_symbol = try vm.builder().internStatic(Symbol.init("x"));
    try testing.expectEqualDeep(
        Ir{ .get = expected_symbol },
        ir,
    );
}

test "build multiple symbol references" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const symbols = [_][]const u8{ "foo", "bar", "baz", "hello-world" };
    for (symbols) |symbol_name| {
        const expr = try vm.builder().readOne(symbol_name);
        const ir = try builder.build(expr);

        const expected_symbol = try vm.builder().internStatic(Symbol.init(symbol_name));
        try testing.expectEqualDeep(
            Ir{ .get = expected_symbol },
            ir,
        );
    }
}

// ========== Procedure Call Tests ==========

test "build procedure call with no arguments" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(foo)");
    const ir = try builder.build(expr);

    const expected_symbol = try vm.builder().internStatic(Symbol.init("foo"));
    try testing.expectEqualDeep(
        Ir{
            .proc_call = .{
                .proc = &Ir{ .get = expected_symbol },
                .args = &.{},
            },
        },
        ir,
    );
}

test "build procedure call with arguments" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(+ 1 2 3)");
    const ir = try builder.build(expr);

    const expected_symbol = try vm.builder().internStatic(Symbol.init("+"));
    try testing.expectEqualDeep(
        Ir{
            .proc_call = .{
                .proc = &Ir{ .get = expected_symbol },
                .args = &.{
                    Ir{ .const_val = Val.init(@as(i64, 1)) },
                    Ir{ .const_val = Val.init(@as(i64, 2)) },
                    Ir{ .const_val = Val.init(@as(i64, 3)) },
                },
            },
        },
        ir,
    );
}

test "build nested procedure call" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(+ (- 5 2) (* 3 4))");
    const ir = try builder.build(expr);

    const plus_symbol = try vm.builder().internStatic(Symbol.init("+"));
    const minus_symbol = try vm.builder().internStatic(Symbol.init("-"));
    const mult_symbol = try vm.builder().internStatic(Symbol.init("*"));
    try testing.expectEqualDeep(
        Ir{
            .proc_call = .{
                .proc = &Ir{ .get = plus_symbol },
                .args = &.{
                    Ir{
                        .proc_call = .{
                            .proc = &Ir{ .get = minus_symbol },
                            .args = &.{
                                Ir{ .const_val = Val.init(@as(i64, 5)) },
                                Ir{ .const_val = Val.init(@as(i64, 2)) },
                            },
                        },
                    },
                    Ir{
                        .proc_call = .{
                            .proc = &Ir{ .get = mult_symbol },
                            .args = &.{
                                Ir{ .const_val = Val.init(@as(i64, 3)) },
                                Ir{ .const_val = Val.init(@as(i64, 4)) },
                            },
                        },
                    },
                },
            },
        },
        ir,
    );
}

// ========== Control Flow Tests ==========

test "build if with both branches" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(if #t 1 2)");
    const ir = try builder.build(expr);

    try testing.expect(ir == .@"if");
    try testing.expectEqualDeep(Ir{ .const_val = Val.init(true) }, ir.@"if".@"test".*);
    try testing.expectEqualDeep(Ir{ .const_val = Val.init(@as(i64, 1)) }, ir.@"if".true.*);
    try testing.expectEqualDeep(Ir{ .const_val = Val.init(@as(i64, 2)) }, ir.@"if".false.*);
}

test "build nested if expressions" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(if #t (if #f 1 2) 3)");
    const ir = try builder.build(expr);

    try testing.expectEqualDeep(
        Ir{
            .@"if" = .{
                .@"test" = &Ir{ .const_val = Val.init(true) },
                .true = &Ir{
                    .@"if" = .{
                        .@"test" = &Ir{ .const_val = Val.init(false) },
                        .true = &Ir{ .const_val = Val.init(@as(i64, 1)) },
                        .false = &Ir{ .const_val = Val.init(@as(i64, 2)) },
                    },
                },
                .false = &Ir{ .const_val = Val.init(@as(i64, 3)) },
            },
        },
        ir,
    );
}

// ========== Body/Begin Tests ==========

test "build begin with multiple expressions" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(begin 1 2 3)");
    const ir = try builder.build(expr);

    try testing.expectEqualDeep(
        Ir{
            .body = &.{
                Ir{ .const_val = Val.init(@as(i64, 1)) },
                Ir{ .const_val = Val.init(@as(i64, 2)) },
                Ir{ .const_val = Val.init(@as(i64, 3)) },
            },
        },
        ir,
    );
}

test "build begin with single expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(begin 42)");
    const ir = try builder.build(expr);

    try testing.expectEqualDeep(
        Ir{
            .body = &.{
                Ir{ .const_val = Val.init(42) },
            },
        },
        ir,
    );
}

test "build empty begin" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(begin)");
    const ir = try builder.build(expr);

    try testing.expectEqualDeep(
        Ir{
            .body = &.{},
        },
        ir,
    );
}

// ========== Define Tests ==========

test "build variable definition" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(define x 42)");
    const ir = try builder.build(expr);

    const expected_symbol = try vm.builder().internStatic(Symbol.init("x"));
    try testing.expectEqualDeep(
        Ir{
            .define = .{
                .symbol = expected_symbol,
                .expr = &Ir{ .const_val = Val.init(42) },
            },
        },
        ir,
    );
}

test "build function definition shorthand" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(define (square x) (* x x))");
    const ir = try builder.build(expr);

    const expected_symbol = try vm.builder().internStatic(Symbol.init("square"));
    const x_symbol = try vm.builder().internStatic(Symbol.init("x"));
    const mult_symbol = try vm.builder().internStatic(Symbol.init("*"));
    try testing.expectEqualDeep(
        Ir{
            .define = .{
                .symbol = expected_symbol,
                .expr = &Ir{
                    .lambda = .{
                        .name = expected_symbol,
                        .args = &.{x_symbol},
                        .body = &Ir{
                            .body = &.{
                                Ir{
                                    .proc_call = .{
                                        .proc = &Ir{ .get = mult_symbol },
                                        .args = &.{
                                            Ir{ .get = x_symbol },
                                            Ir{ .get = x_symbol },
                                        },
                                    },
                                },
                            },
                        },
                    },
                },
            },
        },
        ir,
    );
}

test "build function definition with no parameters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(define (get-answer) 42)");
    const ir = try builder.build(expr);

    const expected_symbol = try vm.builder().internStatic(Symbol.init("get-answer"));
    try testing.expectEqualDeep(
        Ir{
            .define = .{
                .symbol = expected_symbol,
                .expr = &Ir{
                    .lambda = .{
                        .name = expected_symbol,
                        .args = &.{},
                        .body = &Ir{
                            .body = &.{
                                Ir{ .const_val = Val.init(42) },
                            },
                        },
                    },
                },
            },
        },
        ir,
    );
}

test "build function definition with multiple parameters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(define (add a b c) (+ a b c))");
    const ir = try builder.build(expr);

    const add_symbol = try vm.builder().internStatic(Symbol.init("add"));
    const a_symbol = try vm.builder().internStatic(Symbol.init("a"));
    const b_symbol = try vm.builder().internStatic(Symbol.init("b"));
    const c_symbol = try vm.builder().internStatic(Symbol.init("c"));
    const plus_symbol = try vm.builder().internStatic(Symbol.init("+"));
    try testing.expectEqualDeep(
        Ir{
            .define = .{
                .symbol = add_symbol,
                .expr = &Ir{
                    .lambda = .{
                        .name = add_symbol,
                        .args = &.{ a_symbol, b_symbol, c_symbol },
                        .body = &Ir{
                            .body = &.{
                                Ir{
                                    .proc_call = .{
                                        .proc = &Ir{ .get = plus_symbol },
                                        .args = &.{
                                            Ir{ .get = a_symbol },
                                            Ir{ .get = b_symbol },
                                            Ir{ .get = c_symbol },
                                        },
                                    },
                                },
                            },
                        },
                    },
                },
            },
        },
        ir,
    );
}

// ========== Lambda Tests ==========

test "build lambda with parameters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(lambda (x y) (+ x y))");
    const ir = try builder.build(expr);

    const x_symbol = try vm.builder().internStatic(Symbol.init("x"));
    const y_symbol = try vm.builder().internStatic(Symbol.init("y"));
    const plus_symbol = try vm.builder().internStatic(Symbol.init("+"));
    try testing.expectEqualDeep(
        Ir{
            .lambda = .{
                .name = null,
                .args = &.{ x_symbol, y_symbol },
                .body = &Ir{
                    .body = &.{
                        Ir{
                            .proc_call = .{
                                .proc = &Ir{ .get = plus_symbol },
                                .args = &.{
                                    Ir{ .get = x_symbol },
                                    Ir{ .get = y_symbol },
                                },
                            },
                        },
                    },
                },
            },
        },
        ir,
    );
}

test "build lambda with no parameters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(lambda () 42)");
    const ir = try builder.build(expr);

    try testing.expectEqualDeep(
        Ir{
            .lambda = .{
                .name = null,
                .args = &.{},
                .body = &Ir{
                    .body = &.{
                        Ir{ .const_val = Val.init(42) },
                    },
                },
            },
        },
        ir,
    );
}

test "build nested lambdas" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(lambda (x) (lambda (y) (+ x y)))");
    const ir = try builder.build(expr);

    const x_symbol = try vm.builder().internStatic(Symbol.init("x"));
    const y_symbol = try vm.builder().internStatic(Symbol.init("y"));
    const plus_symbol = try vm.builder().internStatic(Symbol.init("+"));
    try testing.expectEqualDeep(
        Ir{
            .lambda = .{
                .name = null,
                .args = &.{x_symbol},
                .body = &Ir{
                    .body = &.{
                        Ir{
                            .lambda = .{
                                .name = null,
                                .args = &.{y_symbol},
                                .body = &Ir{
                                    .body = &.{
                                        Ir{
                                            .proc_call = .{
                                                .proc = &Ir{ .get = plus_symbol },
                                                .args = &.{
                                                    Ir{ .get = x_symbol },
                                                    Ir{ .get = y_symbol },
                                                },
                                            },
                                        },
                                    },
                                },
                            },
                        },
                    },
                },
            },
        },
        ir,
    );
}

// ========== Let Binding Tests ==========

test "build let with empty bindings" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(let () 42)");
    const ir = try builder.build(expr);

    try testing.expectEqualDeep(
        Ir{
            .let = .{
                .star = false,
                .bindings = &.{},
                .body = &Ir{
                    .body = &.{
                        Ir{ .const_val = Val.init(42) },
                    },
                },
            },
        },
        ir,
    );
}

test "build let with single binding" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(let ((x 42)) x)");
    const ir = try builder.build(expr);

    const expected_symbol = try vm.builder().internStatic(Symbol.init("x"));
    try testing.expectEqualDeep(
        Ir{
            .let = .{
                .star = false,
                .bindings = &.{
                    LetBinding{
                        .name = expected_symbol,
                        .expr = Ir{ .const_val = Val.init(42) },
                    },
                },
                .body = &Ir{
                    .body = &.{
                        Ir{ .get = expected_symbol },
                    },
                },
            },
        },
        ir,
    );
}

test "build nested let expressions" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(let ((x 1)) (let ((y 2)) (+ x y)))");
    const ir = try builder.build(expr);

    const x_symbol = try vm.builder().internStatic(Symbol.init("x"));
    const y_symbol = try vm.builder().internStatic(Symbol.init("y"));
    const plus_symbol = try vm.builder().internStatic(Symbol.init("+"));
    try testing.expectEqualDeep(
        Ir{
            .let = .{
                .star = false,
                .bindings = &.{
                    LetBinding{
                        .name = x_symbol,
                        .expr = Ir{ .const_val = Val.init(@as(i64, 1)) },
                    },
                },
                .body = &Ir{
                    .body = &.{
                        Ir{
                            .let = .{
                                .star = false,
                                .bindings = &.{
                                    LetBinding{
                                        .name = y_symbol,
                                        .expr = Ir{ .const_val = Val.init(@as(i64, 2)) },
                                    },
                                },
                                .body = &Ir{
                                    .body = &.{
                                        Ir{
                                            .proc_call = .{
                                                .proc = &Ir{ .get = plus_symbol },
                                                .args = &.{
                                                    Ir{ .get = x_symbol },
                                                    Ir{ .get = y_symbol },
                                                },
                                            },
                                        },
                                    },
                                },
                            },
                        },
                    },
                },
            },
        },
        ir,
    );
}

test "build let with complex expressions in bindings" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(let ((x (+ 1 2)) (y (* 3 4))) (+ x y))");
    const ir = try builder.build(expr);

    const x_symbol = try vm.builder().internStatic(Symbol.init("x"));
    const y_symbol = try vm.builder().internStatic(Symbol.init("y"));
    const plus_symbol = try vm.builder().internStatic(Symbol.init("+"));
    const mult_symbol = try vm.builder().internStatic(Symbol.init("*"));
    try testing.expectEqualDeep(
        Ir{
            .let = .{
                .star = false,
                .bindings = &.{
                    LetBinding{
                        .name = x_symbol,
                        .expr = Ir{
                            .proc_call = .{
                                .proc = &Ir{ .get = plus_symbol },
                                .args = &.{
                                    Ir{ .const_val = Val.init(@as(i64, 1)) },
                                    Ir{ .const_val = Val.init(@as(i64, 2)) },
                                },
                            },
                        },
                    },
                    LetBinding{
                        .name = y_symbol,
                        .expr = Ir{
                            .proc_call = .{
                                .proc = &Ir{ .get = mult_symbol },
                                .args = &.{
                                    Ir{ .const_val = Val.init(@as(i64, 3)) },
                                    Ir{ .const_val = Val.init(@as(i64, 4)) },
                                },
                            },
                        },
                    },
                },
                .body = &Ir{
                    .body = &.{
                        Ir{
                            .proc_call = .{
                                .proc = &Ir{ .get = plus_symbol },
                                .args = &.{
                                    Ir{ .get = x_symbol },
                                    Ir{ .get = y_symbol },
                                },
                            },
                        },
                    },
                },
            },
        },
        ir,
    );
}

// ========== Quote Tests ==========

test "build quoted literal" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(quote 42)");
    const ir = try builder.build(expr);

    try testing.expect(ir == .const_val);
    try testing.expectEqualDeep(Ir{ .const_val = Val.init(42) }, ir);
}

test "build quoted symbol" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(quote foo)");
    const ir = try builder.build(expr);

    try testing.expect(ir == .const_val);
}

test "build quoted list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(quote (1 2 3))");
    const ir = try builder.build(expr);

    try testing.expect(ir == .const_val);
}

// ========== Error Handling Tests ==========

test "build invalid if with missing test expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(if)");
    try testing.expectError(Error.InvalidExpression, builder.build(expr));
}

test "build invalid define with no arguments" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(define)");
    try testing.expectError(Error.InvalidExpression, builder.build(expr));
}

test "build invalid lambda with no parameters or body" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(lambda)");
    try testing.expectError(Error.InvalidExpression, builder.build(expr));
}

test "build invalid let with no bindings list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(let)");
    try testing.expectError(Error.InvalidExpression, builder.build(expr));
}

test "build invalid quote with no argument" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(quote)");
    try testing.expectError(Error.InvalidExpression, builder.build(expr));
}

test "build invalid quote with multiple arguments" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(quote a b)");
    try testing.expectError(Error.InvalidExpression, builder.build(expr));
}

// ========== Complex Expression Tests ==========

test "build complex nested expression" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(let ((f (lambda (x) (* x x)))) (if #t (f 5) 0))");
    const ir = try builder.build(expr);

    const f_symbol = try vm.builder().internStatic(Symbol.init("f"));
    const x_symbol = try vm.builder().internStatic(Symbol.init("x"));
    const mult_symbol = try vm.builder().internStatic(Symbol.init("*"));
    try testing.expectEqualDeep(
        Ir{
            .let = .{
                .star = false,
                .bindings = &.{
                    LetBinding{
                        .name = f_symbol,
                        .expr = Ir{
                            .lambda = .{
                                .name = null,
                                .args = &.{x_symbol},
                                .body = &Ir{
                                    .body = &.{
                                        Ir{
                                            .proc_call = .{
                                                .proc = &Ir{ .get = mult_symbol },
                                                .args = &.{
                                                    Ir{ .get = x_symbol },
                                                    Ir{ .get = x_symbol },
                                                },
                                            },
                                        },
                                    },
                                },
                            },
                        },
                    },
                },
                .body = &Ir{
                    .body = &.{
                        Ir{
                            .@"if" = .{
                                .@"test" = &Ir{ .const_val = Val.init(true) },
                                .true = &Ir{
                                    .proc_call = .{
                                        .proc = &Ir{ .get = f_symbol },
                                        .args = &.{
                                            Ir{ .const_val = Val.init(@as(i64, 5)) },
                                        },
                                    },
                                },
                                .false = &Ir{ .const_val = Val.init(@as(i64, 0)) },
                            },
                        },
                    },
                },
            },
        },
        ir,
    );
}

test "build realistic fibonacci function" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne("(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))");
    const ir = try builder.build(expr);

    const fib_symbol = try vm.builder().internStatic(Symbol.init("fib"));
    const n_symbol = try vm.builder().internStatic(Symbol.init("n"));
    const less_symbol = try vm.builder().internStatic(Symbol.init("<"));
    const plus_symbol = try vm.builder().internStatic(Symbol.init("+"));
    const minus_symbol = try vm.builder().internStatic(Symbol.init("-"));
    try testing.expectEqualDeep(
        Ir{
            .define = .{
                .symbol = fib_symbol,
                .expr = &Ir{
                    .lambda = .{
                        .name = fib_symbol,
                        .args = &.{n_symbol},
                        .body = &Ir{
                            .body = &.{
                                Ir{
                                    .@"if" = .{
                                        .@"test" = &Ir{
                                            .proc_call = .{
                                                .proc = &Ir{ .get = less_symbol },
                                                .args = &.{
                                                    Ir{ .get = n_symbol },
                                                    Ir{ .const_val = Val.init(@as(i64, 2)) },
                                                },
                                            },
                                        },
                                        .true = &Ir{ .get = n_symbol },
                                        .false = &Ir{
                                            .proc_call = .{
                                                .proc = &Ir{ .get = plus_symbol },
                                                .args = &.{
                                                    Ir{
                                                        .proc_call = .{
                                                            .proc = &Ir{ .get = fib_symbol },
                                                            .args = &.{
                                                                Ir{
                                                                    .proc_call = .{
                                                                        .proc = &Ir{ .get = minus_symbol },
                                                                        .args = &.{
                                                                            Ir{ .get = n_symbol },
                                                                            Ir{ .const_val = Val.init(@as(i64, 1)) },
                                                                        },
                                                                    },
                                                                },
                                                            },
                                                        },
                                                    },
                                                    Ir{
                                                        .proc_call = .{
                                                            .proc = &Ir{ .get = fib_symbol },
                                                            .args = &.{
                                                                Ir{
                                                                    .proc_call = .{
                                                                        .proc = &Ir{ .get = minus_symbol },
                                                                        .args = &.{
                                                                            Ir{ .get = n_symbol },
                                                                            Ir{ .const_val = Val.init(@as(i64, 2)) },
                                                                        },
                                                                    },
                                                                },
                                                            },
                                                        },
                                                    },
                                                },
                                            },
                                        },
                                    },
                                },
                            },
                        },
                    },
                },
            },
        },
        ir,
    );
}

test "build combination of all constructs" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(&vm, &arena);

    const expr = try vm.builder().readOne(
        \\(begin
        \\  (define (test-func x y)
        \\    (let* ((a (+ x 1)) (b (* y 2)))
        \\      (if (> a b) a b)))
        \\  (test-func 10 20))
    );
    const ir = try builder.build(expr);

    const test_func_symbol = try vm.builder().internStatic(Symbol.init("test-func"));
    const x_symbol = try vm.builder().internStatic(Symbol.init("x"));
    const y_symbol = try vm.builder().internStatic(Symbol.init("y"));
    const a_symbol = try vm.builder().internStatic(Symbol.init("a"));
    const b_symbol = try vm.builder().internStatic(Symbol.init("b"));
    const plus_symbol = try vm.builder().internStatic(Symbol.init("+"));
    const mult_symbol = try vm.builder().internStatic(Symbol.init("*"));
    const greater_symbol = try vm.builder().internStatic(Symbol.init(">"));
    try testing.expectEqualDeep(
        Ir{
            .body = &.{
                Ir{
                    .define = .{
                        .symbol = test_func_symbol,
                        .expr = &Ir{
                            .lambda = .{
                                .name = test_func_symbol,
                                .args = &.{ x_symbol, y_symbol },
                                .body = &Ir{
                                    .body = &.{
                                        Ir{
                                            .let = .{
                                                .star = true,
                                                .bindings = &.{
                                                    LetBinding{
                                                        .name = a_symbol,
                                                        .expr = Ir{
                                                            .proc_call = .{
                                                                .proc = &Ir{ .get = plus_symbol },
                                                                .args = &.{
                                                                    Ir{ .get = x_symbol },
                                                                    Ir{ .const_val = Val.init(@as(i64, 1)) },
                                                                },
                                                            },
                                                        },
                                                    },
                                                    LetBinding{
                                                        .name = b_symbol,
                                                        .expr = Ir{
                                                            .proc_call = .{
                                                                .proc = &Ir{ .get = mult_symbol },
                                                                .args = &.{
                                                                    Ir{ .get = y_symbol },
                                                                    Ir{ .const_val = Val.init(@as(i64, 2)) },
                                                                },
                                                            },
                                                        },
                                                    },
                                                },
                                                .body = &Ir{
                                                    .body = &.{
                                                        Ir{
                                                            .@"if" = .{
                                                                .@"test" = &Ir{
                                                                    .proc_call = .{
                                                                        .proc = &Ir{ .get = greater_symbol },
                                                                        .args = &.{
                                                                            Ir{ .get = a_symbol },
                                                                            Ir{ .get = b_symbol },
                                                                        },
                                                                    },
                                                                },
                                                                .true = &Ir{ .get = a_symbol },
                                                                .false = &Ir{ .get = b_symbol },
                                                            },
                                                        },
                                                    },
                                                },
                                            },
                                        },
                                    },
                                },
                            },
                        },
                    },
                },
                Ir{
                    .proc_call = .{
                        .proc = &Ir{ .get = test_func_symbol },
                        .args = &.{
                            Ir{ .const_val = Val.init(@as(i64, 10)) },
                            Ir{ .const_val = Val.init(@as(i64, 20)) },
                        },
                    },
                },
            },
        },
        ir,
    );
}
