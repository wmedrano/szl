const std = @import("std");

const Module = @import("../types/Module.zig");
const Handle = @import("../types/object_pool.zig").Handle;
const Symbol = @import("../types/Symbol.zig");
const SyntaxRules = @import("../types/SyntaxRules.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

pub const Error = error{
    InvalidExpression,
    OutOfMemory,
    NotImplemented,
    UndefinedBehavior,
};

pub const Ir = union(enum) {
    push_const: Val,
    get: Symbol,
    define: struct {
        symbol: Symbol,
        expr: *Ir,
    },
    if_expr: IfExpr,
    let_expr: struct {
        bindings: []LetBinding,
        body: []Ir,
    },
    eval: Eval,
    lambda: Lambda,

    pub fn init(arena: *std.heap.ArenaAllocator, vm: *Vm, module: Handle(Module), expr: Val) Error!Ir {
        var builder = Builder{
            .arena = arena,
            .vm = vm,
            .module = module,
        };
        return builder.build(expr);
    }

    pub const IfExpr = struct {
        test_expr: *Ir,
        true_expr: *Ir,
        false_expr: *Ir,
    };

    pub const LetBinding = struct {
        name: Symbol,
        expr: Ir,
    };

    pub const Eval = struct {
        proc: *Ir,
        args: []Ir,
    };

    pub const Lambda = struct {
        name: ?Symbol,
        args: []Symbol,
        body: []Ir,
    };

    pub const Variable = struct {
        name: ?Symbol,
        location: Location,

        pub const Location = union(enum) {
            arg: u32,
            capture: u32,
        };
    };
};

const Builder = struct {
    arena: *std.heap.ArenaAllocator,
    vm: *Vm,
    module: Handle(Module),

    pub fn build(self: *Builder, expr: Val) Error!Ir {
        // Try to expand macros first
        const expanded = (try self.expandMacros(expr)) orelse expr;

        switch (expanded.data) {
            .empty_list => return Error.InvalidExpression,
            .boolean,
            .int,
            .float,
            .char,
            .string,
            .vector,
            .bytevector,
            .module,
            .proc,
            .closure,
            .native_proc,
            .continuation,
            .record,
            .record_descriptor,
            => return Ir{ .push_const = expanded },
            .pair => {
                const list = try self.valToSlice(expanded);
                return self.buildList(list);
            },
            .symbol => |sym| return Ir{ .get = sym },
            .syntax_rules => return Error.NotImplemented,
        }
    }

    pub fn buildMany(self: *Builder, exprs: []const Val) Error![]Ir {
        const irs = try self.arena.allocator().alloc(Ir, exprs.len);
        for (exprs, irs) |expr, *ir| {
            ir.* = try self.build(expr);
        }
        return irs;
    }

    fn buildList(self: *Builder, list: []const Val) Error!Ir {
        if (list.len == 0) return Error.InvalidExpression;
        const define = try self.vm.builder().makeStaticSymbolHandle("define");
        const define_syntax = try self.vm.builder().makeStaticSymbolHandle("define-syntax");
        const if_sym = try self.vm.builder().makeStaticSymbolHandle("if");
        const when = try self.vm.builder().makeStaticSymbolHandle("when");
        const lambda = try self.vm.builder().makeStaticSymbolHandle("lambda");
        const let = try self.vm.builder().makeStaticSymbolHandle("let");
        const begin = try self.vm.builder().makeStaticSymbolHandle("begin");
        const quote = try self.vm.builder().makeStaticSymbolHandle("quote");
        const syntax_rules = try self.vm.builder().makeStaticSymbolHandle("syntax-rules");
        if (list[0].asSymbol()) |sym| {
            if (sym.eq(define)) {
                switch (list.len) {
                    0, 1, 2 => return Error.InvalidExpression,
                    else => return self.buildDefine(list[1], list[2..]),
                }
            }
            if (sym.eq(define_syntax)) {
                switch (list.len) {
                    3 => return self.buildDefineSyntax(list[1], list[2]),
                    else => return Error.InvalidExpression,
                }
            }
            if (sym.eq(lambda)) {
                if (list.len < 2) return Error.InvalidExpression;
                return self.buildLambda(null, list[1], list[2..]);
            }
            if (sym.eq(if_sym)) {
                switch (list.len) {
                    3 => return self.buildIf(list[1], list[2], null),
                    4 => return self.buildIf(list[1], list[2], list[3]),
                    else => return Error.InvalidExpression,
                }
            }
            if (sym.eq(when)) {
                switch (list.len) {
                    0, 1, 2 => return Error.InvalidExpression,
                    else => return self.buildWhen(list[1], list[2..]),
                }
            }
            if (sym.eq(let)) {
                switch (list.len) {
                    0, 1 => return Error.InvalidExpression,
                    else => return self.buildLet(list[1], list[2..]),
                }
            }
            if (sym.eq(begin)) return self.buildLet(Val.initEmptyList(), list[1..]);
            if (sym.eq(quote)) {
                switch (list.len) {
                    2 => return Ir{ .push_const = list[1] },
                    else => return Error.InvalidExpression,
                }
            }
            if (sym.eq(syntax_rules)) {
                if (list.len < 2) return Error.InvalidExpression;
                return self.buildSyntaxRules(list[1], list[2..]);
            }
        }
        const irs = try self.arena.allocator().alloc(Ir, list.len);
        for (list, irs) |expr, *ir| {
            ir.* = try self.build(expr);
        }
        return Ir{
            .eval = .{
                .proc = &irs[0],
                .args = irs[1..],
            },
        };
    }

    fn buildDefine(self: *Builder, symbol_val: Val, exprs: []const Val) Error!Ir {
        // (define name value)
        if (symbol_val.asSymbol()) |sym| {
            if (exprs.len != 1) return Error.InvalidExpression;
            return self.buildDefineVal(sym, exprs[0]);
        }
        // (define (name args...) body...)
        if (symbol_val.data == .pair) {
            return self.buildDefineProcedure(symbol_val, exprs);
        }
        return Error.InvalidExpression;
    }

    fn buildDefineVal(self: *Builder, symbol: Symbol, expr: Val) Error!Ir {
        const expr_ir = try self.arena.allocator().create(Ir);
        expr_ir.* = try self.build(expr);
        return Ir{
            .define = .{
                .symbol = symbol,
                .expr = expr_ir,
            },
        };
    }

    fn buildDefineSyntax(self: *Builder, symbol_val: Val, syntax_rules_expr: Val) Error!Ir {
        // (define-syntax name (syntax-rules ...))
        const symbol = symbol_val.asSymbol() orelse return Error.InvalidExpression;

        // Build the syntax-rules expression (which should create a SyntaxRules object)
        const expr_ir = try self.arena.allocator().create(Ir);
        expr_ir.* = try self.build(syntax_rules_expr);

        // Return a define IR with the syntax-rules value
        return Ir{
            .define = .{
                .symbol = symbol,
                .expr = expr_ir,
            },
        };
    }

    fn buildDefineProcedure(self: *Builder, name_and_args: Val, body: []const Val) Error!Ir {
        // name_and_args is (name arg1 arg2 ...)
        const inspector = self.vm.inspector();
        const pair = inspector.asPair(name_and_args) catch return Error.InvalidExpression;
        const name = pair.car.asSymbol() orelse return Error.InvalidExpression;
        const args = pair.cdr; // (arg1 arg2 ...)

        // Build the IR
        const expr = try self.arena.allocator().create(Ir);
        expr.* = try self.buildLambda(name, args, body);
        return Ir{
            .define = .{
                .symbol = name,
                .expr = expr,
            },
        };
    }

    fn buildIf(self: *Builder, test_expr: Val, true_expr: Val, false_expr: ?Val) Error!Ir {
        const irs = try self.arena.allocator().dupe(Ir, &.{
            try self.build(test_expr),
            try self.build(true_expr),
            if (false_expr) |e| try self.build(e) else Ir{ .push_const = Val.initEmptyList() },
        });
        return Ir{
            .if_expr = .{
                .test_expr = &irs[0],
                .true_expr = &irs[1],
                .false_expr = &irs[2],
            },
        };
    }

    fn buildWhen(self: *Builder, test_expr: Val, exprs: []const Val) Error!Ir {
        const irs = try self.arena.allocator().dupe(Ir, &.{
            try self.build(test_expr),
            try self.buildLet(Val.initEmptyList(), exprs),
            Ir{ .push_const = Val.initEmptyList() },
        });
        return Ir{
            .if_expr = .{
                .test_expr = &irs[0],
                .true_expr = &irs[1],
                .false_expr = &irs[2],
            },
        };
    }

    fn buildLet(self: *Builder, bindings_val: Val, body: []const Val) Error!Ir {
        const bindings_slice = try self.valToSlice(bindings_val);
        const bindings = try self.arena.allocator().alloc(Ir.LetBinding, bindings_slice.len);
        for (bindings_slice, bindings) |src, *dst| {
            const parts = self.vm.inspector().listToSliceExact(src, 2) catch |e| switch (e) {
                error.UndefinedBehavior => return error.UndefinedBehavior,
                error.WrongType, error.BadLength => return error.InvalidExpression,
            };
            dst.* = Ir.LetBinding{
                .name = parts[0].asSymbol() orelse return Error.InvalidExpression,
                .expr = try self.build(parts[1]),
            };
        }
        return Ir{
            .let_expr = .{
                .bindings = bindings,
                .body = try self.buildMany(body),
            },
        };
    }

    fn buildLambda(self: *Builder, name: ?Symbol, parameters: Val, body: []const Val) Error!Ir {
        var lambda_builder = Builder{
            .arena = self.arena,
            .vm = self.vm,
            .module = self.module,
        };
        return Ir{
            .lambda = .{
                .name = name,
                .args = try self.valToSymbolsSlice(parameters),
                .body = try lambda_builder.buildMany(body),
            },
        };
    }

    fn valToSlice(self: Builder, val: Val) Error![]const Val {
        const inspector = self.vm.inspector();
        const list = inspector.listToSliceAlloc(self.arena.allocator(), val) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.WrongType, error.UndefinedBehavior => return Error.UndefinedBehavior,
        };
        return list;
    }

    fn valToSymbolsSlice(self: Builder, val: Val) Error![]Symbol {
        const vals = try self.valToSlice(val);
        const syms = try self.arena.allocator().alloc(Symbol, vals.len);
        for (vals, syms) |v, *sym| {
            const s = v.asSymbol() orelse return Error.InvalidExpression;
            sym.* = s;
        }
        return syms;
    }

    fn buildSyntaxRules(self: *Builder, literals_val: Val, rule_vals: []const Val) Error!Ir {
        // Parse literals list
        const literals_slice = try self.valToSymbolsSlice(literals_val);

        // Parse rules
        var rules = std.ArrayList(SyntaxRules.Rule){};
        for (rule_vals) |rule_val| {
            const rule_parts = try self.valToSlice(rule_val);
            if (rule_parts.len != 2) return Error.InvalidExpression;

            const pattern = try self.buildPattern(rule_parts[0], literals_slice);
            const template = try self.buildTemplate(rule_parts[1]);

            try rules.append(self.arena.allocator(), .{ .pattern = pattern, .template = template });
        }

        // Create SyntaxRules object
        const duped_literals = try self.vm.allocator().dupe(Symbol, literals_slice);
        const duped_rules = try self.vm.allocator().dupe(SyntaxRules.Rule, rules.items);

        const syntax_rules = SyntaxRules{
            .literals = std.ArrayList(Symbol).fromOwnedSlice(duped_literals),
            .rules = std.ArrayList(SyntaxRules.Rule).fromOwnedSlice(duped_rules),
            .defining_env = self.module,
        };

        const handle = try self.vm.objects.syntax_rules.put(self.vm.allocator(), syntax_rules);
        return Ir{ .push_const = Val{ .data = .{ .syntax_rules = handle } } };
    }

    fn buildPattern(self: *Builder, val: Val, literals: []const Symbol) Error!SyntaxRules.Pattern {
        // Check if it's a symbol
        if (val.asSymbol()) |sym| {
            // Check if it's a literal
            for (literals) |lit| {
                if (lit.eq(sym)) {
                    return .{ .literal = sym };
                }
            }
            // Check for underscore wildcard
            const underscore = try self.vm.builder().makeStaticSymbolHandle("_");
            if (sym.eq(underscore)) {
                return .{ .any = {} };
            }
            // Otherwise it's a pattern variable
            return .{ .variable = sym };
        }

        // Check if it's a list
        if (val.data == .pair or val.data == .empty_list) {
            const list_vals = try self.valToSlice(val);
            var patterns = std.ArrayList(SyntaxRules.Pattern){};

            var i: usize = 0;
            while (i < list_vals.len) : (i += 1) {
                const item = list_vals[i];

                // Check if next item is ellipsis
                if (i + 1 < list_vals.len) {
                    if (list_vals[i + 1].asSymbol()) |next_sym| {
                        const ellipsis = try self.vm.builder().makeStaticSymbolHandle("...");
                        if (next_sym.eq(ellipsis)) {
                            // Create ellipsis pattern
                            const sub_pattern = try self.buildPattern(item, literals);
                            const pattern_ptr = try self.vm.allocator().create(SyntaxRules.Pattern);
                            pattern_ptr.* = sub_pattern;

                            try patterns.append(self.vm.allocator(), .{
                                .ellipsis = .{ .pattern = pattern_ptr },
                            });

                            i += 1; // Skip the ellipsis symbol
                            continue;
                        }
                    }
                }

                // Regular pattern
                try patterns.append(self.vm.allocator(), try self.buildPattern(item, literals));
            }

            return .{ .list = patterns };
        }

        // Anything else is treated as a literal constant
        return Error.InvalidExpression;
    }

    fn buildTemplate(self: *Builder, val: Val) Error!SyntaxRules.Template {
        // Check if it's a symbol (template variable)
        if (val.asSymbol()) |sym| {
            return .{ .variable = sym };
        }

        // Check if it's a list
        if (val.data == .pair or val.data == .empty_list) {
            const list_vals = try self.valToSlice(val);
            var templates = std.ArrayList(SyntaxRules.Template){};

            var i: usize = 0;
            while (i < list_vals.len) : (i += 1) {
                const item = list_vals[i];

                // Check if next item is ellipsis
                if (i + 1 < list_vals.len) {
                    if (list_vals[i + 1].asSymbol()) |next_sym| {
                        const ellipsis = try self.vm.builder().makeStaticSymbolHandle("...");
                        if (next_sym.eq(ellipsis)) {
                            // Create ellipsis template
                            const sub_template = try self.buildTemplate(item);
                            const template_ptr = try self.vm.allocator().create(SyntaxRules.Template);
                            template_ptr.* = sub_template;

                            try templates.append(self.vm.allocator(), .{
                                .ellipsis = .{ .template = template_ptr },
                            });

                            i += 1; // Skip the ellipsis symbol
                            continue;
                        }
                    }
                }

                // Regular template
                try templates.append(self.vm.allocator(), try self.buildTemplate(item));
            }

            return .{ .list = templates };
        }

        // Everything else is a literal value
        return .{ .literal = val };
    }

    /// Expand macros in an expression recursively
    fn expandMacros(self: *Builder, expr: Val) Error!?Val {
        // Base cases: atoms don't need expansion
        switch (expr.data) {
            .boolean, .int, .float, .char, .string, .symbol, .empty_list, .module, .proc, .closure, .native_proc, .continuation, .vector, .bytevector, .record, .record_descriptor => {
                return expr;
            },
            .syntax_rules => return expr,
            .pair => {},
        }

        // For lists, check if the head is a macro
        const list = try self.valToSlice(expr);
        if (list.len == 0) return expr;

        // Check if the first element is a symbol bound to a macro
        if (list[0].asSymbol()) |sym| {
            // Look up the symbol in the module
            const module_ptr = self.vm.objects.modules.get(self.module) orelse return Error.UndefinedBehavior;
            if (module_ptr.getBySymbol(sym)) |val| {
                // If it's a syntax-rules macro, expand it
                if (val.data == .syntax_rules) {
                    const macro_handle = val.data.syntax_rules;
                    const macro_ptr = self.vm.objects.syntax_rules.get(macro_handle) orelse return Error.UndefinedBehavior;

                    // Try to expand the macro
                    const maybe_expanded = macro_ptr.expand(self.vm, expr) catch |err| switch (err) {
                        error.InvalidExpression => return Error.InvalidExpression,
                        error.NotImplemented => return Error.NotImplemented,
                        error.OutOfMemory => return Error.OutOfMemory,
                        error.UndefinedBehavior => return Error.UndefinedBehavior,
                        error.ReadError, error.Unreachable, error.WrongType, error.UncaughtException => return Error.UndefinedBehavior,
                    };
                    if (maybe_expanded) |expanded| {
                        // Recursively expand the result
                        return self.expandMacros(expanded);
                    }
                }
            }
        }

        // Not a macro call - recursively expand sub-expressions
        // But preserve special forms (quote, define, define-syntax, lambda, etc.) which should not expand their contents
        if (list[0].asSymbol()) |sym| {
            const quote = try self.vm.builder().makeStaticSymbolHandle("quote");
            if (sym.eq(quote)) {
                // Don't expand quoted expressions
                return expr;
            }

            const define_syntax = try self.vm.builder().makeStaticSymbolHandle("define-syntax");
            if (sym.eq(define_syntax)) {
                // Don't expand define-syntax forms
                return expr;
            }

            const syntax_rules_sym = try self.vm.builder().makeStaticSymbolHandle("syntax-rules");
            if (sym.eq(syntax_rules_sym)) {
                // Don't expand syntax-rules definitions
                return expr;
            }
        }

        // Recursively expand all elements of the list
        const expanded_items = try self.arena.allocator().alloc(Val, list.len);
        var has_expanded = false;
        for (list, 0..) |item, i| {
            if (try self.expandMacros(item)) |exp| {
                has_expanded = true;
                expanded_items[i] = exp;
            } else {
                expanded_items[i] = item;
            }
        }
        if (!has_expanded) return null;

        // Reconstruct the list
        const ret = try self.vm.builder().makeList(expanded_items);
        return ret;
    }
};
