const std = @import("std");

const Symbol = @import("../types/Symbol.zig");
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

    pub fn init(arena: *std.heap.ArenaAllocator, vm: *Vm, expr: Val) Error!Ir {
        var builder = Builder{
            .arena = arena,
            .vm = vm,
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

    pub fn build(self: *Builder, expr: Val) Error!Ir {
        switch (expr.data) {
            .empty_list => return Error.InvalidExpression,
            .boolean,
            .int,
            .module,
            .proc,
            .closure,
            .native_proc,
            .continuation,
            => return Ir{ .push_const = expr },
            .pair => {
                const list = try self.valToSlice(expr);
                return self.buildList(list);
            },
            .symbol => |sym| return Ir{ .get = sym },
            .vector => return Error.NotImplemented,
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
        const if_sym = try self.vm.builder().makeStaticSymbolHandle("if");
        const lambda = try self.vm.builder().makeStaticSymbolHandle("lambda");
        const let = try self.vm.builder().makeStaticSymbolHandle("let");
        const quote = try self.vm.builder().makeStaticSymbolHandle("quote");
        if (list[0].asSymbol()) |sym| {
            if (sym.eq(define)) {
                switch (list.len) {
                    0, 1, 2 => return Error.InvalidExpression,
                    else => return self.buildDefine(list[1], list[2..]),
                }
            }
            if (sym.eq(lambda)) {
                if (list.len < 2) return Error.InvalidExpression;
                return self.buildLambda(null, list[1], list[2..]);
            }
            if (sym.eq(if_sym)) {
                switch (list.len) {
                    4 => return self.buildIf(list[1], list[2], list[3]),
                    else => return Error.InvalidExpression,
                }
            }
            if (sym.eq(let)) {
                switch (list.len) {
                    0, 1 => return Error.InvalidExpression,
                    else => return self.buildLet(list[1], list[2..]),
                }
            }
            if (sym.eq(quote)) {
                switch (list.len) {
                    2 => return Ir{ .push_const = list[1] },
                    else => return Error.InvalidExpression,
                }
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

    fn buildIf(self: *Builder, test_expr: Val, true_expr: Val, false_expr: Val) Error!Ir {
        const irs = try self.arena.allocator().alloc(Ir, 3);
        irs[0] = try self.build(test_expr);
        irs[1] = try self.build(true_expr);
        irs[2] = try self.build(false_expr);
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
};
