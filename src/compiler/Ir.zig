const std = @import("std");

const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

const Ir = @This();

kind: IrKind,

pub const Error = error{
    InvalidExpression,
    OutOfMemory,
    NotImplemented,
    UndefinedBehavior,
};

pub const IrKind = union(enum) {
    push_const: Val,
    get: Symbol.Interned,
    get_arg: u32,
    if_expr: IfExpr,
    eval: Eval,
    lambda: Lambda,
    ret,
};

pub const IfExpr = struct {
    test_expr: *Ir,
    true_expr: *Ir,
    false_expr: *Ir,
};

pub const Eval = struct {
    proc: *Ir,
    args: []Ir,
};

pub const Lambda = struct {
    arg_count: u32,
    body: []Ir,
};

pub const Variable = struct {
    name: ?Symbol.Interned,
    location: Location,

    pub const Location = union(enum) {
        arg: u32,
    };
};

pub const Builder = struct {
    arena: *std.heap.ArenaAllocator,
    vm: *Vm,
    variables: std.ArrayList(Variable) = .{},

    pub fn init(arena: *std.heap.ArenaAllocator, vm: *Vm) Builder {
        return .{ .arena = arena, .vm = vm };
    }

    fn resolveVariable(self: Builder, name: Symbol.Interned) ?Variable.Location {
        var idx = self.variables.items.len;
        while (idx > 0) {
            idx -= 1;
            const variable = self.variables.items[idx];
            if (variable.name) |var_name| {
                if (var_name.eq(name)) return variable.location;
            }
        }
        return null;
    }

    pub fn build(self: *Builder, expr: Val) Error!Ir {
        switch (expr.data) {
            .empty_list => return Error.InvalidExpression,
            .boolean,
            .int,
            .module,
            .proc,
            .proc_builtin,
            => return Ir{ .kind = IrKind{ .push_const = expr } },
            .pair => {
                const list = try self.valToSlice(expr);
                return self.buildList(list);
            },
            .symbol => |sym| return self.buildSymbol(sym),
        }
    }

    fn buildList(self: *Builder, list: []const Val) Error!Ir {
        if (list.len == 0) return Error.InvalidExpression;
        const lambda = try self.vm.builder().makeSymbolInterned(Symbol.init("lambda"));
        const if_sym = try self.vm.builder().makeSymbolInterned(Symbol.init("if"));
        if (list[0].asSymbol()) |sym| {
            if (sym.eq(lambda)) {
                if (list.len < 2) return Error.InvalidExpression;
                return self.buildLambda(list[1], list[2..]);
            }
            if (sym.eq(if_sym)) {
                if (list.len != 4) return Error.InvalidExpression;
                const exprs = try self.arena.allocator().alloc(Ir, list.len - 1);
                exprs[0] = try self.build(list[1]); // test
                exprs[1] = try self.build(list[2]); // true
                exprs[2] = try self.build(list[3]); // false
                return Ir{
                    .kind = .{
                        .if_expr = .{
                            .test_expr = &exprs[0],
                            .true_expr = &exprs[1],
                            .false_expr = &exprs[2],
                        },
                    },
                };
            }
        }
        const irs = try self.arena.allocator().alloc(Ir, list.len);
        for (list, irs) |expr, *ir| {
            ir.* = try self.build(expr);
        }
        return Ir{
            .kind = .{
                .eval = .{
                    .proc = &irs[0],
                    .args = irs[1..],
                },
            },
        };
    }

    fn buildLambda(self: *Builder, parameters: Val, body: []const Val) Error!Ir {
        // 1. Populate bound variables.
        const start_idx = self.variables.items.len;
        const parameters_list = try self.valToSlice(parameters);
        try self.variables.ensureUnusedCapacity(self.arena.allocator(), parameters_list.len);
        for (parameters_list, 0..parameters_list.len) |param_val, idx| {
            const param = param_val.asSymbol() orelse return Error.InvalidExpression;
            try self.variables.appendBounded(Variable{
                .name = param,
                .location = .{ .arg = @intCast(idx) },
            });
        }
        const end_idx = self.variables.items.len;

        // 2. Compile body.
        const body_irs = try self.arena.allocator().alloc(Ir, body.len);
        for (body_irs, body) |*ir, expr| {
            ir.* = try self.build(expr);
        }

        // 3. Remove parameters from scope.
        for (self.variables.items[start_idx..end_idx]) |*variable| {
            variable.name = null;
        }

        // 4. Build IR
        return Ir{
            .kind = .{
                .lambda = .{
                    .arg_count = @intCast(parameters_list.len),
                    .body = body_irs,
                },
            },
        };
    }

    fn buildSymbol(self: Builder, symbol: Symbol.Interned) Error!Ir {
        if (self.resolveVariable(symbol)) |variable| {
            return Ir{ .kind = .{ .get_arg = variable.arg } };
        }
        return Ir{ .kind = .{ .get = symbol } };
    }

    fn valToSlice(self: Builder, val: Val) Error![]const Val {
        const inspector = self.vm.inspector();
        const list = inspector.asList(self.arena.allocator(), val) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.WrongType, error.UndefinedBehavior => return Error.UndefinedBehavior,
        };
        return list;
    }
};
