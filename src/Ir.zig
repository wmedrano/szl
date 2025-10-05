const std = @import("std");

const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

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
    get_local: u32,
    eval: Eval,
    lambda: Lambda,
    ret,
};

pub const Eval = struct {
    proc: *Ir,
    args: []Ir,
};

pub const Lambda = struct {
    body: []Ir,
};

pub const Builder = struct {
    arena: *std.heap.ArenaAllocator,
    vm: *Vm,

    pub fn init(arena: *std.heap.ArenaAllocator, vm: *Vm) Builder {
        return .{ .arena = arena, .vm = vm };
    }

    pub fn build(self: Builder, expr: Val) Error!Ir {
        switch (expr.data) {
            .empty_list => return Error.InvalidExpression,
            .int,
            .module,
            .proc,
            .proc_builtin,
            => return Ir{ .kind = IrKind{ .push_const = expr } },
            .pair => {
                const list = try self.valToSlice(expr);
                return self.buildList(list);
            },
            .symbol => |sym| return Ir{ .kind = IrKind{ .get = sym } },
        }
    }

    pub fn buildList(self: Builder, list: []const Val) Error!Ir {
        if (list.len == 0) return Error.InvalidExpression;
        const lambda = try self.vm.builder().makeSymbolInterned(Symbol.init("lambda"));
        if (list[0].asSymbol()) |sym| {
            if (sym.eq(lambda)) {
                if (list.len < 2) return Error.InvalidExpression;
                return self.buildLambda(list[1], list[2..]);
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

    pub fn buildLambda(self: Builder, parameters: Val, body: []const Val) Error!Ir {
        // 1. Build body
        const body_irs = try self.arena.allocator().alloc(Ir, body.len);
        for (body_irs, body) |*ir, expr| {
            ir.* = try self.build(expr);
        }

        // 2. Resolve parameters
        const parameters_list = try self.valToSlice(parameters);
        for (parameters_list, 0..parameters_list.len) |param_val, idx| {
            const param = param_val.asSymbol() orelse return Error.InvalidExpression;
            for (body_irs) |*ir| {
                ir.resolveVariable(param, @intCast(idx));
            }
        }

        // 3. Build IR
        return Ir{
            .kind = .{
                .lambda = .{ .body = body_irs },
            },
        };
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

// TODO: Populate the variable instead of recursing down for better performance.
fn resolveVariable(self: *Ir, name: Symbol.Interned, local_idx: u32) void {
    switch (self.kind) {
        .push_const => {},
        .get => |sym| if (sym.eq(name)) {
            self.kind = .{ .get_local = local_idx };
        },
        .get_local => {},
        .eval => |e| {
            e.proc.resolveVariable(name, local_idx);
            for (e.args) |*arg| arg.resolveVariable(name, local_idx);
        },
        .lambda => @panic("todo"),
        .ret => {},
    }
}
