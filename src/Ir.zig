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
    eval: Eval,
    ret,
};

pub const Eval = struct {
    proc: *const Ir,
    args: []const Ir,
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
                const list = self.vm.inspector().asList(self.arena.allocator(), expr) catch |e| switch (e) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.WrongType, error.UndefinedBehavior => return Error.UndefinedBehavior,
                };
                return self.buildList(list);
            },
            .symbol => |sym| return Ir{ .kind = IrKind{ .get = sym } },
        }
    }

    pub fn buildList(self: Builder, list: []const Val) Error!Ir {
        if (list.len == 0) return Error.InvalidExpression;
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
};
