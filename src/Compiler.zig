const std = @import("std");
const testing = std.testing;

const Builder = @import("Builder.zig");
const Handle = @import("object_pool.zig").Handle;
const Inspector = @import("Inspector.zig");
const Instruction = @import("instruction.zig").Instruction;
const Ir = @import("Ir.zig");
const Module = @import("Module.zig");
const ObjectPool = @import("object_pool.zig").ObjectPool;
const Pair = @import("Pair.zig");
const PrettyPrinter = @import("PrettyPrinter.zig");
const Proc = @import("Proc.zig");
const Reader = @import("Reader.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const Compiler = @This();

vm: *Vm,
arena: *std.heap.ArenaAllocator,
module: Handle(Module),
instructions: std.ArrayList(Instruction) = .{},

pub const Error = error{
    InvalidExpression,
    OutOfMemory,
    NotImplemented,
    UndefinedBehavior,
};

pub fn init(arena: *std.heap.ArenaAllocator, vm: *Vm, module: Handle(Module)) Compiler {
    return Compiler{
        .vm = vm,
        .arena = arena,
        .module = module,
    };
}

pub fn compile(self: *Compiler, expr: Val) Error!Val {
    // Build IR
    const ir = try Ir.Builder.init(self.arena, self.vm).build(expr);
    try self.addIr(ir);
    // Build procedure
    const proc = try self.makeProc();
    return proc.val;
}

fn addIr(self: *Compiler, ir: Ir) Error!void {
    switch (ir.kind) {
        .push_const => |v| try self.addInstruction(.{ .push_const = v }),
        .get => |sym| try self.addInstruction(.{
            .module_get = .{ .module = self.module, .symbol = sym },
        }),
        .get_local => |idx| try self.addInstruction(.{ .get_local = idx }),
        .eval => |e| {
            try self.addIr(e.proc.*);
            for (e.args) |arg| try self.addIr(arg);
            try self.addInstruction(.{ .eval = @intCast(e.args.len) });
        },
        .lambda => |l| try self.addInstruction(
            Instruction{ .push_const = try self.buildLambda(l) },
        ),
        .ret => try self.addInstruction(.{ .ret = {} }),
    }
}

fn addInstruction(self: *Compiler, instruction: Instruction) !void {
    try self.instructions.append(self.arena.allocator(), instruction);
}

fn buildLambda(self: *Compiler, lambda: Ir.Lambda) Error!Val {
    var sub_compiler = Compiler{
        .vm = self.vm,
        .arena = self.arena,
        .module = self.module,
    };
    if (lambda.body.len == 0)
        try sub_compiler.addIr(Ir{ .kind = .{ .push_const = Val.initEmptyList() } });
    for (lambda.body) |ir|
        try sub_compiler.addIr(ir);
    const proc = try sub_compiler.makeProc();
    return proc.val;
}

pub fn makeProc(self: Compiler) Error!struct { val: Val, proc: Proc } {
    const builder = self.vm.builder();
    const name = try builder.makeSymbolInterned(Symbol.init("_"));
    var proc = Proc{
        .name = name,
        .instructions = try self.vm.allocator().dupe(Instruction, self.instructions.items),
    };
    errdefer proc.deinit(self.vm.allocator());
    const handle = try self.vm.objects.procs.put(self.vm.allocator(), proc);
    const val = Val{ .data = .{ .proc = handle } };
    return .{ .val = val, .proc = proc };
}
