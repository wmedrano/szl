const std = @import("std");
const testing = std.testing;

const Builder = @import("utils/Builder.zig");
const Compiler = @import("compiler/Compiler.zig");
const Context = @import("Context.zig");
const Handle = @import("types/object_pool.zig").Handle;
const Inspector = @import("utils/Inspector.zig");
const Instruction = @import("instruction.zig").Instruction;
const Module = @import("types/Module.zig");
const ObjectPool = @import("types/object_pool.zig").ObjectPool;
const Pair = @import("types/Pair.zig");
const PrettyPrinter = @import("utils/PrettyPrinter.zig");
const Proc = @import("types/Proc.zig");
const Reader = @import("compiler/Reader.zig");
const Symbol = @import("types/Symbol.zig");
const Val = @import("types/Val.zig");

const Vm = @This();

pub const Objects = struct {
    symbols: Symbol.Interner,
    pairs: ObjectPool(Pair) = .{},
    modules: ObjectPool(Module) = .{},
    procs: ObjectPool(Proc) = .{},

    pub fn init(alloc: std.mem.Allocator) Objects {
        return Objects{
            .symbols = Symbol.Interner.init(alloc),
        };
    }
};

options: Options,
objects: Objects,
context: Context,

pub const Options = struct {
    allocator: std.mem.Allocator,
};

pub const Error = error{
    InvalidExpression,
    NotImplemented,
    OutOfMemory,
    ReadError,
    UndefinedBehavior,
    Unreachable,
    WrongType,
};

pub fn init(options: Options) Error!Vm {
    var vm = Vm{
        .options = options,
        .objects = Objects.init(options.allocator),
        .context = try Context.init(options.allocator),
    };
    errdefer vm.deinit();
    try vm.initLibraries();
    return vm;
}

fn initLibraries(vm: *Vm) Error!void {
    const b = vm.builder();

    const base_definitions = [_]Builder.Definition{
        .{
            .symbol = (try b.makeSymbol(Symbol.init("+"))).data.symbol,
            .value = Val.initBuiltinProc(Proc.Builtin.add),
        },
    };
    _ = try b.makeEnvironment(&.{
        (try b.makeSymbol(Symbol.init("scheme"))).data.symbol,
        (try b.makeSymbol(Symbol.init("base"))).data.symbol,
    }, &base_definitions);
    _ = try b.makeEnvironment(&.{
        (try b.makeSymbol(Symbol.init("user"))).data.symbol,
        (try b.makeSymbol(Symbol.init("repl"))).data.symbol,
    }, &base_definitions);
}

pub fn deinit(self: *Vm) void {
    self.context.deinit(self.allocator());
    self.objects.pairs.deinit(self.allocator());

    var modules_iter = self.objects.modules.iterator();
    while (modules_iter.next()) |module|
        module.value.deinit(self.allocator());
    self.objects.modules.deinit(self.allocator());

    var procs_iter = self.objects.procs.iterator();
    while (procs_iter.next()) |proc|
        proc.value.deinit(self.allocator());
    self.objects.procs.deinit(self.allocator());

    self.objects.symbols.deinit(self.allocator());
}

pub fn allocator(self: Vm) std.mem.Allocator {
    return self.options.allocator;
}

pub fn builder(self: *Vm) Builder {
    return Builder.init(self);
}

pub fn inspector(self: *Vm) Inspector {
    return Inspector.init(self);
}

pub fn pretty(self: *const Vm, val: Val) PrettyPrinter {
    return val.pretty(self);
}

test builder {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const items = [_]Val{ Val.initInt(1), Val.initInt(2), Val.initInt(3) };
    try testing.expectFmt(
        "(1 2 3)",
        "{f}",
        .{vm.pretty(try b.makeList(&items))},
    );
}

pub fn read(self: *Vm, source: []const u8) Reader {
    return Reader.init(self, source);
}

test read {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var reader = vm.read("(1 2 3)");
    try testing.expectFmt(
        "(1 2 3)",
        "{f}",
        .{vm.pretty((try (reader.readNext())).?)},
    );
}

pub fn evalStr(self: *Vm, source: []const u8) Error!Val {
    var reader = self.read(source);
    var return_val = Val.initEmptyList();
    const b = self.builder();
    const env = self.inspector().findModule(&.{
        try b.makeSymbolInterned(Symbol.init("user")),
        try b.makeSymbolInterned(Symbol.init("repl")),
    }) orelse return Error.Unreachable;
    while (try reader.readNext()) |raw_expr| {
        const proc = try self.compile(raw_expr, env);
        self.context.reset();
        try self.context.push(self.allocator(), proc);
        try (Instruction{ .eval = 0 }).execute(self);
        while (self.context.nextInstruction()) |instruction| {
            try instruction.execute(self);
        }
        return_val = self.context.pop() orelse return Error.UndefinedBehavior;
    }
    return return_val;
}

fn compile(self: *Vm, expr: Val, env: Handle(Module)) !Val {
    var arena = std.heap.ArenaAllocator.init(self.allocator());
    defer arena.deinit();
    var compiler = Compiler.init(&arena, self, env);
    const proc = try compiler.compile(expr);
    return proc;
}

test evalStr {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectEqual(
        Val.initInt(42),
        try vm.evalStr("((lambda (x) (+ x 30 2)) 10)"),
    );
}
