const std = @import("std");
const testing = std.testing;

const Compiler = @import("compiler/Compiler.zig");
const Reader = @import("compiler/Reader.zig");
const Context = @import("Context.zig");
const instruction = @import("instruction.zig");
const Instruction = @import("instruction.zig").Instruction;
const Continuation = @import("types/Continuation.zig");
const Module = @import("types/Module.zig");
const NativeProc = @import("types/NativeProc.zig");
const Handle = @import("types/object_pool.zig").Handle;
const ObjectPool = @import("types/object_pool.zig").ObjectPool;
const Pair = @import("types/Pair.zig");
const Proc = @import("types/Proc.zig");
const Symbol = @import("types/Symbol.zig");
const Val = @import("types/Val.zig");
const Vector = @import("types/Vector.zig");
const Builder = @import("utils/Builder.zig");
const Inspector = @import("utils/Inspector.zig");
const PrettyPrinter = @import("utils/PrettyPrinter.zig");

const Vm = @This();

pub const Objects = struct {
    symbols: Symbol.Interner,
    pairs: ObjectPool(Pair) = .{},
    modules: ObjectPool(Module) = .{},
    procs: ObjectPool(Proc) = .{},
    vectors: ObjectPool(Vector) = .{},
    continuations: ObjectPool(Continuation) = .{},

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
    UncaughtException,
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

    // (scheme base)
    const global_handle = try b.makeEnvironment(&.{
        (try b.makeSymbol(Symbol.init("scheme"))).data.symbol,
        (try b.makeSymbol(Symbol.init("base"))).data.symbol,
    }, &[_]Builder.Definition{
        .{
            .symbol = (try b.makeSymbol(Symbol.init("+"))).data.symbol,
            .value = Val.initNativeProc(&NativeProc.add),
        },
        .{
            .symbol = (try b.makeSymbol(Symbol.init("<="))).data.symbol,
            .value = Val.initNativeProc(&NativeProc.lte),
        },
        .{
            .symbol = (try b.makeSymbol(Symbol.init("call/cc"))).data.symbol,
            .value = Val.initNativeProc(&NativeProc.call_cc),
        },
        .{
            .symbol = (try b.makeSymbol(Symbol.init("call-with-current-continuation"))).data.symbol,
            .value = Val.initNativeProc(&NativeProc.call_cc),
        },
        .{
            .symbol = (try b.makeSymbol(Symbol.init("with-exception-handler"))).data.symbol,
            .value = Val.initNativeProc(&NativeProc.with_exception_handler),
        },
        .{
            .symbol = (try b.makeSymbol(Symbol.init("raise-continuable"))).data.symbol,
            .value = Val.initNativeProc(&NativeProc.raise_continuable),
        },
        .{
            .symbol = (try b.makeSymbol(Symbol.init("%szl-raise-next"))).data.symbol,
            .value = Val.initNativeProc(&NativeProc.szl_raise_next),
        },
        .{
            .symbol = (try b.makeSymbol(Symbol.init("import"))).data.symbol,
            .value = Val.initNativeProc(&NativeProc.import),
        },
    });
    _ = try vm.evalStr(
        \\ (define (raise err)
        \\   (raise-continuable err)
        \\   (%szl-raise-next err))
    , global_handle);

    // (sizzle unstable compiler)
    const sizzle_unstable_compiler_handle = try b.makeEnvironment(&.{
        (try b.makeSymbol(Symbol.init("sizzle"))).data.symbol,
        (try b.makeSymbol(Symbol.init("unstable"))).data.symbol,
        (try b.makeSymbol(Symbol.init("compiler"))).data.symbol,
    }, &[_]Builder.Definition{
        .{
            .symbol = (try b.makeSymbol(Symbol.init("proc-instructions"))).data.symbol,
            .value = Val.initNativeProc(&NativeProc.proc_instructions),
        },
    });
    _ = try vm.evalStr("", sizzle_unstable_compiler_handle);

    // (user repl)
    const repl_handle = try b.makeEnvironment(&.{
        (try b.makeSymbol(Symbol.init("user"))).data.symbol,
        (try b.makeSymbol(Symbol.init("repl"))).data.symbol,
    }, &.{});
    const repl_mod = try vm.inspector().handleToModule(repl_handle);
    const global_mod = try vm.inspector().handleToModule(global_handle);
    try repl_mod.import(vm.allocator(), global_mod.*);
}

pub fn deinit(self: *Vm) void {
    self.context.deinit(self.allocator());
    self.objects.pairs.deinit(self.allocator());

    const standard_deinit = struct {
        allocator: std.mem.Allocator,
        pub fn apply(this: @This(), obj: anytype) void {
            obj.deinit(this.allocator);
        }
    }{ .allocator = self.allocator() };

    self.objects.modules.applyAll(standard_deinit);
    self.objects.modules.deinit(self.allocator());

    self.objects.procs.applyAll(standard_deinit);
    self.objects.procs.deinit(self.allocator());

    self.objects.vectors.applyAll(standard_deinit);
    self.objects.vectors.deinit(self.allocator());

    self.objects.continuations.applyAll(standard_deinit);
    self.objects.continuations.deinit(self.allocator());

    self.objects.symbols.deinit(self.allocator());
}

pub inline fn allocator(self: Vm) std.mem.Allocator {
    return self.options.allocator;
}

pub fn builder(self: *Vm) Builder {
    return Builder.init(self);
}

pub fn inspector(self: *Vm) Inspector {
    return Inspector.init(self);
}

pub fn pretty(self: *const Vm, val: Val) PrettyPrinter {
    return PrettyPrinter{ .vm = self, .val = val };
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

pub fn evalStr(self: *Vm, source: []const u8, maybe_env: ?Handle(Module)) Error!Val {
    var reader = self.read(source);
    var return_val = Val.initEmptyList();
    while (try reader.readNext()) |raw_expr| {
        return_val = try self.evalExpr(raw_expr, maybe_env);
    }
    return return_val;
}

pub fn expectEval(self: *Vm, expect: []const u8, source: []const u8) !void {
    const actual = try self.evalStr(source, null);
    try testing.expectFmt(expect, "{f}", .{self.pretty(actual)});
}

pub fn evalExpr(self: *Vm, expr: Val, maybe_env: ?Handle(Module)) Error!Val {
    const env = maybe_env orelse try self.inspector().getReplEnv();
    const proc = try self.compile(expr, env);
    self.context.reset();
    try self.context.push(self.allocator(), proc);
    try (Instruction{ .eval = 0 }).execute(self);
    return try instruction.executeUntilEnd(self);
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
        try vm.evalStr("((lambda (x) (+ x 30 2)) 10)", null),
    );
}
