const std = @import("std");
const testing = std.testing;

const Compiler = @import("compiler/Compiler.zig");
const Reader = @import("compiler/Reader.zig");
const Context = @import("Context.zig");
const Diagnostics = @import("Diagnostics.zig");
const Gc = @import("Gc.zig");
const instruction = @import("instruction.zig");
const Instruction = @import("instruction.zig").Instruction;
const builtins = @import("schemelib/base.zig");
const sizzle_unstable_compiler = @import("schemelib/sizzle_unstable_compiler.zig");
const Continuation = @import("types/Continuation.zig");
const Module = @import("types/Module.zig");
const NativeProc = @import("types/NativeProc.zig");
const Handle = @import("types/object_pool.zig").Handle;
const ObjectPool = @import("types/object_pool.zig").ObjectPool;
const Pair = @import("types/Pair.zig");
const Proc = @import("types/Proc.zig");
const Record = @import("types/Record.zig");
const String = @import("types/String.zig");
const Symbol = @import("types/Symbol.zig");
const SyntaxRules = @import("types/SyntaxRules.zig");
const Val = @import("types/Val.zig");
const Vector = Val.Vector;
const ByteVector = Val.ByteVector;
const Builder = @import("utils/Builder.zig");
const Inspector = @import("utils/Inspector.zig");
const PrettyPrinter = @import("utils/PrettyPrinter.zig");

const Vm = @This();

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
    UncaughtException,
};

pub const Objects = struct {
    symbols: Symbol.Interner,
    pairs: ObjectPool(Pair) = .{},
    strings: ObjectPool(String) = .{},
    modules: ObjectPool(Module) = .{},
    procs: ObjectPool(Proc) = .{},
    vectors: ObjectPool(Vector) = .{},
    bytevectors: ObjectPool(ByteVector) = .{},
    continuations: ObjectPool(Continuation) = .{},
    syntax_rules: ObjectPool(SyntaxRules) = .{},
    records: ObjectPool(Record) = .{},
    record_descriptors: ObjectPool(Record.Descriptor) = .{},

    pub fn init(alloc: std.mem.Allocator) Objects {
        return Objects{
            .symbols = Symbol.Interner.init(alloc),
        };
    }

    pub fn deinit(self: *Objects, alloc: std.mem.Allocator) void {
        self.pairs.deinit(alloc);
        self.symbols.deinit(alloc);

        const standard_deinit = struct {
            allocator: std.mem.Allocator,
            pub fn apply(this: @This(), obj: anytype) void {
                obj.deinit(this.allocator);
            }
        }{ .allocator = alloc };
        self.strings.applyAll(standard_deinit);
        self.strings.deinit(alloc);
        self.modules.applyAll(standard_deinit);
        self.modules.deinit(alloc);
        self.procs.applyAll(standard_deinit);
        self.procs.deinit(alloc);
        self.vectors.applyAll(standard_deinit);
        self.vectors.deinit(alloc);
        self.bytevectors.applyAll(standard_deinit);
        self.bytevectors.deinit(alloc);
        self.continuations.applyAll(standard_deinit);
        self.continuations.deinit(alloc);
        self.syntax_rules.applyAll(standard_deinit);
        self.syntax_rules.deinit(alloc);
        self.records.applyAll(standard_deinit);
        self.records.deinit(alloc);
        self.record_descriptors.applyAll(standard_deinit);
        self.record_descriptors.deinit(alloc);
    }
};

pub fn init(options: Options) Error!Vm {
    var vm = Vm{
        .options = options,
        .objects = Objects.init(options.allocator),
        .context = try Context.init(options.allocator),
    };
    errdefer vm.deinit();
    try vm.initLibraries();
    _ = try vm.runGc();
    return vm;
}

fn initLibraries(vm: *Vm) Error!void {
    const b = vm.builder();

    // (scheme base)
    const global_handle = try builtins.init(vm);
    // (sizzle unstable compiler)
    _ = try sizzle_unstable_compiler.init(vm);

    // (user repl)
    const repl_handle = try b.makeEnvironment(&.{
        (try b.makeStaticSymbolHandle("user")),
        (try b.makeStaticSymbolHandle("repl")),
    }, &.{});
    const repl_mod = try vm.inspector().handleToModule(repl_handle);
    const global_mod = try vm.inspector().handleToModule(global_handle);
    try repl_mod.import(vm.allocator(), global_mod.*);
}

pub fn deinit(self: *Vm) void {
    self.context.deinit(self.allocator());
    self.objects.deinit(self.allocator());
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

pub fn pretty(self: *const Vm, val: Val, options: PrettyPrinter.Options) PrettyPrinter {
    return PrettyPrinter{ .vm = self, .val = val, .options = options };
}

test builder {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const items = [_]Val{ Val.initInt(1), Val.initInt(2), Val.initInt(3) };
    try testing.expectFmt(
        "(1 2 3)",
        "{f}",
        .{vm.pretty(try b.makeList(&items), .{})},
    );
}

pub fn evalStr(
    self: *Vm,
    source: []const u8,
    maybe_env: ?Handle(Module),
    diagnostics: ?*Diagnostics,
) Error!Val {
    var reader = Reader.init(self, source);
    var return_val = Val.initEmptyList();

    while (try reader.readNext(diagnostics)) |raw_expr| {
        return_val = try self.evalExpr(raw_expr, maybe_env, diagnostics);
    }
    return return_val;
}

pub fn expectEval(self: *Vm, expect: []const u8, source: []const u8) !void {
    const actual = try self.evalStr(source, null, null);
    try testing.expectFmt(expect, "{f}", .{self.pretty(actual, .{})});
}

pub fn evalExpr(
    self: *Vm,
    expr: Val,
    maybe_env: ?Handle(Module),
    diagnostics: ?*Diagnostics,
) Error!Val {
    const env = maybe_env orelse try self.inspector().getReplEnv(diagnostics);
    const proc = try self.compile(expr, env);
    self.context.reset();
    try self.context.push(self.allocator(), proc);
    try (Instruction{ .eval = 0 }).execute(self, diagnostics);
    return try instruction.executeUntilEnd(self, diagnostics);
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
        try vm.evalStr("((lambda (x) (+ x 30 2)) 10)", null, null),
    );
}

pub fn runGc(self: *Vm) !usize {
    var arena = std.heap.ArenaAllocator.init(self.allocator());
    var gc = Gc{ .arena = &arena };
    defer arena.deinit();

    // Mark all reachable values within the environment.
    try gc.markContext(self, self.context);

    // Mark all modules (they are always roots)
    var module_iter = self.objects.modules.iterator();
    while (module_iter.next()) |entry| try gc.markModule(self, entry.handle);

    return gc.sweep(self);
}

test "runGc is ok" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval(
        "42",
        "((lambda (x) (+ x 30 2)) 10)",
    );
    try testing.expectEqual(34, try vm.runGc());
    try testing.expectEqual(0, try vm.runGc());
    try vm.expectEval("42", "((lambda (x) (+ x 30 2)) 10)");
}
