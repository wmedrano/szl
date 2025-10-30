const std = @import("std");
const testing = std.testing;

const Compiler = @import("compiler/Compiler.zig");
const Reader = @import("compiler/Reader.zig");
const Context = @import("Context.zig");
const Gc = @import("Gc.zig");
const instruction = @import("instruction.zig");
const Instruction = @import("instruction.zig").Instruction;
const builtins = @import("schemelib/base.zig");
const sizzle_unstable_compiler = @import("schemelib/sizzle_unstable_compiler.zig");
const Box = @import("types/Box.zig");
const Continuation = @import("types/Continuation.zig");
const ErrorDetails = @import("types/ErrorDetails.zig");
const Module = @import("types/Module.zig");
const NativeProc = @import("types/NativeProc.zig");
const Handle = @import("types/object_pool.zig").Handle;
const ObjectPool = @import("types/object_pool.zig").ObjectPool;
const Pair = @import("types/Pair.zig");
const Parameter = @import("types/Parameter.zig");
const Port = @import("types/Port.zig");
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
    UncaughtException,
    UndefinedBehavior,
};

pub const Objects = struct {
    symbols: Symbol.Interner,
    pairs: ObjectPool(Pair) = .{},
    strings: ObjectPool(String) = .{},
    modules: ObjectPool(Module) = .{},
    procs: ObjectPool(Proc) = .{},
    vectors: ObjectPool(Vector) = .{},
    bytevectors: ObjectPool(ByteVector) = .{},
    boxes: ObjectPool(Box) = .{},
    continuations: ObjectPool(Continuation) = .{},
    syntax_rules: ObjectPool(SyntaxRules) = .{},
    records: ObjectPool(Record) = .{},
    record_descriptors: ObjectPool(Record.Descriptor) = .{},
    parameters: ObjectPool(Parameter) = .{},
    ports: ObjectPool(Port) = .{},
    error_details: ObjectPool(ErrorDetails) = .{},

    pub fn init(alloc: std.mem.Allocator) error{OutOfMemory}!Objects {
        return Objects{
            .symbols = try Symbol.Interner.init(alloc),
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
        self.boxes.deinit(alloc);
        self.continuations.applyAll(standard_deinit);
        self.continuations.deinit(alloc);
        self.syntax_rules.applyAll(standard_deinit);
        self.syntax_rules.deinit(alloc);
        self.records.applyAll(standard_deinit);
        self.records.deinit(alloc);
        self.record_descriptors.applyAll(standard_deinit);
        self.record_descriptors.deinit(alloc);
        self.parameters.applyAll(standard_deinit);
        self.parameters.deinit(alloc);
        self.ports.deinit(alloc);
        self.error_details.applyAll(standard_deinit);
        self.error_details.deinit(alloc);
    }
};

pub fn init(options: Options) Error!Vm {
    var vm = Vm{
        .options = options,
        .objects = try Objects.init(options.allocator),
        .context = try Context.init(options.allocator),
    };
    errdefer vm.deinit();
    var error_details = ErrorDetails{};
    errdefer {
        // This should not happen since initialization is well tested in
        // release.
        std.debug.print("{f}", .{error_details.pretty(&vm, .color)});
    }
    defer error_details.deinit(options.allocator);
    try vm.initLibraries(&error_details);
    _ = try vm.runGc();
    return vm;
}

fn initLibraries(vm: *Vm, error_details: *ErrorDetails) Error!void {
    const b = vm.builder();

    // (scheme base)
    const global_handle = try builtins.init(vm, error_details);
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

test "makePort creates port values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const port_stdin = try b.makePort(.stdin);
    const port_stdout = try b.makePort(.stdout);
    const port_null = try b.makePort(.null);

    // Test external representation (default)
    try testing.expectFmt("#<port:input:stdin>", "{f}", .{vm.pretty(port_stdin, .{})});
    try testing.expectFmt("#<port:output:stdout>", "{f}", .{vm.pretty(port_stdout, .{})});
    try testing.expectFmt("#<port:null>", "{f}", .{vm.pretty(port_null, .{})});

    // Test display representation
    try testing.expectFmt("#<stdin>", "{f}", .{vm.pretty(port_stdin, .{ .repr = .display })});
    try testing.expectFmt("#<stdout>", "{f}", .{vm.pretty(port_stdout, .{ .repr = .display })});
    try testing.expectFmt("#<null port>", "{f}", .{vm.pretty(port_null, .{ .repr = .display })});

    // Verify we can inspect the ports
    const insp = vm.inspector();
    const stdin_port = try insp.asPort(port_stdin);
    const stdout_port = try insp.asPort(port_stdout);
    const null_port = try insp.asPort(port_null);

    try testing.expectEqual(.stdin, stdin_port.inner);
    try testing.expectEqual(.stdout, stdout_port.inner);
    try testing.expectEqual(.null, null_port.inner);
}

pub fn evalStr(
    self: *Vm,
    source: []const u8,
    maybe_env: ?Handle(Module),
    error_details: *ErrorDetails,
) Error!Val {
    var reader = Reader.init(self, source);
    var return_val = Val.initUnspecified();

    while (try reader.readNext(error_details)) |raw_expr| {
        return_val = try self.evalExpr(raw_expr, maybe_env, error_details);
    }
    return return_val;
}

pub fn expectEval(self: *Vm, expect: []const u8, source: []const u8) !void {
    var diag: ErrorDetails = .{};
    defer diag.deinit(testing.allocator);
    errdefer std.debug.print("Error details:\n{f}\n", .{diag.pretty(self, .nocolor)});

    const actual = try self.evalStr(source, null, &diag);
    try testing.expectFmt(expect, "{f}", .{self.pretty(actual, .{})});
}

pub fn expectError(self: *Vm, expected_error: anyerror, source: []const u8) !void {
    var diag: ErrorDetails = .{};
    defer diag.deinit(testing.allocator);
    errdefer std.debug.print("Error details:\n{f}\n", .{diag.pretty(self, .nocolor)});

    const actual_error = self.evalStr(source, null, &diag);
    try testing.expectError(expected_error, actual_error);
}

pub fn evalExpr(
    self: *Vm,
    expr: Val,
    maybe_env: ?Handle(Module),
    error_details: *ErrorDetails,
) Error!Val {
    const env = maybe_env orelse try self.inspector().getReplEnv(error_details);
    const proc = try self.compile(expr, env, error_details);
    self.context.reset();
    try self.context.push(self.allocator(), proc);
    try (Instruction{ .eval = 0 }).execute(self, error_details);
    return try instruction.executeUntilEnd(self, error_details);
}

fn compile(self: *Vm, expr: Val, env: Handle(Module), error_details: *ErrorDetails) !Val {
    var arena = std.heap.ArenaAllocator.init(self.allocator());
    defer arena.deinit();
    var compiler = Compiler.init(&arena, self, env);
    const proc = try compiler.compile(expr, error_details);
    return proc;
}

test evalStr {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var error_details = ErrorDetails{};
    defer error_details.deinit(testing.allocator);
    try testing.expectEqual(
        Val.initInt(42),
        try vm.evalStr("((lambda (x) (+ x 30 2)) 10)", null, &error_details),
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

test "make-parameter creates procedure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    var error_details = ErrorDetails{};
    defer error_details.deinit(testing.allocator);
    _ = try vm.evalStr("(define p (make-parameter 10))", null, &error_details);
    try vm.expectEval("10", "(p)");
}

test "make-parameter with wrong arg count returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // make-parameter requires exactly 1 arg
    try vm.expectError(error.UncaughtException, "(make-parameter)");
    try vm.expectError(error.UncaughtException, "(make-parameter 1 2)");
    try vm.expectError(error.UncaughtException, "(make-parameter 1 2 3)");

    // Calling parameter with > 1 arg is an error
    var error_details = ErrorDetails{};
    defer error_details.deinit(testing.allocator);
    _ = try vm.evalStr("(define p (make-parameter 10))", null, &error_details);
    try vm.expectError(error.UncaughtException, "(p 1 2)");
}
