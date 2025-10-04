const std = @import("std");
const testing = std.testing;

const Builder = @import("Builder.zig");
const Cons = @import("Cons.zig");
const Handle = @import("object_pool.zig").Handle;
const Inspector = @import("Inspector.zig");
const Module = @import("Module.zig");
const ObjectPool = @import("object_pool.zig").ObjectPool;
const PrettyPrinter = @import("PrettyPrinter.zig");
const Reader = @import("Reader.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");

const Vm = @This();

options: Options,
objects: Objects,

pub const Options = struct {
    allocator: std.mem.Allocator,
};

const Objects = struct {
    symbols: Symbol.Interner,
    cons: ObjectPool(Cons) = .{},
    modules: ObjectPool(Module) = .{},

    pub fn init(alloc: std.mem.Allocator) Objects {
        return Objects{
            .symbols = Symbol.Interner.init(alloc),
        };
    }
};

pub const Error = error{
    OutOfMemory,
    NotImplemented,
    ReadError,
    UndefinedBehavior,
    WrongType,
    Unreachable,
};

pub fn init(options: Options) Error!Vm {
    var vm = Vm{
        .options = options,
        .objects = Objects.init(options.allocator),
    };
    errdefer vm.deinit();
    try vm.initLibraries();
    return vm;
}

fn initLibraries(vm: *Vm) Error!void {
    const b = vm.builder();

    _ = try b.makeEnvironment(&.{
        (try b.makeSymbol(Symbol.init("scheme"))).data.symbol,
        (try b.makeSymbol(Symbol.init("base"))).data.symbol,
    }, &.{});
    _ = try b.makeEnvironment(&.{
        (try b.makeSymbol(Symbol.init("user"))).data.symbol,
        (try b.makeSymbol(Symbol.init("repl"))).data.symbol,
    }, &.{});
}

pub fn deinit(self: *Vm) void {
    self.objects.cons.deinit(self.allocator());

    var modules_iter = self.objects.modules.iterator();
    while (modules_iter.next()) |module| module.deinit(self.allocator());
    self.objects.modules.deinit(self.allocator());

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
    const items = [_]Val{ b.makeInt(1), b.makeInt(2), b.makeInt(3) };
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
