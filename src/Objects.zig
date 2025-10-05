const std = @import("std");

const Module = @import("Module.zig");
const ObjectPool = @import("object_pool.zig").ObjectPool;
const Pair = @import("Pair.zig");
const Proc = @import("Proc.zig");
const Symbol = @import("Symbol.zig");

const Objects = @This();

symbols: Symbol.Interner,
pairs: ObjectPool(Pair) = .{},
modules: ObjectPool(Module) = .{},
procs: ObjectPool(Proc) = .{},

pub fn init(alloc: std.mem.Allocator) Objects {
    return Objects{
        .symbols = Symbol.Interner.init(alloc),
    };
}
