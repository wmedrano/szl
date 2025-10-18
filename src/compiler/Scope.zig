const std = @import("std");
const testing = std.testing;

const Module = @import("../types/Module.zig");
const Handle = @import("../types/object_pool.zig").Handle;
const Symbol = @import("../types/Symbol.zig");

const Scope = @This();

module: Handle(Module),
proc: ?Symbol,
args: []const Symbol = &.{},
locals: std.ArrayList(Local) = .{},
captures: Captures = .{},
captures_count: u32 = 0,

pub const Local = struct {
    name: Symbol,
    available: bool,
};

/// A map from captured to symbol to its captured index. If captured index
/// is `null`, then the variable has not been captured.
pub const Captures = std.AutoHashMapUnmanaged(Symbol, ?u32);

pub const Location = union(enum) {
    proc,
    arg: u32,
    local: u32,
    capture: u32,
    module: Symbol,
};

pub fn resolve(self: *Scope, name: Symbol) Location {
    // Locals
    var local_idx = self.locals.items.len;
    while (local_idx > 0) {
        local_idx -= 1;
        const item = self.locals.items[local_idx];
        if (item.available and item.name.eq(name))
            return Location{ .local = @intCast(local_idx) };
    }
    // Args
    for (self.args, 0..self.args.len) |arg, idx| {
        if (arg.eq(name)) return Location{ .arg = @intCast(idx) };
    }
    // Proc
    if (self.proc) |proc_name| {
        if (proc_name.eq(name)) return Location{ .proc = {} };
    }
    // Captures
    if (self.captures.getEntry(name)) |capture| {
        if (capture.value_ptr.*) |idx| return Location{ .capture = idx };
        const idx = self.captures_count;
        capture.value_ptr.* = self.captures_count;
        self.captures_count += 1;
        return Location{ .capture = idx };
    }
    return Location{ .module = name };
}

pub fn toCaptureCandidates(self: Scope, allocator: std.mem.Allocator) error{OutOfMemory}!Captures {
    var ret = Captures{};
    for (self.args) |arg| try ret.put(allocator, arg, null);
    for (self.locals.items) |local|
        if (local.available) try ret.put(allocator, local.name, null);
    return ret;
}

pub fn capturesSlice(self: Scope, allocator: std.mem.Allocator) error{OutOfMemory}![]Symbol {
    const ret = try allocator.alloc(Symbol, @intCast(self.captures_count));
    var captures_iter = self.captures.iterator();
    while (captures_iter.next()) |capture| {
        if (capture.value_ptr.*) |idx| {
            ret[@intCast(idx)] = capture.key_ptr.*;
        }
    }
    return ret;
}
