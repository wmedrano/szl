//! Local variable bindings for procedure compilation.
//!
//! Maps symbol names to stack indices during compilation.

const std = @import("std");

const Symbol = @import("../types/Symbol.zig");

const Scope = @This();

/// Symbol to stack index mappings for the current procedure.
bindings: std.ArrayList(Binding) = .{},

/// Unique identifier for bindings.
pub const Id = struct {
    id: usize,

    /// Creates a new binding ID.
    pub fn init(id: usize) Id {
        return Id{ .id = id };
    }
};

/// Type of binding.
pub const Type = enum {
    argument, // Procedure parameter
    proc, // The current procedure (for recursion)
    local, // Local variable
    hidden, // Temporarily unavailable (during let binding)
    free, // Cleared binding, slot can be reused
};

/// Maps a symbol name to a stack index.
pub const Binding = struct {
    /// The interned symbol name.
    name: Symbol.Interned,
    /// Stack index for this binding.
    index: isize,
    /// Type classification of this binding.
    type: Type,
};

/// Errors that can occur during scope operations.
pub const Error = error{
    OutOfMemory,
};

/// Deallocates resources.
pub fn deinit(self: *Scope, allocator: std.mem.Allocator) void {
    self.bindings.deinit(allocator);
}

/// Adds a binding and returns its ID.
pub fn addBinding(self: *Scope, allocator: std.mem.Allocator, binding: Binding) Error!Id {
    const id = Id.init(self.bindings.items.len);
    try self.bindings.append(allocator, binding);
    return id;
}

/// Gets a binding by ID.
pub fn getBinding(self: *Scope, id: Id) ?*Binding {
    if (id.id >= self.bindings.items.len) return null;
    return &self.bindings.items[id.id];
}

/// Resolves a symbol to its binding.
/// Searches in reverse order so inner bindings shadow outer ones.
pub fn resolve(self: *Scope, symbol: Symbol.Interned) ?Binding {
    var i = self.bindings.items.len;
    while (i > 0) {
        i -= 1;
        const binding = self.bindings.items[i];
        if (binding.name.eql(symbol) and binding.type != .hidden and binding.type != .free) {
            return binding;
        }
    }
    return null;
}

/// Clears a binding by marking it as free.
pub fn clear(self: *Scope, id: Id) void {
    if (id.id >= self.bindings.items.len) return;
    self.bindings.items[id.id].type = .free;
}

/// Creates a new local binding with an available slot.
/// The binding index is automatically assigned based on the current binding count.
pub fn addLocal(self: *Scope, allocator: std.mem.Allocator, symbol: Symbol.Interned, binding_type: Type) Error!Id {
    const index = self.bindings.items.len;
    const binding = Binding{
        .name = symbol,
        .index = @intCast(index),
        .type = binding_type,
    };
    return self.addBinding(allocator, binding);
}

/// Returns the number of procedure arguments.
pub fn argCount(self: *Scope) usize {
    var max_arg_index: isize = 0;
    for (self.bindings.items) |binding| {
        if (binding.type == .argument) {
            if (binding.index + 1 > max_arg_index) max_arg_index = binding.index + 1;
        }
    }
    return @intCast(max_arg_index);
}

pub const Stats = struct {
    args: usize = 0,
    locals: usize = 0,
    total: usize = 0,
};

pub fn stats(self: *Scope) Stats {
    var ret = Stats{};
    for (self.bindings.items) |binding| {
        switch (binding.type) {
            .argument => ret.args = @max(ret.args, binding.index),
            .free, .hidden, .local => @max(ret.args, binding.index),
            .proc => {},
        }
    }
    return ret;
}

/// Returns the total number of local variable slots.
pub fn count(self: *Scope) usize {
    var max_index: isize = 0;
    for (self.bindings.items) |binding| {
        if (binding.index + 1 > max_index) max_index = binding.index + 1;
    }
    return @intCast(max_index);
}
