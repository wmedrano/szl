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
    index: i32,
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

/// Statistics about bindings in a scope.
pub const Stats = struct {
    /// Number of argument bindings.
    args: u32 = 0,
    /// Number of local variable bindings.
    locals: u32 = 0,
};

/// Computes statistics about the bindings in this scope.
///
/// Analyzes all bindings to count arguments, locals, and total bindings.
/// This is useful for understanding the scope's memory layout and requirements.
///
/// Returns:
///   A Stats struct containing counts of different binding types.
pub fn stats(self: *Scope) Stats {
    var ret = Stats{};
    for (self.bindings.items) |binding| {
        switch (binding.type) {
            .argument => ret.args = @max(ret.args, binding.index + 1),
            .free, .hidden, .local => ret.locals = @max(ret.args, binding.index + 1),
            .proc => {},
        }
    }
    return ret;
}
