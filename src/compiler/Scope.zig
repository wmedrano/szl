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

    pub fn init(id: usize) Id {
        return Id{ .id = id };
    }
};

/// Type of binding.
pub const Type = enum {
    argument, // Procedure parameter
    proc,     // The current procedure (for recursion)
    local,    // Local variable
    hidden,   // Temporarily unavailable (during let binding)
    free,     // Cleared binding, slot can be reused
};

/// Maps a symbol name to a stack index.
pub const Binding = struct {
    name: Symbol.Interned,
    index: isize,
    type: Type,
};

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

/// Finds the next available stack slot.
fn nextAvailableSlot(self: Scope) isize {
    for (0..self.bindings.items.len + 1) |idx| {
        var ok = true;
        for (self.bindings.items) |b| {
            if (b.index == idx and b.type != .free) {
                ok = false;
                break;
            }
        }
        if (ok) return @intCast(idx);
    }
    unreachable;
}

/// Creates a new local binding with an available slot.
pub fn addLocal(self: *Scope, allocator: std.mem.Allocator, symbol: Symbol.Interned, binding_type: Type) Error!Id {
    const index = self.nextAvailableSlot();
    const binding = Binding{
        .name = symbol,
        .index = index,
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

/// Returns the total number of local variable slots.
pub fn count(self: *Scope) usize {
    var max_index: isize = 0;
    for (self.bindings.items) |binding| {
        if (binding.index + 1 > max_index) max_index = binding.index + 1;
    }
    return @intCast(max_index);
}
