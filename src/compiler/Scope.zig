//! Local variable bindings for procedure compilation.
//!
//! Maps symbol names to stack indices during compilation.

const std = @import("std");

const Symbol = @import("../types/Symbol.zig");

/// Local variable bindings for procedure compilation.
///
/// Manages symbol-to-stack-index mappings during compilation, tracking
/// local variables, arguments, captured values, and their visibility states.
/// Supports lexical scoping with proper shadowing semantics.
const Scope = @This();

/// A set of interned symbols, used to track available bindings.
pub const BindingSet = std.AutoHashMap(Symbol.Interned, void);

/// Symbol to stack index mappings for the current procedure.
bindings: std.ArrayList(Binding) = .{},

capture_candidates: BindingSet,

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
    captured, // A binding that was captured from its environment.
    free, // Cleared binding, slot can be reused
};

/// Maps a symbol name to a stack index with type information.
///
/// Represents a single variable binding within a scope, containing
/// the symbol name, its stack position, and its binding type which
/// determines visibility and behavior during compilation.
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

pub fn init(allocator: std.mem.Allocator, prior: ?*Scope) Error!Scope {
    const captured = if (prior) |p| try p.availableBindings(allocator) else BindingSet.init(allocator);

    return Scope{
        .bindings = .{},
        .capture_candidates = captured,
    };
}

/// Deallocates all resources used by the scope.
///
/// Args:
///   self: The scope instance to deallocate.
///   allocator: The allocator used to free memory.
pub fn deinit(self: *Scope, allocator: std.mem.Allocator) void {
    self.bindings.deinit(allocator);
    self.capture_candidates.deinit();
}

/// Adds a binding to the scope and returns its unique ID.
///
/// Args:
///   self: The scope instance.
///   allocator: The allocator for memory allocation.
///   binding: The binding to add to the scope.
///
/// Returns:
///   A unique ID that can be used to reference this binding.
///
/// Errors:
///   - OutOfMemory if allocation fails.
pub fn addBinding(self: *Scope, allocator: std.mem.Allocator, binding: Binding) Error!Id {
    const id = Id.init(self.bindings.items.len);
    try self.bindings.append(allocator, binding);
    return id;
}

/// Retrieves a mutable reference to a binding by its ID.
///
/// Args:
///   self: The scope instance.
///   id: The unique ID of the binding to retrieve.
///
/// Returns:
///   A mutable pointer to the binding, or null if the ID is invalid.
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

/// Clears a binding by marking it as free for reuse.
///
/// Args:
///   self: The scope instance.
///   id: The unique ID of the binding to clear.
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
    /// The number of captured bindings. Note that a captured binding is a
    /// subset of locals.
    captured: u32 = 0,
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
            .free, .hidden, .local, .captured => ret.locals = @max(ret.locals, binding.index + 1),
            .proc => {},
        }
        switch (binding.type) {
            .captured => ret.captured += 1,
            else => {},
        }
    }
    return ret;
}

/// Returns the set of available Symbol.Interned bindings in this scope.
///
/// Collects all non-hidden and non-free binding names from both current scope
/// bindings and captured bindings from parent scopes. The returned set
/// contains symbol names that can be resolved in the current scope.
///
/// Returns:
///   A hash set containing all available interned symbols.
pub fn availableBindings(self: *Scope, allocator: std.mem.Allocator) Error!BindingSet {
    var bindings = BindingSet.init(allocator);

    // Add captured bindings from parent scopes
    var captured_iter = self.capture_candidates.iterator();
    while (captured_iter.next()) |entry| {
        try bindings.put(entry.key_ptr.*, {});
    }

    // Add current scope bindings
    for (self.bindings.items) |binding| {
        if (binding.type != .hidden and binding.type != .free) {
            try bindings.put(binding.name, {});
        }
    }

    return bindings;
}
