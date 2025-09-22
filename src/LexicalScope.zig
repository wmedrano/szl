//! Lexical scope management for variable bindings during compilation.
//!
//! Provides functionality to manage lexical bindings that map symbol names
//! to local variable indices within procedure compilation scope.

const std = @import("std");

const Symbol = @import("Symbol.zig");

const LexicalScope = @This();

/// List of lexical bindings (local variables) visible in the current compilation scope.
/// These bindings are used to resolve local variable references during compilation.
bindings: std.ArrayList(Binding) = .{},

/// Unique identifier for lexical scope instances.
/// Used to track and reference specific lexical scopes during compilation.
pub const Id = struct {
    /// The unique identifier value.
    id: usize,

    /// Creates a new lexical scope identifier.
    ///
    /// Args:
    ///   id: The unique identifier value to use.
    ///
    /// Returns:
    ///   A new Id instance with the specified identifier.
    pub fn init(id: usize) Id {
        return Id{ .id = id };
    }
};

/// Type of lexical binding.
///
/// Binding types represent different states and scopes of variable bindings:
/// - `argument`: Procedure parameter, available from function start
/// - `proc`: The current procedure, available from function start
/// - `local`: Local variable, available for normal resolution
/// - `hidden`: Temporary state for bindings not yet available (used in let expressions)
/// - `free`: Cleared binding, slot can be reused
pub const BindingType = enum {
    argument,
    proc,
    local,
    hidden,
    free,
};

/// Represents a lexical binding that maps a symbol name to a local variable index.
/// Used for resolving local variable references during procedure compilation.
pub const Binding = struct {
    /// The interned symbol name for this binding.
    name: Symbol.Interned,
    /// The local variable index in the stack frame where this binding's value is stored.
    index: isize,
    /// The type of this binding.
    type: BindingType,
};

/// Errors that can occur during lexical scope operations.
pub const Error = error{
    /// Memory allocation failed.
    OutOfMemory,
};

/// Deallocates resources used by the lexical scope.
/// Cleans up the bindings list.
///
/// Args:
///   self: The lexical scope instance to clean up.
///   allocator: The allocator used for the bindings list.
pub fn deinit(self: *LexicalScope, allocator: std.mem.Allocator) void {
    self.bindings.deinit(allocator);
}

/// Adds a lexical binding to the scope's binding list.
/// Creates a mapping from a symbol name to a local variable index,
/// enabling the compiler to resolve local variable references within procedure scope.
/// Bindings are added in order to support proper lexical scoping.
///
/// Args:
///   self: The lexical scope instance.
///   allocator: The allocator to use for the bindings list.
///   binding: The binding to add, containing the symbol name and stack index.
///
/// Returns:
///   The Id of the newly added binding (its index in the bindings list).
///   Returns an error if memory allocation fails.
pub fn addBinding(self: *LexicalScope, allocator: std.mem.Allocator, binding: Binding) Error!Id {
    const id = Id.init(self.bindings.items.len);
    try self.bindings.append(allocator, binding);
    return id;
}

/// Gets a lexical binding by its identifier.
/// Returns a pointer to the binding if it exists, otherwise null.
///
/// Args:
///   self: The lexical scope instance.
///   id: The identifier of the binding to retrieve.
///
/// Returns:
///   A pointer to the binding if found, or null if the id is invalid.
pub fn getBinding(self: *LexicalScope, id: Id) ?*Binding {
    if (id.id >= self.bindings.items.len) return null;
    return &self.bindings.items[id.id];
}

/// Finds a lexical binding by symbol name.
/// Searches bindings in reverse order to implement proper lexical scoping
/// where inner bindings shadow outer ones (most recent binding wins).
/// Skips hidden and free bindings as they are not available for resolution.
///
/// Args:
///   self: The lexical scope instance.
///   symbol: The interned symbol to search for.
///
/// Returns:
///   The binding if found and available, or null if not found.
pub fn resolve(self: *LexicalScope, symbol: Symbol.Interned) ?Binding {
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

/// Clears a lexical binding by setting its type to free.
/// This effectively removes the binding from scope resolution
/// while preserving the binding slot for potential reuse.
///
/// Args:
///   self: The lexical scope instance.
///   id: The identifier of the binding to clear.
pub fn clear(self: *LexicalScope, id: Id) void {
    if (id.id >= self.bindings.items.len) return;
    self.bindings.items[id.id].type = .free;
}

/// Finds the lowest available slot index for a new variable binding.
/// Searches through all possible indices starting from 0 to find one that
/// is not currently occupied by a non-free binding.
///
/// Args:
///   self: The lexical scope instance to search.
///
/// Returns:
///   The lowest available slot index that can be used for a new binding.
fn reserveSlot(self: LexicalScope) isize {
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

/// Reserves a variable slot and creates a binding for it.
/// Uses reserveSlot to find an available index and creates a binding with the specified type.
///
/// Args:
///   self: The lexical scope instance.
///   allocator: The allocator to use for the bindings list.
///   symbol: The interned symbol name for the variable.
///   binding_type: The type of binding to create.
///
/// Returns:
///   The Id of the newly created variable binding.
///   Returns an error if memory allocation fails.
pub fn addVariable(self: *LexicalScope, allocator: std.mem.Allocator, symbol: Symbol.Interned, binding_type: BindingType) Error!Id {
    const index = self.reserveSlot();
    const binding = Binding{
        .name = symbol,
        .index = index,
        .type = binding_type,
    };
    return self.addBinding(allocator, binding);
}

/// Returns the number of procedure arguments in the current lexical scope.
/// Finds the largest index among argument bindings and returns index + 1.
///
/// Args:
///   self: The lexical scope instance.
///
/// Returns:
///   The number of procedure arguments (largest procedure_arg index + 1).
pub fn procedureArgCount(self: *LexicalScope) usize {
    var arg_count: isize = 0;
    for (self.bindings.items) |binding| {
        if (binding.type == .argument) {
            if (binding.index + 1 > arg_count) arg_count = binding.index + 1;
        }
    }
    return @intCast(arg_count);
}

/// Returns the total number of local variable slots used in the current lexical scope.
/// Finds the largest index among all bindings and returns index + 1.
/// This includes both procedure arguments and local variables.
///
/// Args:
///   self: The lexical scope instance.
///
/// Returns:
///   The total number of local variable slots (largest binding index + 1).
pub fn count(self: *LexicalScope) usize {
    var arg_count: isize = 0;
    for (self.bindings.items) |binding| {
        if (binding.index + 1 > arg_count) arg_count = binding.index + 1;
    }
    return @intCast(arg_count);
}
