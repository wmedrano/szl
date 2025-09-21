//! Lexical scope management for variable bindings during compilation.
//!
//! Provides functionality to manage lexical bindings that map symbol names
//! to local variable indices within procedure compilation scope.

const std = @import("std");

const Symbol = @import("Symbol.zig");

const LexicalScope = @This();

/// List of lexical bindings (local variables) visible in the current compilation scope.
/// These bindings are used to resolve local variable references during compilation.
bindings: std.ArrayList(LexicalBind) = .{},

/// Type of lexical binding.
pub const BindType = enum {
    procedure_arg,
    local_var,
};

/// Represents a lexical binding that maps a symbol name to a local variable index.
/// Used for resolving local variable references during procedure compilation.
pub const LexicalBind = struct {
    /// The interned symbol name for this binding.
    name: Symbol.Interned,
    /// The local variable index in the stack frame where this binding's value is stored.
    index: usize,
    /// The type of this binding.
    bind_type: BindType,
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
///   binding: The lexical binding to add, containing the symbol name and stack index.
///
/// Returns:
///   An error if memory allocation fails.
pub fn addBinding(self: *LexicalScope, allocator: std.mem.Allocator, binding: LexicalBind) Error!void {
    try self.bindings.append(allocator, binding);
}

/// Finds the stack index of a lexical binding by symbol name.
/// Searches bindings in reverse order to implement proper lexical scoping
/// where inner bindings shadow outer ones (most recent binding wins).
///
/// Args:
///   self: The lexical scope instance.
///   symbol: The interned symbol to search for.
///
/// Returns:
///   The stack index of the binding if found, or null if not found.
pub fn findBinding(self: *LexicalScope, symbol: Symbol.Interned) ?usize {
    var i = self.bindings.items.len;
    while (i > 0) {
        i -= 1;
        if (self.bindings.items[i].name.eql(symbol))
            return self.bindings.items[i].index;
    }
    return null;
}

/// Returns the number of procedure arguments in the current lexical scope.
/// Finds the largest index among procedure_arg bindings and returns index + 1.
///
/// Args:
///   self: The lexical scope instance.
///
/// Returns:
///   The number of procedure arguments (largest procedure_arg index + 1).
pub fn procedureArgCount(self: *LexicalScope) usize {
    var arg_count: usize = 0;
    for (self.bindings.items) |binding| {
        if (binding.bind_type == .procedure_arg) {
            if (binding.index + 1 > arg_count) arg_count = binding.index + 1;
        }
    }
    return arg_count;
}
