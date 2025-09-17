//! Virtual machine and runtime environment for the Scheme interpreter.
//!
//! This module provides the core virtual machine that manages the execution
//! environment for Scheme programs. It handles symbol interning, value conversion,
//! and provides the foundation for evaluating Scheme expressions.

const std = @import("std");
const testing = std.testing;

const Builder = @import("Builder.zig");
const Cons = @import("Cons.zig");
const Handle = @import("object_pool.zig").Handle;
const ObjectPool = @import("object_pool.zig").ObjectPool;
const PrettyPrinter = @import("PrettyPrinter.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");

const Vm = @This();

allocator: std.mem.Allocator,
interner: Symbol.Interner,
cons: ObjectPool(Cons),

/// Configuration options for initializing the virtual machine.
pub const Options = struct {
    /// The allocator to use for memory management within the VM.
    allocator: std.mem.Allocator,
};

/// Initializes a new virtual machine with the given options.
/// Creates a new symbol interner and sets up the runtime environment.
///
/// Args:
///   options: Configuration options including the allocator to use.
///
/// Returns:
///   A new Vm instance ready for use.
pub fn init(options: Options) Vm {
    const interner = Symbol.Interner.init(options.allocator);
    return Vm{
        .allocator = options.allocator,
        .interner = interner,
        .cons = ObjectPool(Cons).init(),
    };
}

/// Releases all memory associated with the virtual machine.
/// This includes the symbol interner and all internal data structures.
/// After calling this function, the VM should not be used.
///
/// Args:
///   self: Pointer to the VM to deinitialize.
pub fn deinit(self: *Vm) void {
    self.interner.deinit();
    self.cons.deinit(self.allocator);
}

/// Creates a new Builder instance for converting Zig values to Scheme values.
/// The Builder provides type-safe conversion from compile-time known Zig types
/// to the dynamic value system used by the Scheme interpreter.
///
/// Args:
///   self: Pointer to the VM that will be used by the Builder.
///
/// Returns:
///   A Builder instance configured to use this VM.
pub fn builder(self: *Vm) Builder {
    return Builder{ .vm = self };
}

/// Creates a PrettyPrinter for formatting a Scheme value.
/// The PrettyPrinter can be used with Zig's standard formatting functions
/// to display Scheme values in a human-readable format.
///
/// Args:
///   self: Pointer to the VM that owns the value's data.
///   val: The Scheme value to format.
///
/// Returns:
///   A PrettyPrinter instance ready for formatting.
pub fn pretty(self: *const Vm, val: Val) PrettyPrinter {
    return PrettyPrinter{ .vm = self, .val = val };
}

