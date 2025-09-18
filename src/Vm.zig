//! Virtual machine and runtime environment for the Scheme interpreter.
//!
//! This module provides the core virtual machine that manages the execution
//! environment for Scheme programs. It handles symbol interning, value conversion,
//! and provides the foundation for evaluating Scheme expressions.

const std = @import("std");

const Builder = @import("Builder.zig");
const Handle = @import("object_pool.zig").Handle;
const Inspector = @import("Inspector.zig");
const ObjectPool = @import("object_pool.zig").ObjectPool;
const Pair = @import("Pair.zig");
const PrettyPrinter = @import("PrettyPrinter.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");

const Vm = @This();

allocator: std.mem.Allocator,
interner: Symbol.Interner,
cons: ObjectPool(Pair),

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
        .cons = ObjectPool(Pair).init(),
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

pub fn inspector(self: *const Vm) Inspector {
    return Inspector{ .vm = self };
}
