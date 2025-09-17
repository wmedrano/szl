//! Virtual machine and runtime environment for the Scheme interpreter.
//!
//! This module provides the core virtual machine that manages the execution
//! environment for Scheme programs. It handles symbol interning, value conversion,
//! and provides the foundation for evaluating Scheme expressions.

const std = @import("std");
const testing = std.testing;

const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Builder = @import("Builder.zig");

const Vm = @This();

interner: Symbol.Interner,

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
        .interner = interner,
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
