//! Continuation representation for the Scheme interpreter.
//!
//! Continuations capture the execution context at a particular point,
//! allowing for non-local control flow and advanced control structures.

const std = @import("std");
const testing = std.testing;

const Context = @import("../Context.zig");

const Continuation = @This();

/// The captured execution context containing the stack and frame information
context: Context,

/// Initializes a new continuation by capturing the current context state.
///
/// Args:
///   allocator: The memory allocator to use for copying context data.
///   current_context: The context to capture.
///
/// Returns:
///   A new Continuation containing a copy of the execution state.
pub fn init(allocator: std.mem.Allocator, current_context: *const Context) std.mem.Allocator.Error!Continuation {
    return Continuation{
        .context = try current_context.clone(allocator),
    };
}

/// Deinitializes the continuation, freeing all captured context data.
///
/// Args:
///   self: Pointer to the Continuation to deinitialize.
///   allocator: The allocator used to create the continuation.
pub fn deinit(self: *Continuation, allocator: std.mem.Allocator) void {
    self.context.deinit(allocator);
}
