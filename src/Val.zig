//! Value representation system for the Scheme interpreter.
//!
//! This module defines the core value types that can be stored and manipulated
//! in the Scheme interpreter. Values are represented using a tagged union to
//! efficiently handle the dynamic typing inherent in Scheme.

const Cons = @import("Cons.zig");
const object_pool = @import("object_pool.zig");
const Handle = object_pool.Handle;
const Symbol = @import("Symbol.zig");

const Val = @This();

/// The internal representation of this value.
/// Uses a tagged union to store different value types efficiently.
repr: Repr,

/// Tagged union representing all possible value types in the Scheme interpreter.
/// Each variant corresponds to a different Scheme data type that can be
/// stored and manipulated at runtime.
pub const Repr = union(enum) {
    /// Represents the end of a list (equivalent to '() or nil in Scheme).
    /// This is used to terminate linked list structures.
    nil,

    /// Represents a 64-bit signed integer value.
    /// Used for numeric computations and integer literals in Scheme.
    i64: i64,

    /// Represents a symbol value using an interned symbol for efficient comparison.
    /// Symbols in Scheme are identifiers that evaluate to themselves when quoted
    /// or are used for variable/function names when unquoted.
    symbol: Symbol.Interned,

    /// Represents a cons cell (pair) using a handle to an object pool.
    /// Cons cells are the fundamental building blocks for lists and pairs in Scheme.
    cons: Handle(Cons),
};
