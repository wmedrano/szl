//! Value representation system for the Scheme interpreter.
//!
//! This module defines the core value types that can be stored and manipulated
//! in the Scheme interpreter. Values are represented using a tagged union to
//! efficiently handle the dynamic typing inherent in Scheme.

pub const Symbol = @import("Symbol.zig");

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
    end_of_list,

    /// Represents a symbol value using an interned symbol for efficient comparison.
    /// Symbols in Scheme are identifiers that evaluate to themselves when quoted
    /// or are used for variable/function names when unquoted.
    symbol: Symbol.Interned,
};
