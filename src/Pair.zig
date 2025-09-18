//! Pair implementation for the Scheme interpreter.
//!
//! This module provides the fundamental pair data structure used to build
//! lists and pairs in Scheme. A pair contains two values: car (first) and
//! cdr (rest), which can be any valid Scheme value.

const Val = @import("Val.zig");

const Pair = @This();

/// The first element of the pair (traditionally called "car").
///
/// CAR stands for "Contents of the Address part of Register".
car: Val,

/// The second element of the cons (traditionally called "cdr").
///
/// CDR stands for "Contents of the Decrement part of Register".
cdr: Val,

/// Create a new pair with `first` and `second`.
pub fn init(first: Val, second: Val) Pair {
    return Pair{ .car = first, .cdr = second };
}
