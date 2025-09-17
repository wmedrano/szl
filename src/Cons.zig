//! Cons cell implementation for the Scheme interpreter.
//!
//! This module provides the fundamental cons cell data structure used to build
//! lists and pairs in Scheme. A cons cell contains two values: car (first) and
//! cdr (rest), which can be any valid Scheme value.

const Val = @import("Val.zig");

const Cons = @This();

/// The first element of the cons cell (traditionally called "car").
/// CAR stands for "Contents of the Address part of Register".
car: Val,

/// The second element of the cons cell (traditionally called "cdr").
/// CDR stands for "Contents of the Decrement part of Register".
cdr: Val,