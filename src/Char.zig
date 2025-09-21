//! Character representation for the Scheme interpreter.
//!
//! This module defines the character type used in the Scheme interpreter.
//! Characters are represented as single bytes (u8) and support various
//! character literals including named characters and escape sequences.

const std = @import("std");
const testing = std.testing;

const Char = @This();

/// The character data as a single byte.
data: u8,

/// Create a new character with the given byte value.
///
/// Args:
///   ch: The byte value for the character.
///
/// Returns:
///   A new Char instance containing the given byte.
pub fn init(ch: u8) Char {
    return Char{ .data = ch };
}

test "Char init creates character with correct data" {
    const char_a = Char.init('a');
    try testing.expectEqual(@as(u8, 'a'), char_a.data);

    const char_space = Char.init(' ');
    try testing.expectEqual(@as(u8, ' '), char_space.data);

    const char_null = Char.init(0);
    try testing.expectEqual(@as(u8, 0), char_null.data);
}