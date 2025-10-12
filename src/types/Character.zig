const Character = @This();

/// The Unicode code point for this character.
/// Uses u21 because the maximum Unicode code point is U+10FFFF (1,114,111),
/// which requires 21 bits to represent. This supports the full Unicode range
/// while being more compact than u32.
data: u21,

pub fn init(c: u21) Character {
    return Character{ .data = c };
}
