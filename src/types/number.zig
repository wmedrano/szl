const std = @import("std");

/// Compute greatest common divisor using Euclidean algorithm
fn gcd(a: u64, b: u64) u64 {
    if (b == 0) return a;
    return gcd(b, a % b);
}

pub const Rational = struct {
    numerator: i32,
    denominator: u32,

    pub const Error = error{
        DenominatorZero,
        NumeratorTooLarge,
        DenominatorTooLarge,
    };

    /// Create a rational number in reduced form
    pub fn init(numerator: i32, denominator: u32) Rational {
        if (denominator == 0) {
            @panic("Rational denominator cannot be zero");
        }

        // Reduce to lowest terms
        const g: i64 = @intCast(gcd(@abs(numerator), denominator));
        return Rational{
            .numerator = @intCast(@divExact(numerator, @as(i32, @intCast(g)))),
            .denominator = @intCast(@divExact(denominator, @as(u32, @intCast(g)))),
        };
    }

    /// Create a rational number from i64/u64, automatically reducing to fit in i32/u32
    /// Returns an error if the reduced values still don't fit
    pub fn fromInt64(numerator: i64, denominator: u64) Error!Rational {
        if (denominator == 0) {
            return Error.DenominatorZero;
        }

        // Reduce the fraction by computing GCD
        const abs_numerator: u64 = @intCast(@abs(numerator));
        const g = gcd(abs_numerator, denominator);
        const reduced_numerator = @divExact(numerator, @as(i64, @intCast(g)));
        const reduced_denominator = @divExact(denominator, g);

        // Check if reduced values fit in i32/u32
        if (reduced_numerator < std.math.minInt(i32) or reduced_numerator > std.math.maxInt(i32)) {
            return Error.NumeratorTooLarge;
        }

        if (reduced_denominator > std.math.maxInt(u32)) {
            return Error.DenominatorTooLarge;
        }

        return Rational{
            .numerator = @intCast(reduced_numerator),
            .denominator = @intCast(reduced_denominator),
        };
    }
};

pub const Number = union(enum) {
    int: i64,
    rational: Rational,
    float: f64,

    pub fn asFloat(self: Number) f64 {
        switch (self) {
            .int => |x| return @floatFromInt(x),
            .rational => |r| return @as(f64, @floatFromInt(r.numerator)) / @as(f64, @floatFromInt(r.denominator)),
            .float => |x| return x,
        }
    }
};
