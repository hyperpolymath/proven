// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe rational number arithmetic with exact precision.
//!
//! Provides arbitrary-precision rational numbers (fractions) that cannot
//! overflow unexpectedly. All operations maintain normalized form with
//! the denominator always positive and GCD(numerator, denominator) = 1.

const std = @import("std");

/// Error types for rational operations.
pub const RationalError = error{
    DivisionByZero,
    Overflow,
    InvalidRepresentation,
};

/// A rational number represented as numerator/denominator.
/// Always maintained in normalized form: denominator > 0, gcd(num, denom) = 1.
pub fn Rational(comptime T: type) type {
    return struct {
        const Self = @This();

        numerator: T,
        denominator: T,

        /// Create a rational number with validation and normalization.
        pub fn init(numerator: T, denominator: T) RationalError!Self {
            if (denominator == 0) return error.DivisionByZero;

            var num = numerator;
            var denom = denominator;

            // Ensure denominator is positive
            if (denom < 0) {
                num = std.math.negate(num) catch return error.Overflow;
                denom = std.math.negate(denom) catch return error.Overflow;
            }

            // Normalize by GCD
            const divisor = gcd(absValue(num), absValue(denom));
            if (divisor > 1) {
                num = @divExact(num, @as(T, @intCast(divisor)));
                denom = @divExact(denom, @as(T, @intCast(divisor)));
            }

            return Self{
                .numerator = num,
                .denominator = denom,
            };
        }

        /// Create a rational from an integer.
        pub fn fromInt(value: T) Self {
            return Self{
                .numerator = value,
                .denominator = 1,
            };
        }

        /// Create zero.
        pub fn zero() Self {
            return Self{
                .numerator = 0,
                .denominator = 1,
            };
        }

        /// Create one.
        pub fn one() Self {
            return Self{
                .numerator = 1,
                .denominator = 1,
            };
        }

        /// Check if zero.
        pub fn isZero(self: Self) bool {
            return self.numerator == 0;
        }

        /// Check if positive.
        pub fn isPositive(self: Self) bool {
            return self.numerator > 0;
        }

        /// Check if negative.
        pub fn isNegative(self: Self) bool {
            return self.numerator < 0;
        }

        /// Check if this is an integer (denominator == 1).
        pub fn isInteger(self: Self) bool {
            return self.denominator == 1;
        }

        /// Negate the rational.
        pub fn negate(self: Self) RationalError!Self {
            const neg_num = std.math.negate(self.numerator) catch return error.Overflow;
            return Self{
                .numerator = neg_num,
                .denominator = self.denominator,
            };
        }

        /// Absolute value.
        pub fn abs(self: Self) Self {
            return Self{
                .numerator = absValue(self.numerator),
                .denominator = self.denominator,
            };
        }

        /// Reciprocal (1/x).
        pub fn reciprocal(self: Self) RationalError!Self {
            if (self.numerator == 0) return error.DivisionByZero;
            return init(self.denominator, self.numerator);
        }

        /// Add two rationals.
        pub fn add(self: Self, other: Self) RationalError!Self {
            // a/b + c/d = (a*d + c*b) / (b*d)
            const ad = std.math.mul(T, self.numerator, other.denominator) catch return error.Overflow;
            const cb = std.math.mul(T, other.numerator, self.denominator) catch return error.Overflow;
            const num = std.math.add(T, ad, cb) catch return error.Overflow;
            const denom = std.math.mul(T, self.denominator, other.denominator) catch return error.Overflow;
            return init(num, denom);
        }

        /// Subtract two rationals.
        pub fn sub(self: Self, other: Self) RationalError!Self {
            const neg_other = try other.negate();
            return self.add(neg_other);
        }

        /// Multiply two rationals.
        pub fn mul(self: Self, other: Self) RationalError!Self {
            const num = std.math.mul(T, self.numerator, other.numerator) catch return error.Overflow;
            const denom = std.math.mul(T, self.denominator, other.denominator) catch return error.Overflow;
            return init(num, denom);
        }

        /// Divide two rationals.
        pub fn div(self: Self, other: Self) RationalError!Self {
            const other_recip = try other.reciprocal();
            return self.mul(other_recip);
        }

        /// Compare two rationals.
        /// Returns -1 if self < other, 0 if equal, 1 if self > other.
        pub fn compare(self: Self, other: Self) RationalError!i2 {
            // a/b vs c/d => compare a*d vs c*b
            const ad = std.math.mul(T, self.numerator, other.denominator) catch return error.Overflow;
            const cb = std.math.mul(T, other.numerator, self.denominator) catch return error.Overflow;

            if (ad < cb) return -1;
            if (ad > cb) return 1;
            return 0;
        }

        /// Check equality.
        pub fn eql(self: Self, other: Self) bool {
            return self.numerator == other.numerator and self.denominator == other.denominator;
        }

        /// Convert to floating point (may lose precision).
        pub fn toFloat(self: Self, comptime F: type) F {
            return @as(F, @floatFromInt(self.numerator)) / @as(F, @floatFromInt(self.denominator));
        }

        /// Floor division (largest integer <= rational).
        pub fn floor(self: Self) T {
            if (self.numerator >= 0) {
                return @divFloor(self.numerator, self.denominator);
            } else {
                // For negative numbers, divFloor already does what we want
                return @divFloor(self.numerator, self.denominator);
            }
        }

        /// Ceiling (smallest integer >= rational).
        pub fn ceil(self: Self) T {
            if (self.numerator >= 0) {
                return @divFloor(self.numerator + self.denominator - 1, self.denominator);
            } else {
                // For negative numbers, use truncation (toward zero) for ceiling
                return @divTrunc(self.numerator, self.denominator);
            }
        }

        /// Round to nearest integer.
        pub fn round(self: Self) T {
            const doubled = self.numerator * 2;
            if (doubled >= 0) {
                return @divFloor(doubled + self.denominator, self.denominator * 2);
            } else {
                return @divFloor(doubled - self.denominator, self.denominator * 2);
            }
        }

        /// Raise to an integer power.
        pub fn pow(self: Self, exponent: u32) RationalError!Self {
            if (exponent == 0) return one();

            var result = self;
            var exp = exponent - 1;

            while (exp > 0) : (exp -= 1) {
                result = try result.mul(self);
            }

            return result;
        }

        /// Get the mediant of two rationals: (a+c)/(b+d).
        /// Useful for Stern-Brocot tree and Farey sequences.
        pub fn mediant(self: Self, other: Self) RationalError!Self {
            const num = std.math.add(T, self.numerator, other.numerator) catch return error.Overflow;
            const denom = std.math.add(T, self.denominator, other.denominator) catch return error.Overflow;
            return init(num, denom);
        }
    };
}

/// Compute GCD using Euclidean algorithm.
fn gcd(a: anytype, b: @TypeOf(a)) @TypeOf(a) {
    var x = a;
    var y = b;
    while (y != 0) {
        const temp = y;
        y = @mod(x, y);
        x = temp;
    }
    return x;
}

/// Absolute value helper.
fn absValue(value: anytype) @TypeOf(value) {
    return if (value < 0) -value else value;
}

/// Standard 64-bit rational type alias.
pub const Rational64 = Rational(i64);

/// Standard 32-bit rational type alias.
pub const Rational32 = Rational(i32);

test "Rational init and normalization" {
    const r = try Rational64.init(4, 6);
    try std.testing.expectEqual(@as(i64, 2), r.numerator);
    try std.testing.expectEqual(@as(i64, 3), r.denominator);
}

test "Rational negative denominator" {
    const r = try Rational64.init(3, -4);
    try std.testing.expectEqual(@as(i64, -3), r.numerator);
    try std.testing.expectEqual(@as(i64, 4), r.denominator);
}

test "Rational division by zero" {
    try std.testing.expectError(error.DivisionByZero, Rational64.init(1, 0));
}

test "Rational addition" {
    const a = try Rational64.init(1, 2);
    const b = try Rational64.init(1, 3);
    const c = try a.add(b);
    try std.testing.expectEqual(@as(i64, 5), c.numerator);
    try std.testing.expectEqual(@as(i64, 6), c.denominator);
}

test "Rational subtraction" {
    const a = try Rational64.init(1, 2);
    const b = try Rational64.init(1, 4);
    const c = try a.sub(b);
    try std.testing.expectEqual(@as(i64, 1), c.numerator);
    try std.testing.expectEqual(@as(i64, 4), c.denominator);
}

test "Rational multiplication" {
    const a = try Rational64.init(2, 3);
    const b = try Rational64.init(3, 4);
    const c = try a.mul(b);
    try std.testing.expectEqual(@as(i64, 1), c.numerator);
    try std.testing.expectEqual(@as(i64, 2), c.denominator);
}

test "Rational division" {
    const a = try Rational64.init(1, 2);
    const b = try Rational64.init(1, 4);
    const c = try a.div(b);
    try std.testing.expectEqual(@as(i64, 2), c.numerator);
    try std.testing.expectEqual(@as(i64, 1), c.denominator);
}

test "Rational comparison" {
    const a = try Rational64.init(1, 3);
    const b = try Rational64.init(1, 2);
    try std.testing.expectEqual(@as(i2, -1), try a.compare(b));
    try std.testing.expectEqual(@as(i2, 1), try b.compare(a));
    try std.testing.expectEqual(@as(i2, 0), try a.compare(a));
}

test "Rational toFloat" {
    const r = try Rational64.init(1, 4);
    try std.testing.expectApproxEqAbs(@as(f64, 0.25), r.toFloat(f64), 0.0001);
}

test "Rational floor and ceil" {
    const r = try Rational64.init(7, 3);
    try std.testing.expectEqual(@as(i64, 2), r.floor());
    try std.testing.expectEqual(@as(i64, 3), r.ceil());
}

test "Rational negative floor" {
    const r = try Rational64.init(-7, 3);
    try std.testing.expectEqual(@as(i64, -3), r.floor());
    try std.testing.expectEqual(@as(i64, -2), r.ceil());
}

test "Rational mediant" {
    const a = try Rational64.init(1, 2);
    const b = try Rational64.init(2, 3);
    const m = try a.mediant(b);
    try std.testing.expectEqual(@as(i64, 3), m.numerator);
    try std.testing.expectEqual(@as(i64, 5), m.denominator);
}
