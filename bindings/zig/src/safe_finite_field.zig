// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe finite field arithmetic (modular arithmetic) operations.
//!
//! Provides safe operations over finite fields (Z/pZ) with:
//! - Overflow-safe modular arithmetic
//! - Modular exponentiation
//! - Modular multiplicative inverse (extended Euclidean algorithm)
//! - Prime field operations for cryptographic applications

const std = @import("std");

/// Error types for finite field operations.
pub const FiniteFieldError = error{
    InvalidModulus,
    DivisionByZero,
    NoInverse,
    Overflow,
    NotPrime,
};

/// A finite field element with compile-time modulus.
pub fn FieldElement(comptime T: type, comptime modulus: T) type {
    if (modulus <= 1) @compileError("Modulus must be greater than 1");

    return struct {
        const Self = @This();

        value: T,

        /// Create a new field element, reducing the value modulo the modulus.
        pub fn init(value: T) Self {
            return .{ .value = @mod(value, modulus) };
        }

        /// Create from a potentially larger unsigned value.
        pub fn fromUnsigned(comptime U: type, value: U) Self {
            const reduced = @mod(value, @as(U, modulus));
            return .{ .value = @intCast(reduced) };
        }

        /// Get the raw value.
        pub fn toInt(self: Self) T {
            return self.value;
        }

        /// Add two field elements.
        pub fn add(self: Self, other: Self) Self {
            // Use wider type to prevent overflow during addition
            const WideT = std.meta.Int(.unsigned, @bitSizeOf(T) * 2);
            const sum: WideT = @as(WideT, self.value) + @as(WideT, other.value);
            return .{ .value = @intCast(@mod(sum, modulus)) };
        }

        /// Subtract two field elements.
        pub fn sub(self: Self, other: Self) Self {
            if (self.value >= other.value) {
                return .{ .value = self.value - other.value };
            } else {
                // Handle underflow by adding modulus
                return .{ .value = modulus - (other.value - self.value) };
            }
        }

        /// Multiply two field elements.
        pub fn mul(self: Self, other: Self) Self {
            // Use wider type to prevent overflow during multiplication
            const WideT = std.meta.Int(.unsigned, @bitSizeOf(T) * 2);
            const product: WideT = @as(WideT, self.value) * @as(WideT, other.value);
            return .{ .value = @intCast(@mod(product, modulus)) };
        }

        /// Compute modular exponentiation using square-and-multiply.
        pub fn pow(self: Self, exponent: T) Self {
            if (exponent == 0) return .{ .value = 1 };

            var result = Self{ .value = 1 };
            var base = self;
            var exp = exponent;

            while (exp > 0) {
                if (exp & 1 == 1) {
                    result = result.mul(base);
                }
                base = base.mul(base);
                exp >>= 1;
            }

            return result;
        }

        /// Compute multiplicative inverse using extended Euclidean algorithm.
        /// Returns error if no inverse exists (value and modulus are not coprime).
        pub fn inverse(self: Self) FiniteFieldError!Self {
            if (self.value == 0) return error.DivisionByZero;

            const result = extendedGcd(T, self.value, modulus);
            if (result.gcd != 1) return error.NoInverse;

            // Handle negative coefficient
            const inv = if (result.x < 0)
                @as(T, @intCast(@mod(result.x, @as(i128, modulus))))
            else
                @as(T, @intCast(result.x));

            return .{ .value = @mod(inv, modulus) };
        }

        /// Divide two field elements (multiply by inverse).
        pub fn div(self: Self, other: Self) FiniteFieldError!Self {
            const other_inv = try other.inverse();
            return self.mul(other_inv);
        }

        /// Negate a field element.
        pub fn negate(self: Self) Self {
            if (self.value == 0) return self;
            return .{ .value = modulus - self.value };
        }

        /// Check equality.
        pub fn eql(self: Self, other: Self) bool {
            return self.value == other.value;
        }

        /// Check if element is zero.
        pub fn isZero(self: Self) bool {
            return self.value == 0;
        }

        /// Check if element is one.
        pub fn isOne(self: Self) bool {
            return self.value == 1;
        }

        /// Get the additive identity (zero).
        pub fn zero() Self {
            return .{ .value = 0 };
        }

        /// Get the multiplicative identity (one).
        pub fn one() Self {
            return .{ .value = 1 };
        }

        /// Get the modulus.
        pub fn getModulus() T {
            return modulus;
        }
    };
}

/// Result of extended GCD computation.
pub fn ExtendedGcdResult(comptime T: type) type {
    return struct {
        gcd: T,
        x: i128,
        y: i128,
    };
}

/// Extended Euclidean algorithm to compute GCD and Bezout coefficients.
/// Returns gcd(a, b) and coefficients x, y such that ax + by = gcd(a, b).
pub fn extendedGcd(comptime T: type, a: T, b: T) ExtendedGcdResult(T) {
    var old_r: i128 = @intCast(a);
    var r: i128 = @intCast(b);
    var old_s: i128 = 1;
    var s: i128 = 0;
    var old_t: i128 = 0;
    var t: i128 = 1;

    while (r != 0) {
        const quotient = @divTrunc(old_r, r);

        const temp_r = r;
        r = old_r - quotient * r;
        old_r = temp_r;

        const temp_s = s;
        s = old_s - quotient * s;
        old_s = temp_s;

        const temp_t = t;
        t = old_t - quotient * t;
        old_t = temp_t;
    }

    return .{
        .gcd = @intCast(if (old_r < 0) -old_r else old_r),
        .x = old_s,
        .y = old_t,
    };
}

/// Compute GCD of two numbers.
pub fn gcd(comptime T: type, a: T, b: T) T {
    var x = a;
    var y = b;
    while (y != 0) {
        const temp = y;
        y = @mod(x, y);
        x = temp;
    }
    return x;
}

/// Compute LCM of two numbers, returning error on overflow.
pub fn lcm(comptime T: type, a: T, b: T) FiniteFieldError!T {
    if (a == 0 or b == 0) return 0;
    const g = gcd(T, a, b);
    const div_result = @divExact(a, g);
    return std.math.mul(T, div_result, b) catch error.Overflow;
}

/// Check if a number is coprime to another (GCD is 1).
pub fn coprime(comptime T: type, a: T, b: T) bool {
    return gcd(T, a, b) == 1;
}

/// Modular addition without overflow.
pub fn modAdd(comptime T: type, a: T, b: T, modulus: T) FiniteFieldError!T {
    if (modulus == 0) return error.InvalidModulus;
    const WideT = std.meta.Int(.unsigned, @bitSizeOf(T) * 2);
    const sum: WideT = @as(WideT, @mod(a, modulus)) + @as(WideT, @mod(b, modulus));
    return @intCast(@mod(sum, modulus));
}

/// Modular subtraction without underflow.
pub fn modSub(comptime T: type, a: T, b: T, modulus: T) FiniteFieldError!T {
    if (modulus == 0) return error.InvalidModulus;
    const a_mod = @mod(a, modulus);
    const b_mod = @mod(b, modulus);
    if (a_mod >= b_mod) {
        return a_mod - b_mod;
    } else {
        return modulus - (b_mod - a_mod);
    }
}

/// Modular multiplication without overflow.
pub fn modMul(comptime T: type, a: T, b: T, modulus: T) FiniteFieldError!T {
    if (modulus == 0) return error.InvalidModulus;
    const WideT = std.meta.Int(.unsigned, @bitSizeOf(T) * 2);
    const product: WideT = @as(WideT, @mod(a, modulus)) * @as(WideT, @mod(b, modulus));
    return @intCast(@mod(product, modulus));
}

/// Modular exponentiation using square-and-multiply.
pub fn modPow(comptime T: type, base: T, exponent: T, modulus: T) FiniteFieldError!T {
    if (modulus == 0) return error.InvalidModulus;
    if (modulus == 1) return 0;
    if (exponent == 0) return 1;

    const WideT = std.meta.Int(.unsigned, @bitSizeOf(T) * 2);
    var result: WideT = 1;
    var b: WideT = @mod(@as(WideT, base), modulus);
    var exp = exponent;

    while (exp > 0) {
        if (exp & 1 == 1) {
            result = @mod(result * b, modulus);
        }
        b = @mod(b * b, modulus);
        exp >>= 1;
    }

    return @intCast(result);
}

/// Compute modular multiplicative inverse using extended Euclidean algorithm.
pub fn modInverse(comptime T: type, a: T, modulus: T) FiniteFieldError!T {
    if (modulus == 0) return error.InvalidModulus;
    if (a == 0) return error.DivisionByZero;

    const result = extendedGcd(T, @mod(a, modulus), modulus);
    if (result.gcd != 1) return error.NoInverse;

    // Handle negative coefficient
    const inv = @mod(result.x, @as(i128, modulus));
    return @intCast(if (inv < 0) inv + modulus else inv);
}

/// Modular division (multiply by inverse).
pub fn modDiv(comptime T: type, a: T, b: T, modulus: T) FiniteFieldError!T {
    const b_inv = try modInverse(T, b, modulus);
    return modMul(T, a, b_inv, modulus);
}

// Common prime field definitions
pub const F17 = FieldElement(u32, 17);
pub const F31 = FieldElement(u32, 31);
pub const F101 = FieldElement(u32, 101);
pub const F257 = FieldElement(u32, 257);
pub const F65537 = FieldElement(u32, 65537);

test "FieldElement basic operations" {
    const F7 = FieldElement(u32, 7);

    const a = F7.init(3);
    const b = F7.init(5);

    try std.testing.expectEqual(@as(u32, 3), a.toInt());
    try std.testing.expectEqual(@as(u32, 5), b.toInt());

    // Addition: (3 + 5) mod 7 = 1
    try std.testing.expectEqual(@as(u32, 1), a.add(b).toInt());

    // Subtraction: (3 - 5) mod 7 = 5
    try std.testing.expectEqual(@as(u32, 5), a.sub(b).toInt());

    // Multiplication: (3 * 5) mod 7 = 1
    try std.testing.expectEqual(@as(u32, 1), a.mul(b).toInt());
}

test "FieldElement reduction" {
    const F7 = FieldElement(u32, 7);

    // 10 mod 7 = 3
    try std.testing.expectEqual(@as(u32, 3), F7.init(10).toInt());

    // 0 mod 7 = 0
    try std.testing.expectEqual(@as(u32, 0), F7.init(0).toInt());

    // 7 mod 7 = 0
    try std.testing.expectEqual(@as(u32, 0), F7.init(7).toInt());
}

test "FieldElement exponentiation" {
    const F7 = FieldElement(u32, 7);

    // 3^0 mod 7 = 1
    try std.testing.expectEqual(@as(u32, 1), F7.init(3).pow(0).toInt());

    // 3^1 mod 7 = 3
    try std.testing.expectEqual(@as(u32, 3), F7.init(3).pow(1).toInt());

    // 3^2 mod 7 = 2
    try std.testing.expectEqual(@as(u32, 2), F7.init(3).pow(2).toInt());

    // 3^6 mod 7 = 1 (Fermat's little theorem)
    try std.testing.expectEqual(@as(u32, 1), F7.init(3).pow(6).toInt());
}

test "FieldElement inverse" {
    const F7 = FieldElement(u32, 7);

    // 3 * 5 = 15 = 1 (mod 7), so 3^(-1) = 5
    const inv = try F7.init(3).inverse();
    try std.testing.expectEqual(@as(u32, 5), inv.toInt());

    // Verify: 3 * 5 = 1 (mod 7)
    try std.testing.expectEqual(@as(u32, 1), F7.init(3).mul(inv).toInt());

    // 0 has no inverse
    try std.testing.expectError(error.DivisionByZero, F7.init(0).inverse());
}

test "gcd and extendedGcd" {
    try std.testing.expectEqual(@as(u32, 6), gcd(u32, 12, 18));
    try std.testing.expectEqual(@as(u32, 1), gcd(u32, 17, 31));
    try std.testing.expectEqual(@as(u32, 5), gcd(u32, 0, 5));

    const result = extendedGcd(u32, 12, 18);
    try std.testing.expectEqual(@as(u32, 6), result.gcd);
    // Verify Bezout's identity: 12*x + 18*y = 6
    try std.testing.expectEqual(@as(i128, 6), 12 * result.x + 18 * result.y);
}

test "modular operations" {
    // modAdd
    try std.testing.expectEqual(@as(u32, 3), try modAdd(u32, 10, 7, 7));

    // modSub
    try std.testing.expectEqual(@as(u32, 5), try modSub(u32, 3, 5, 7));

    // modMul
    try std.testing.expectEqual(@as(u32, 1), try modMul(u32, 3, 5, 7));

    // modPow
    try std.testing.expectEqual(@as(u32, 1), try modPow(u32, 3, 6, 7));

    // modInverse
    try std.testing.expectEqual(@as(u32, 5), try modInverse(u32, 3, 7));
}

test "coprime" {
    try std.testing.expect(coprime(u32, 7, 11));
    try std.testing.expect(coprime(u32, 1, 100));
    try std.testing.expect(!coprime(u32, 6, 9));
    try std.testing.expect(!coprime(u32, 12, 18));
}

test "lcm" {
    try std.testing.expectEqual(@as(u32, 12), try lcm(u32, 4, 6));
    try std.testing.expectEqual(@as(u32, 35), try lcm(u32, 5, 7));
    try std.testing.expectEqual(@as(u32, 0), try lcm(u32, 0, 5));
}
