// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe complex number arithmetic with overflow protection and NaN handling.
//!
//! Provides complex number operations that cannot crash and return explicit
//! error types for overflow, NaN, and infinity conditions. All operations
//! validate inputs and outputs to ensure numerical stability.

const std = @import("std");

/// Error types for complex number operations.
pub const ComplexError = error{
    Overflow,
    DivisionByZero,
    NaN,
    Infinity,
    InvalidInput,
};

/// Complex number with 64-bit floating point components.
pub const Complex64 = struct {
    real: f64,
    imag: f64,

    /// Create a complex number from real and imaginary parts.
    pub fn init(real: f64, imag: f64) ComplexError!Complex64 {
        if (std.math.isNan(real) or std.math.isNan(imag)) {
            return error.NaN;
        }
        if (std.math.isInf(real) or std.math.isInf(imag)) {
            return error.Infinity;
        }
        return Complex64{ .real = real, .imag = imag };
    }

    /// Create a purely real complex number.
    pub fn fromReal(real: f64) ComplexError!Complex64 {
        return init(real, 0.0);
    }

    /// Create a purely imaginary complex number.
    pub fn fromImag(imag: f64) ComplexError!Complex64 {
        return init(0.0, imag);
    }

    /// Create from polar coordinates (r, theta).
    pub fn fromPolar(r: f64, theta: f64) ComplexError!Complex64 {
        if (std.math.isNan(r) or std.math.isNan(theta)) {
            return error.NaN;
        }
        if (r < 0) return error.InvalidInput;
        return init(r * @cos(theta), r * @sin(theta));
    }

    /// The zero complex number.
    pub const zero = Complex64{ .real = 0.0, .imag = 0.0 };

    /// The unit imaginary number i.
    pub const i = Complex64{ .real = 0.0, .imag = 1.0 };

    /// The real number one.
    pub const one = Complex64{ .real = 1.0, .imag = 0.0 };

    /// Check if this complex number is valid (not NaN or infinite).
    pub fn isValid(self: Complex64) bool {
        return !std.math.isNan(self.real) and !std.math.isNan(self.imag) and
            !std.math.isInf(self.real) and !std.math.isInf(self.imag);
    }

    /// Check if this complex number is zero.
    pub fn isZero(self: Complex64) bool {
        return self.real == 0.0 and self.imag == 0.0;
    }

    /// Check if this complex number is purely real.
    pub fn isPurelyReal(self: Complex64) bool {
        return self.imag == 0.0;
    }

    /// Check if this complex number is purely imaginary.
    pub fn isPurelyImaginary(self: Complex64) bool {
        return self.real == 0.0 and self.imag != 0.0;
    }

    /// Compute the complex conjugate.
    pub fn conjugate(self: Complex64) Complex64 {
        return Complex64{ .real = self.real, .imag = -self.imag };
    }

    /// Compute the negation.
    pub fn negate(self: Complex64) Complex64 {
        return Complex64{ .real = -self.real, .imag = -self.imag };
    }
};

/// Validate that a complex number is finite.
fn validateFinite(z: Complex64) ComplexError!void {
    if (std.math.isNan(z.real) or std.math.isNan(z.imag)) {
        return error.NaN;
    }
    if (std.math.isInf(z.real) or std.math.isInf(z.imag)) {
        return error.Infinity;
    }
}

/// Safely add two complex numbers.
pub fn safeAdd(a: Complex64, b: Complex64) ComplexError!Complex64 {
    const real = a.real + b.real;
    const imag = a.imag + b.imag;
    const result = Complex64{ .real = real, .imag = imag };
    try validateFinite(result);
    return result;
}

/// Safely subtract two complex numbers.
pub fn safeSub(a: Complex64, b: Complex64) ComplexError!Complex64 {
    const real = a.real - b.real;
    const imag = a.imag - b.imag;
    const result = Complex64{ .real = real, .imag = imag };
    try validateFinite(result);
    return result;
}

/// Safely multiply two complex numbers.
/// (a + bi)(c + di) = (ac - bd) + (ad + bc)i
pub fn safeMul(a: Complex64, b: Complex64) ComplexError!Complex64 {
    const real = a.real * b.real - a.imag * b.imag;
    const imag = a.real * b.imag + a.imag * b.real;
    const result = Complex64{ .real = real, .imag = imag };
    try validateFinite(result);
    return result;
}

/// Safely divide two complex numbers.
/// (a + bi) / (c + di) = ((ac + bd) + (bc - ad)i) / (c^2 + d^2)
pub fn safeDiv(a: Complex64, b: Complex64) ComplexError!Complex64 {
    const denom = b.real * b.real + b.imag * b.imag;
    if (denom == 0.0) return error.DivisionByZero;

    const real = (a.real * b.real + a.imag * b.imag) / denom;
    const imag = (a.imag * b.real - a.real * b.imag) / denom;
    const result = Complex64{ .real = real, .imag = imag };
    try validateFinite(result);
    return result;
}

/// Compute the magnitude (absolute value) of a complex number.
pub fn magnitude(z: Complex64) ComplexError!f64 {
    const result = @sqrt(z.real * z.real + z.imag * z.imag);
    if (std.math.isNan(result)) return error.NaN;
    if (std.math.isInf(result)) return error.Overflow;
    return result;
}

/// Compute the squared magnitude (avoids sqrt for comparisons).
pub fn magnitudeSquared(z: Complex64) ComplexError!f64 {
    const result = z.real * z.real + z.imag * z.imag;
    if (std.math.isNan(result)) return error.NaN;
    if (std.math.isInf(result)) return error.Overflow;
    return result;
}

/// Compute the phase angle (argument) in radians.
pub fn phase(z: Complex64) ComplexError!f64 {
    const result = std.math.atan2(z.imag, z.real);
    if (std.math.isNan(result)) return error.NaN;
    return result;
}

/// Safely scale a complex number by a real scalar.
pub fn safeScale(z: Complex64, scalar: f64) ComplexError!Complex64 {
    if (std.math.isNan(scalar)) return error.NaN;
    if (std.math.isInf(scalar)) return error.Infinity;

    const result = Complex64{
        .real = z.real * scalar,
        .imag = z.imag * scalar,
    };
    try validateFinite(result);
    return result;
}

/// Compute the reciprocal (1/z).
pub fn reciprocal(z: Complex64) ComplexError!Complex64 {
    return safeDiv(Complex64.one, z);
}

/// Compute the square of a complex number.
pub fn square(z: Complex64) ComplexError!Complex64 {
    return safeMul(z, z);
}

/// Safely compute the square root of a complex number.
pub fn safeSqrt(z: Complex64) ComplexError!Complex64 {
    const r = try magnitude(z);
    const theta = try phase(z);
    return Complex64.fromPolar(@sqrt(r), theta / 2.0);
}

/// Check approximate equality within epsilon.
pub fn approxEqual(a: Complex64, b: Complex64, epsilon: f64) bool {
    return @abs(a.real - b.real) <= epsilon and
        @abs(a.imag - b.imag) <= epsilon;
}

/// Compute the distance between two complex numbers.
pub fn distance(a: Complex64, b: Complex64) ComplexError!f64 {
    const diff = try safeSub(a, b);
    return magnitude(diff);
}

/// Safe exponentiation by an integer power.
pub fn safePowInt(z: Complex64, n: i32) ComplexError!Complex64 {
    if (n == 0) return Complex64.one;
    if (n < 0) {
        const rec = try reciprocal(z);
        return safePowInt(rec, -n);
    }

    var result = Complex64.one;
    var base = z;
    var exp: u32 = @intCast(n);

    while (exp > 0) {
        if (exp & 1 == 1) {
            result = try safeMul(result, base);
        }
        base = try safeMul(base, base);
        exp >>= 1;
    }

    return result;
}

test "Complex64.init" {
    const z = try Complex64.init(3.0, 4.0);
    try std.testing.expectApproxEqAbs(@as(f64, 3.0), z.real, 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 4.0), z.imag, 0.001);

    try std.testing.expectError(error.NaN, Complex64.init(std.math.nan(f64), 1.0));
    try std.testing.expectError(error.Infinity, Complex64.init(std.math.inf(f64), 1.0));
}

test "Complex64.fromPolar" {
    const z = try Complex64.fromPolar(1.0, std.math.pi / 2.0);
    try std.testing.expectApproxEqAbs(@as(f64, 0.0), z.real, 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 1.0), z.imag, 0.001);
}

test "safeAdd" {
    const a = try Complex64.init(1.0, 2.0);
    const b = try Complex64.init(3.0, 4.0);
    const result = try safeAdd(a, b);
    try std.testing.expectApproxEqAbs(@as(f64, 4.0), result.real, 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 6.0), result.imag, 0.001);
}

test "safeSub" {
    const a = try Complex64.init(5.0, 7.0);
    const b = try Complex64.init(2.0, 3.0);
    const result = try safeSub(a, b);
    try std.testing.expectApproxEqAbs(@as(f64, 3.0), result.real, 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 4.0), result.imag, 0.001);
}

test "safeMul" {
    const a = try Complex64.init(1.0, 2.0);
    const b = try Complex64.init(3.0, 4.0);
    // (1 + 2i)(3 + 4i) = 3 + 4i + 6i + 8i^2 = 3 + 10i - 8 = -5 + 10i
    const result = try safeMul(a, b);
    try std.testing.expectApproxEqAbs(@as(f64, -5.0), result.real, 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 10.0), result.imag, 0.001);
}

test "safeDiv" {
    const a = try Complex64.init(1.0, 2.0);
    const b = try Complex64.init(3.0, 4.0);
    const result = try safeDiv(a, b);
    // (1 + 2i) / (3 + 4i) = (1 + 2i)(3 - 4i) / (9 + 16) = (3 - 4i + 6i + 8) / 25 = (11 + 2i) / 25
    try std.testing.expectApproxEqAbs(@as(f64, 0.44), result.real, 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 0.08), result.imag, 0.001);

    try std.testing.expectError(error.DivisionByZero, safeDiv(a, Complex64.zero));
}

test "magnitude" {
    const z = try Complex64.init(3.0, 4.0);
    const mag = try magnitude(z);
    try std.testing.expectApproxEqAbs(@as(f64, 5.0), mag, 0.001);
}

test "phase" {
    const z = try Complex64.init(1.0, 1.0);
    const p = try phase(z);
    try std.testing.expectApproxEqAbs(std.math.pi / 4.0, p, 0.001);
}

test "conjugate" {
    const z = try Complex64.init(3.0, 4.0);
    const conj = z.conjugate();
    try std.testing.expectApproxEqAbs(@as(f64, 3.0), conj.real, 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, -4.0), conj.imag, 0.001);
}

test "safePowInt" {
    const z = try Complex64.init(1.0, 1.0);
    const result = try safePowInt(z, 2);
    // (1 + i)^2 = 1 + 2i + i^2 = 1 + 2i - 1 = 2i
    try std.testing.expectApproxEqAbs(@as(f64, 0.0), result.real, 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 2.0), result.imag, 0.001);
}

test "approxEqual" {
    const a = try Complex64.init(1.0, 2.0);
    const b = try Complex64.init(1.0001, 2.0001);
    try std.testing.expect(approxEqual(a, b, 0.001));
    try std.testing.expect(!approxEqual(a, b, 0.00001));
}

test "isZero and isPurelyReal" {
    try std.testing.expect(Complex64.zero.isZero());
    try std.testing.expect(Complex64.one.isPurelyReal());
    try std.testing.expect(Complex64.i.isPurelyImaginary());
}
