// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe angle operations with unit conversions and normalization.

const std = @import("std");

pub const AngleUnit = enum {
    degrees,
    radians,
    gradians,
    turns,
};

/// Angle with unit tracking.
pub const Angle = struct {
    value: f64,
    unit: AngleUnit,

    pub fn degrees(value: f64) Angle {
        return .{ .value = value, .unit = .degrees };
    }

    pub fn radians(value: f64) Angle {
        return .{ .value = value, .unit = .radians };
    }

    pub fn gradians(value: f64) Angle {
        return .{ .value = value, .unit = .gradians };
    }

    pub fn turns(value: f64) Angle {
        return .{ .value = value, .unit = .turns };
    }

    /// Convert to degrees.
    pub fn toDegrees(self: Angle) f64 {
        return switch (self.unit) {
            .degrees => self.value,
            .radians => self.value * 180.0 / std.math.pi,
            .gradians => self.value * 0.9,
            .turns => self.value * 360.0,
        };
    }

    /// Convert to radians.
    pub fn toRadians(self: Angle) f64 {
        return switch (self.unit) {
            .degrees => self.value * std.math.pi / 180.0,
            .radians => self.value,
            .gradians => self.value * std.math.pi / 200.0,
            .turns => self.value * 2.0 * std.math.pi,
        };
    }

    /// Normalize to [0, 360) degrees or equivalent.
    pub fn normalize(self: Angle) Angle {
        const deg = @mod(self.toDegrees(), 360.0);
        return switch (self.unit) {
            .degrees => Angle.degrees(deg),
            .radians => Angle.radians(deg * std.math.pi / 180.0),
            .gradians => Angle.gradians(deg / 0.9),
            .turns => Angle.turns(deg / 360.0),
        };
    }

    /// Normalize to [-180, 180) degrees or equivalent.
    pub fn normalizeSigned(self: Angle) Angle {
        var deg = @mod(self.toDegrees(), 360.0);
        if (deg >= 180.0) deg -= 360.0;
        return switch (self.unit) {
            .degrees => Angle.degrees(deg),
            .radians => Angle.radians(deg * std.math.pi / 180.0),
            .gradians => Angle.gradians(deg / 0.9),
            .turns => Angle.turns(deg / 360.0),
        };
    }

    /// Sine of angle.
    pub fn sin(self: Angle) f64 {
        return @sin(self.toRadians());
    }

    /// Cosine of angle.
    pub fn cos(self: Angle) f64 {
        return @cos(self.toRadians());
    }

    /// Tangent of angle.
    pub fn tan(self: Angle) f64 {
        return @tan(self.toRadians());
    }

    /// Add two angles.
    pub fn add(self: Angle, other: Angle) Angle {
        const sum_deg = self.toDegrees() + other.toDegrees();
        return switch (self.unit) {
            .degrees => Angle.degrees(sum_deg),
            .radians => Angle.radians(sum_deg * std.math.pi / 180.0),
            .gradians => Angle.gradians(sum_deg / 0.9),
            .turns => Angle.turns(sum_deg / 360.0),
        };
    }
};

/// Create angle from arctangent (result in radians).
pub fn atan2(y: f64, x: f64) Angle {
    return Angle.radians(std.math.atan2(y, x));
}

/// Create angle from arcsine (result in radians).
pub fn asin(value: f64) ?Angle {
    if (value < -1.0 or value > 1.0) return null;
    return Angle.radians(std.math.asin(value));
}

/// Create angle from arccosine (result in radians).
pub fn acos(value: f64) ?Angle {
    if (value < -1.0 or value > 1.0) return null;
    return Angle.radians(std.math.acos(value));
}

test "Angle conversions" {
    const a = Angle.degrees(180.0);
    try std.testing.expectApproxEqAbs(std.math.pi, a.toRadians(), 0.0001);
}

test "Angle normalize" {
    const a = Angle.degrees(450.0).normalize();
    try std.testing.expectApproxEqAbs(90.0, a.toDegrees(), 0.0001);
}
