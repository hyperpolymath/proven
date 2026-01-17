// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe angle operations with degree/radian conversions.
//!
//! Provides angle types with automatic normalization and
//! safe trigonometric operations.

use std::f64::consts::PI;

/// Angle in degrees with normalization.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Degrees(f64);

impl Degrees {
    /// Create a new angle, normalizing to [0, 360).
    pub fn new(value: f64) -> Self {
        Self(normalize_degrees(value))
    }

    /// Create without normalization.
    pub fn raw(value: f64) -> Self {
        Self(value)
    }

    /// Get the value.
    pub fn value(&self) -> f64 {
        self.0
    }

    /// Convert to radians.
    pub fn to_radians(&self) -> Radians {
        Radians::raw(self.0 * PI / 180.0)
    }

    /// Normalize to [0, 360).
    pub fn normalize(&self) -> Self {
        Self::new(self.0)
    }

    /// Normalize to [-180, 180).
    pub fn normalize_signed(&self) -> Self {
        let mut v = normalize_degrees(self.0);
        if v >= 180.0 {
            v -= 360.0;
        }
        Self::raw(v)
    }

    /// Add angles.
    pub fn add(&self, other: Degrees) -> Self {
        Self::new(self.0 + other.0)
    }

    /// Subtract angles.
    pub fn sub(&self, other: Degrees) -> Self {
        Self::new(self.0 - other.0)
    }

    /// Sine of angle.
    pub fn sin(&self) -> f64 {
        self.to_radians().sin()
    }

    /// Cosine of angle.
    pub fn cos(&self) -> f64 {
        self.to_radians().cos()
    }

    /// Tangent of angle.
    pub fn tan(&self) -> f64 {
        self.to_radians().tan()
    }
}

/// Angle in radians.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Radians(f64);

impl Radians {
    /// Create a new angle, normalizing to [0, 2π).
    pub fn new(value: f64) -> Self {
        Self(normalize_radians(value))
    }

    /// Create without normalization.
    pub fn raw(value: f64) -> Self {
        Self(value)
    }

    /// Get the value.
    pub fn value(&self) -> f64 {
        self.0
    }

    /// Convert to degrees.
    pub fn to_degrees(&self) -> Degrees {
        Degrees::raw(self.0 * 180.0 / PI)
    }

    /// Normalize to [0, 2π).
    pub fn normalize(&self) -> Self {
        Self::new(self.0)
    }

    /// Add angles.
    pub fn add(&self, other: Radians) -> Self {
        Self::new(self.0 + other.0)
    }

    /// Subtract angles.
    pub fn sub(&self, other: Radians) -> Self {
        Self::new(self.0 - other.0)
    }

    /// Sine of angle.
    pub fn sin(&self) -> f64 {
        self.0.sin()
    }

    /// Cosine of angle.
    pub fn cos(&self) -> f64 {
        self.0.cos()
    }

    /// Tangent of angle.
    pub fn tan(&self) -> f64 {
        self.0.tan()
    }
}

/// Normalize degrees to [0, 360).
pub fn normalize_degrees(deg: f64) -> f64 {
    let mut result = deg % 360.0;
    if result < 0.0 {
        result += 360.0;
    }
    result
}

/// Normalize radians to [0, 2π).
pub fn normalize_radians(rad: f64) -> f64 {
    let two_pi = 2.0 * PI;
    let mut result = rad % two_pi;
    if result < 0.0 {
        result += two_pi;
    }
    result
}

/// Convert degrees to radians.
pub fn deg_to_rad(deg: f64) -> f64 {
    deg * PI / 180.0
}

/// Convert radians to degrees.
pub fn rad_to_deg(rad: f64) -> f64 {
    rad * 180.0 / PI
}

/// Calculate angle difference (shortest path).
pub fn angle_diff_degrees(a: f64, b: f64) -> f64 {
    let diff = normalize_degrees(b - a);
    if diff > 180.0 {
        diff - 360.0
    } else {
        diff
    }
}

/// Linear interpolation between angles.
pub fn lerp_angle_degrees(a: f64, b: f64, t: f64) -> f64 {
    let diff = angle_diff_degrees(a, b);
    normalize_degrees(a + diff * t)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_degrees() {
        assert!((normalize_degrees(450.0) - 90.0).abs() < 0.0001);
        assert!((normalize_degrees(-90.0) - 270.0).abs() < 0.0001);
        assert!((normalize_degrees(0.0) - 0.0).abs() < 0.0001);
    }

    #[test]
    fn test_conversion() {
        let deg = Degrees::new(180.0);
        let rad = deg.to_radians();
        assert!((rad.value() - PI).abs() < 0.0001);

        let back = rad.to_degrees();
        assert!((back.value() - 180.0).abs() < 0.0001);
    }

    #[test]
    fn test_trig() {
        let deg = Degrees::new(90.0);
        assert!((deg.sin() - 1.0).abs() < 0.0001);
        assert!(deg.cos().abs() < 0.0001);
    }

    #[test]
    fn test_angle_diff() {
        assert!((angle_diff_degrees(10.0, 20.0) - 10.0).abs() < 0.0001);
        assert!((angle_diff_degrees(350.0, 10.0) - 20.0).abs() < 0.0001);
        assert!((angle_diff_degrees(10.0, 350.0) - (-20.0)).abs() < 0.0001);
    }
}
