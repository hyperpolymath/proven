// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafeAngle - Angle operations that cannot crash.
////
//// Provides safe angle representation and trigonometric operations.

import gleam/float
import gleam/int
import gleam/order.{type Order}

/// Mathematical constants.
pub const pi = 3.14159265358979323846

pub const tau = 6.28318530717958647692

/// An angle that can be in any unit.
pub opaque type Angle {
  AngleRadians(radians: Float)
}

/// Create an angle from radians.
pub fn from_radians(radians: Float) -> Angle {
  AngleRadians(radians: radians)
}

/// Create an angle from degrees.
pub fn from_degrees(degrees: Float) -> Angle {
  AngleRadians(radians: degrees *. pi /. 180.0)
}

/// Create an angle from gradians.
pub fn from_gradians(gradians: Float) -> Angle {
  AngleRadians(radians: gradians *. pi /. 200.0)
}

/// Create an angle from turns (0-1 represents 0-360 degrees).
pub fn from_turns(turns: Float) -> Angle {
  AngleRadians(radians: turns *. tau)
}

/// Get the angle in radians.
pub fn to_radians(angle: Angle) -> Float {
  angle.radians
}

/// Get the angle in degrees.
pub fn to_degrees(angle: Angle) -> Float {
  angle.radians *. 180.0 /. pi
}

/// Get the angle in gradians.
pub fn to_gradians(angle: Angle) -> Float {
  angle.radians *. 200.0 /. pi
}

/// Get the angle in turns.
pub fn to_turns(angle: Angle) -> Float {
  angle.radians /. tau
}

/// Normalize an angle to [0, 2*pi).
pub fn normalize(angle: Angle) -> Angle {
  let normalized = modulo_float(angle.radians, tau)
  let positive = case normalized <. 0.0 {
    True -> normalized +. tau
    False -> normalized
  }
  AngleRadians(radians: positive)
}

fn modulo_float(value: Float, divisor: Float) -> Float {
  let quotient = float.truncate(value /. divisor)
  value -. int.to_float(quotient) *. divisor
}

/// Normalize an angle to [-pi, pi).
pub fn normalize_signed(angle: Angle) -> Angle {
  let normalized = normalize(angle)
  case normalized.radians >=. pi {
    True -> AngleRadians(radians: normalized.radians -. tau)
    False -> normalized
  }
}

/// Add two angles.
pub fn add(angle_a: Angle, angle_b: Angle) -> Angle {
  AngleRadians(radians: angle_a.radians +. angle_b.radians)
}

/// Subtract two angles.
pub fn subtract(angle_a: Angle, angle_b: Angle) -> Angle {
  AngleRadians(radians: angle_a.radians -. angle_b.radians)
}

/// Multiply an angle by a scalar.
pub fn multiply(angle: Angle, scalar: Float) -> Angle {
  AngleRadians(radians: angle.radians *. scalar)
}

/// Divide an angle by a scalar.
pub fn divide(angle: Angle, scalar: Float) -> Result(Angle, Nil) {
  case scalar == 0.0 {
    True -> Error(Nil)
    False -> Ok(AngleRadians(radians: angle.radians /. scalar))
  }
}

/// Negate an angle.
pub fn negate(angle: Angle) -> Angle {
  AngleRadians(radians: 0.0 -. angle.radians)
}

/// Get the absolute value of an angle.
pub fn abs(angle: Angle) -> Angle {
  AngleRadians(radians: float.absolute_value(angle.radians))
}

/// Compare two angles.
pub fn compare(angle_a: Angle, angle_b: Angle) -> Order {
  float.compare(angle_a.radians, angle_b.radians)
}

/// Check if two angles are approximately equal.
pub fn approx_equal(angle_a: Angle, angle_b: Angle, epsilon: Float) -> Bool {
  float.absolute_value(angle_a.radians -. angle_b.radians) <=. epsilon
}

/// Sine of an angle.
pub fn sin(angle: Angle) -> Float {
  // Taylor series approximation for sin
  // In production, use platform-native math functions
  let x = normalize_signed(angle).radians
  approximate_sin(x)
}

fn approximate_sin(x: Float) -> Float {
  // Taylor series: sin(x) = x - x^3/3! + x^5/5! - x^7/7! + ...
  let x2 = x *. x
  let x3 = x2 *. x
  let x5 = x3 *. x2
  let x7 = x5 *. x2
  let x9 = x7 *. x2
  x -. x3 /. 6.0 +. x5 /. 120.0 -. x7 /. 5040.0 +. x9 /. 362_880.0
}

/// Cosine of an angle.
pub fn cos(angle: Angle) -> Float {
  // cos(x) = sin(x + pi/2)
  sin(add(angle, from_radians(pi /. 2.0)))
}

/// Tangent of an angle.
pub fn tan(angle: Angle) -> Result(Float, Nil) {
  let cos_val = cos(angle)
  case float.absolute_value(cos_val) <. 1.0e-10 {
    True -> Error(Nil)
    False -> Ok(sin(angle) /. cos_val)
  }
}

/// Arcsine (inverse sine).
pub fn asin(value: Float) -> Result(Angle, Nil) {
  case value <. -1.0 || value >. 1.0 {
    True -> Error(Nil)
    False -> Ok(AngleRadians(radians: approximate_asin(value)))
  }
}

fn approximate_asin(x: Float) -> Float {
  // Approximation for asin using polynomial
  // Valid for -1 <= x <= 1
  case float.absolute_value(x) >. 0.9 {
    True -> {
      // Use identity: asin(x) = pi/2 - asin(sqrt(1-x^2)) for |x| > 0.9
      let sign = case x <. 0.0 {
        True -> -1.0
        False -> 1.0
      }
      let abs_x = float.absolute_value(x)
      let sqrt_val = approximate_sqrt(1.0 -. abs_x *. abs_x)
      sign *. { pi /. 2.0 -. approximate_asin_core(sqrt_val) }
    }
    False -> approximate_asin_core(x)
  }
}

fn approximate_asin_core(x: Float) -> Float {
  // Polynomial approximation for small x
  let x2 = x *. x
  let x3 = x2 *. x
  let x5 = x3 *. x2
  let x7 = x5 *. x2
  x +. x3 /. 6.0 +. 3.0 *. x5 /. 40.0 +. 15.0 *. x7 /. 336.0
}

fn approximate_sqrt(value: Float) -> Float {
  case float.square_root(value) {
    Ok(v) -> v
    Error(_) -> 0.0
  }
}

/// Arccosine (inverse cosine).
pub fn acos(value: Float) -> Result(Angle, Nil) {
  case value <. -1.0 || value >. 1.0 {
    True -> Error(Nil)
    False -> Ok(AngleRadians(radians: pi /. 2.0 -. approximate_asin(value)))
  }
}

/// Arctangent (inverse tangent).
pub fn atan(value: Float) -> Angle {
  AngleRadians(radians: approximate_atan(value))
}

fn approximate_atan(x: Float) -> Float {
  // Use identity for large values: atan(x) = pi/2 - atan(1/x) for x > 1
  case float.absolute_value(x) >. 1.0 {
    True -> {
      let sign = case x <. 0.0 {
        True -> -1.0
        False -> 1.0
      }
      sign *. { pi /. 2.0 -. approximate_atan_core(1.0 /. float.absolute_value(x)) }
    }
    False -> approximate_atan_core(x)
  }
}

fn approximate_atan_core(x: Float) -> Float {
  // Polynomial approximation for -1 <= x <= 1
  let x2 = x *. x
  let x3 = x2 *. x
  let x5 = x3 *. x2
  let x7 = x5 *. x2
  x -. x3 /. 3.0 +. x5 /. 5.0 -. x7 /. 7.0
}

/// Arctangent of y/x, handling all quadrants correctly.
pub fn atan2(y: Float, x: Float) -> Angle {
  case x, y {
    0.0, y_val if y_val >. 0.0 -> AngleRadians(radians: pi /. 2.0)
    0.0, y_val if y_val <. 0.0 -> AngleRadians(radians: 0.0 -. pi /. 2.0)
    0.0, _ -> AngleRadians(radians: 0.0)
    x_val, y_val if x_val >. 0.0 -> atan(y_val /. x_val)
    x_val, y_val if y_val >=. 0.0 ->
      AngleRadians(radians: approximate_atan(y_val /. x_val) +. pi)
    x_val, y_val -> AngleRadians(radians: approximate_atan(y_val /. x_val) -. pi)
  }
}

/// Linear interpolation between two angles (shortest path).
pub fn lerp(angle_a: Angle, angle_b: Angle, t: Float) -> Angle {
  let clamped_t = clamp_float(t, 0.0, 1.0)
  let diff = normalize_signed(subtract(angle_b, angle_a))
  add(angle_a, multiply(diff, clamped_t))
}

fn clamp_float(value: Float, min: Float, max: Float) -> Float {
  case value <. min {
    True -> min
    False ->
      case value >. max {
        True -> max
        False -> value
      }
  }
}

/// Calculate the shortest angular distance between two angles.
pub fn angular_distance(angle_a: Angle, angle_b: Angle) -> Angle {
  let diff = normalize_signed(subtract(angle_b, angle_a))
  abs(diff)
}

/// Check if an angle is in a given range (handles wrap-around).
pub fn is_between(angle: Angle, start: Angle, end: Angle) -> Bool {
  let normalized = normalize(angle)
  let normalized_start = normalize(start)
  let normalized_end = normalize(end)

  case normalized_start.radians <=. normalized_end.radians {
    True ->
      normalized.radians >=. normalized_start.radians
      && normalized.radians <=. normalized_end.radians
    False ->
      normalized.radians >=. normalized_start.radians
      || normalized.radians <=. normalized_end.radians
  }
}

/// Common angles.
pub fn zero() -> Angle {
  AngleRadians(radians: 0.0)
}

pub fn right_angle() -> Angle {
  AngleRadians(radians: pi /. 2.0)
}

pub fn straight_angle() -> Angle {
  AngleRadians(radians: pi)
}

pub fn full_rotation() -> Angle {
  AngleRadians(radians: tau)
}
