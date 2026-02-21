// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafeFloat - Floating point operations that cannot crash.
////
//// Provides safe floating point arithmetic with NaN and Infinity handling.

import gleam/float
import gleam/int
import gleam/order.{type Order}

/// Error types for float operations.
pub type FloatError {
  DivisionByZero
  InvalidInput(message: String)
  Overflow
  Underflow
  NaN
}

/// Safe division that returns Error on division by zero.
pub fn div(numerator: Float, denominator: Float) -> Result(Float, FloatError) {
  case denominator == 0.0 {
    True -> Error(DivisionByZero)
    False -> Ok(numerator /. denominator)
  }
}

/// Safe division with a default value for division by zero.
pub fn div_or(default: Float, numerator: Float, denominator: Float) -> Float {
  case div(numerator, denominator) {
    Ok(v) -> v
    Error(_) -> default
  }
}

/// Safe square root that returns Error for negative numbers.
pub fn sqrt(value: Float) -> Result(Float, FloatError) {
  case value <. 0.0 {
    True -> Error(InvalidInput(message: "Cannot take square root of negative number"))
    False -> Ok(float.square_root(value) |> result_or(0.0))
  }
}

fn result_or(result: Result(Float, Nil), default: Float) -> Float {
  case result {
    Ok(v) -> v
    Error(_) -> default
  }
}

/// Safe natural logarithm that returns Error for non-positive numbers.
pub fn ln(value: Float) -> Result(Float, FloatError) {
  case value <=. 0.0 {
    True -> Error(InvalidInput(message: "Logarithm undefined for non-positive values"))
    False -> {
      // Gleam doesn't have a built-in ln, so we use a Taylor series approximation
      // In production, use platform-native math functions
      Ok(approximate_ln(value))
    }
  }
}

fn approximate_ln(x: Float) -> Float {
  // Use the identity: ln(x) = 2 * artanh((x-1)/(x+1))
  // For x near 1, this converges well
  case x == 1.0 {
    True -> 0.0
    False -> {
      let y = { x -. 1.0 } /. { x +. 1.0 }
      let y2 = y *. y
      2.0
      *. y
      *. { 1.0 +. y2 /. 3.0 +. y2 *. y2 /. 5.0 +. y2 *. y2 *. y2 /. 7.0 }
    }
  }
}

/// Safe power function.
pub fn pow(base: Float, exponent: Float) -> Result(Float, FloatError) {
  case base, exponent {
    0.0, exp if exp <. 0.0 -> Error(DivisionByZero)
    b, _ if b <. 0.0 -> Error(InvalidInput(message: "Negative base with fractional exponent"))
    _, _ -> Ok(float.power(base, exponent) |> result_or(1.0))
  }
}

/// Safe modulo for floats.
pub fn modulo(numerator: Float, denominator: Float) -> Result(Float, FloatError) {
  case denominator == 0.0 {
    True -> Error(DivisionByZero)
    False -> {
      let quotient = float.truncate(numerator /. denominator)
      Ok(numerator -. int.to_float(quotient) *. denominator)
    }
  }
}

/// Round to specified decimal places.
pub fn round_to(value: Float, decimal_places: Int) -> Float {
  let multiplier = pow10(decimal_places)
  let shifted = value *. multiplier
  let rounded = float.round(shifted)
  int.to_float(rounded) /. multiplier
}

fn pow10(exponent: Int) -> Float {
  pow10_acc(exponent, 1.0)
}

fn pow10_acc(exponent: Int, accumulated: Float) -> Float {
  case exponent <= 0 {
    True -> accumulated
    False -> pow10_acc(exponent - 1, accumulated *. 10.0)
  }
}

/// Clamp a float to a range.
pub fn clamp(value: Float, min: Float, max: Float) -> Float {
  case value <. min {
    True -> min
    False ->
      case value >. max {
        True -> max
        False -> value
      }
  }
}

/// Linear interpolation between two values.
pub fn lerp(start: Float, end: Float, t: Float) -> Float {
  let clamped_t = clamp(t, 0.0, 1.0)
  start +. { end -. start } *. clamped_t
}

/// Inverse linear interpolation.
pub fn inverse_lerp(start: Float, end: Float, value: Float) -> Result(Float, FloatError) {
  case end -. start == 0.0 {
    True -> Error(DivisionByZero)
    False -> Ok({ value -. start } /. { end -. start })
  }
}

/// Check if two floats are approximately equal within an epsilon.
pub fn approx_equal(value_a: Float, value_b: Float, epsilon: Float) -> Bool {
  float.absolute_value(value_a -. value_b) <=. epsilon
}

/// Check if two floats are approximately equal using relative epsilon.
pub fn approx_equal_relative(
  value_a: Float,
  value_b: Float,
  relative_epsilon: Float,
) -> Bool {
  let diff = float.absolute_value(value_a -. value_b)
  let max_val =
    float.max(float.absolute_value(value_a), float.absolute_value(value_b))
  diff <=. max_val *. relative_epsilon
}

/// Safe comparison of floats.
pub fn compare(value_a: Float, value_b: Float) -> Order {
  float.compare(value_a, value_b)
}

/// Check if a float is finite (not NaN or Infinity).
pub fn is_finite(value: Float) -> Bool {
  // In pure Gleam, we can check by comparing to a large value
  // In practice, platform-native checks should be used
  value == value && value <. 1.0e308 && value >. -1.0e308
}

/// Check if a float is positive.
pub fn is_positive(value: Float) -> Bool {
  value >. 0.0
}

/// Check if a float is negative.
pub fn is_negative(value: Float) -> Bool {
  value <. 0.0
}

/// Check if a float is zero.
pub fn is_zero(value: Float) -> Bool {
  value == 0.0
}

/// Get the sign of a float (-1, 0, or 1).
pub fn sign(value: Float) -> Int {
  case value {
    v if v >. 0.0 -> 1
    v if v <. 0.0 -> -1
    _ -> 0
  }
}

/// Get the fractional part of a float.
pub fn fract(value: Float) -> Float {
  value -. int.to_float(float.truncate(value))
}

/// Convert degrees to radians.
pub fn degrees_to_radians(degrees: Float) -> Float {
  degrees *. 3.14159265358979323846 /. 180.0
}

/// Convert radians to degrees.
pub fn radians_to_degrees(radians: Float) -> Float {
  radians *. 180.0 /. 3.14159265358979323846
}

/// Calculate the hypotenuse of a right triangle.
pub fn hypot(x: Float, y: Float) -> Float {
  let abs_x = float.absolute_value(x)
  let abs_y = float.absolute_value(y)
  let max_val = float.max(abs_x, abs_y)
  let min_val = float.min(abs_x, abs_y)
  case max_val == 0.0 {
    True -> 0.0
    False -> {
      let ratio = min_val /. max_val
      max_val *. { 1.0 +. ratio *. ratio } |> approximate_sqrt()
    }
  }
}

fn approximate_sqrt(value: Float) -> Float {
  case float.square_root(value) {
    Ok(v) -> v
    Error(_) -> 0.0
  }
}

/// Map a value from one range to another.
pub fn map_range(
  value: Float,
  in_min: Float,
  in_max: Float,
  out_min: Float,
  out_max: Float,
) -> Result(Float, FloatError) {
  case in_max -. in_min == 0.0 {
    True -> Error(DivisionByZero)
    False -> {
      let normalized = { value -. in_min } /. { in_max -. in_min }
      Ok(out_min +. normalized *. { out_max -. out_min })
    }
  }
}

/// Smooth step interpolation (cubic Hermite).
pub fn smooth_step(edge0: Float, edge1: Float, x: Float) -> Float {
  let t = clamp({ x -. edge0 } /. { edge1 -. edge0 }, 0.0, 1.0)
  t *. t *. { 3.0 -. 2.0 *. t }
}

/// Smoother step interpolation (quintic).
pub fn smoother_step(edge0: Float, edge1: Float, x: Float) -> Float {
  let t = clamp({ x -. edge0 } /. { edge1 -. edge0 }, 0.0, 1.0)
  t *. t *. t *. { t *. { t *. 6.0 -. 15.0 } +. 10.0 }
}

/// Parse a float from string safely.
pub fn parse(input: String) -> Result(Float, FloatError) {
  case float.parse(input) {
    Ok(f) -> Ok(f)
    Error(_) ->
      case int.parse(input) {
        Ok(i) -> Ok(int.to_float(i))
        Error(_) -> Error(InvalidInput(message: "Not a valid number"))
      }
  }
}

/// Format a float to string with specified decimal places.
pub fn format(value: Float, decimal_places: Int) -> String {
  let rounded = round_to(value, decimal_places)
  float.to_string(rounded)
}
