# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeFloat - Safe floating-point operations for Nickel
#
# Provides safe floating-point operations with proper handling
# of NaN, Infinity, and precision issues.

{
  # Result type
  Result = {
    value | Number,
    ok | Bool,
  },

  # Floating-point constants
  EPSILON = 0.0000001,
  MAX_SAFE_INTEGER = 9007199254740991,
  MIN_SAFE_INTEGER = -9007199254740991,

  # Check if number is a safe integer
  is_safe_integer
    : Number -> Bool
    = fun n =>
      let truncated = std.number.truncate n in
      n == truncated &&
      n >= MIN_SAFE_INTEGER &&
      n <= MAX_SAFE_INTEGER,

  # Check if number is finite (not NaN or Infinity)
  is_finite
    : Number -> Bool
    = fun n =>
      # In Nickel, numbers are generally finite
      # This is a conceptual check
      n == n && n != (1 / 0) && n != (-1 / 0),

  # Check if number is positive
  is_positive
    : Number -> Bool
    = fun n => n > 0,

  # Check if number is negative
  is_negative
    : Number -> Bool
    = fun n => n < 0,

  # Check if number is zero
  is_zero
    : Number -> Bool
    = fun n => n == 0,

  # Approximate equality check
  approx_equal
    : Number -> Number -> Number -> Bool
    = fun a b epsilon =>
      let diff = if a > b then a - b else b - a in
      diff <= epsilon,

  # Default approximate equality
  nearly_equal
    : Number -> Number -> Bool
    = fun a b =>
      approx_equal a b EPSILON,

  # Safe division with zero check
  safe_div
    : Number -> Number -> { value : Number, ok : Bool }
    = fun a b =>
      if b == 0 then
        { value = 0, ok = false }
      else
        { value = a / b, ok = true },

  # Safe modulo with zero check
  safe_mod
    : Number -> Number -> { value : Number, ok : Bool }
    = fun a b =>
      if b == 0 then
        { value = 0, ok = false }
      else
        { value = a % b, ok = true },

  # Round to n decimal places
  round_to
    : Number -> Number -> Number
    = fun n decimals =>
      let multiplier = std.number.pow 10 decimals in
      let shifted = n * multiplier in
      let rounded = std.number.truncate (shifted + (if shifted >= 0 then 0.5 else -0.5)) in
      rounded / multiplier,

  # Floor function
  floor
    : Number -> Number
    = fun n =>
      let truncated = std.number.truncate n in
      if n < 0 && n != truncated then
        truncated - 1
      else
        truncated,

  # Ceiling function
  ceil
    : Number -> Number
    = fun n =>
      let truncated = std.number.truncate n in
      if n > 0 && n != truncated then
        truncated + 1
      else
        truncated,

  # Truncate towards zero
  trunc
    : Number -> Number
    = std.number.truncate,

  # Sign function (-1, 0, or 1)
  sign
    : Number -> Number
    = fun n =>
      if n > 0 then 1
      else if n < 0 then -1
      else 0,

  # Absolute value
  abs
    : Number -> Number
    = fun n =>
      if n < 0 then -n else n,

  # Minimum of two numbers
  min
    : Number -> Number -> Number
    = fun a b =>
      if a < b then a else b,

  # Maximum of two numbers
  max
    : Number -> Number -> Number
    = fun a b =>
      if a > b then a else b,

  # Clamp value to range
  clamp
    : Number -> Number -> Number -> Number
    = fun value min_val max_val =>
      max min_val (min max_val value),

  # Linear interpolation
  lerp
    : Number -> Number -> Number -> Number
    = fun a b t =>
      a + (b - a) * t,

  # Safe square root
  safe_sqrt
    : Number -> { value : Number, ok : Bool }
    = fun n =>
      if n < 0 then
        { value = 0, ok = false }
      else
        { value = std.number.pow n 0.5, ok = true },

  # Power with overflow check (simplified)
  safe_pow
    : Number -> Number -> { value : Number, ok : Bool }
    = fun base exp =>
      let result = std.number.pow base exp in
      if is_finite result then
        { value = result, ok = true }
      else
        { value = 0, ok = false },

  # Calculate percentage
  percentage
    : Number -> Number -> { value : Number, ok : Bool }
    = fun part whole =>
      if whole == 0 then
        { value = 0, ok = false }
      else
        { value = (part / whole) * 100, ok = true },

  # Contracts
  Finite = std.contract.from_predicate is_finite,
  SafeInteger = std.contract.from_predicate is_safe_integer,
  Positive = std.contract.from_predicate is_positive,
  NonNegative = std.contract.from_predicate (fun n => n >= 0),
  Negative = std.contract.from_predicate is_negative,
  NonPositive = std.contract.from_predicate (fun n => n <= 0),

  # Unit interval [0, 1]
  UnitInterval = std.contract.from_predicate (fun n => n >= 0 && n <= 1),

  # Percentage [0, 100]
  Percentage = std.contract.from_predicate (fun n => n >= 0 && n <= 100),
}
