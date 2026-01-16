# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeMath - Overflow-checked arithmetic for Nickel
#
# All operations return Result records with { value, ok } fields.
# Never throws, never crashes.

let MAX_INT64 = 9223372036854775807 in
let MIN_INT64 = -9223372036854775808 in

{
  # Maximum 64-bit signed integer
  MAX_INT64 = MAX_INT64,

  # Minimum 64-bit signed integer
  MIN_INT64 = MIN_INT64,

  # Result contract for safe operations
  Result = {
    value | Number,
    ok | Bool,
  },

  # Safe addition with overflow checking
  # Returns { value: Number, ok: Bool }
  safe_add
    : Number -> Number -> { value : Number, ok : Bool }
    = fun a b =>
      let result = a + b in
      if b > 0 && a > MAX_INT64 - b then
        { value = 0, ok = false }
      else if b < 0 && a < MIN_INT64 - b then
        { value = 0, ok = false }
      else
        { value = result, ok = true },

  # Safe subtraction with overflow checking
  safe_sub
    : Number -> Number -> { value : Number, ok : Bool }
    = fun a b =>
      if b < 0 && a > MAX_INT64 + b then
        { value = 0, ok = false }
      else if b > 0 && a < MIN_INT64 + b then
        { value = 0, ok = false }
      else
        { value = a - b, ok = true },

  # Safe multiplication with overflow checking
  safe_mul
    : Number -> Number -> { value : Number, ok : Bool }
    = fun a b =>
      if a == 0 || b == 0 then
        { value = 0, ok = true }
      else
        let result = a * b in
        # Check for overflow by verifying result / a == b
        if a != 0 && (result / a) != b then
          { value = 0, ok = false }
        else
          { value = result, ok = true },

  # Safe division with zero check
  safe_div
    : Number -> Number -> { value : Number, ok : Bool }
    = fun a b =>
      if b == 0 then
        { value = 0, ok = false }
      else if a == MIN_INT64 && b == -1 then
        { value = 0, ok = false }
      else
        { value = std.number.truncate (a / b), ok = true },

  # Safe modulo with zero check
  safe_mod
    : Number -> Number -> { value : Number, ok : Bool }
    = fun a b =>
      if b == 0 then
        { value = 0, ok = false }
      else
        { value = a % b, ok = true },

  # Safe absolute value (handles MIN_INT64)
  safe_abs
    : Number -> { value : Number, ok : Bool }
    = fun a =>
      if a == MIN_INT64 then
        { value = 0, ok = false }
      else if a < 0 then
        { value = -a, ok = true }
      else
        { value = a, ok = true },

  # Clamp value to range [min, max]
  clamp
    : Number -> Number -> Number -> Number
    = fun value min_val max_val =>
      if value < min_val then min_val
      else if value > max_val then max_val
      else value,

  # Check if value is in range [min, max]
  in_range
    : Number -> Number -> Number -> Bool
    = fun value min_val max_val =>
      value >= min_val && value <= max_val,

  # Safe percentage (0-100)
  Percentage = std.contract.from_predicate (fun x => x >= 0 && x <= 100),

  # Safe port number (0-65535)
  Port = std.contract.from_predicate (fun x => x >= 0 && x <= 65535),

  # Positive number contract
  Positive = std.contract.from_predicate (fun x => x > 0),

  # Non-negative number contract
  NonNegative = std.contract.from_predicate (fun x => x >= 0),
}
