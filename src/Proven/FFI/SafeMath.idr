-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeMath operations
|||
||| This module exports safe arithmetic operations to the C ABI via Idris2's RefC backend.
||| All functions are proven total and handle edge cases (division by zero, overflow, underflow).
|||
||| Return convention:
||| - Tuple of (status: Int, value: Integer)
||| - status = 0: Success, value is valid
||| - status = 1: Error (division by zero, overflow, underflow, etc.)
module Proven.FFI.SafeMath

import Proven.SafeMath
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Maybe Integer as (status, value) tuple for FFI
||| Nothing → (1, 0)  -- Error status
||| Just x  → (0, x)  -- Success status
encodeResult : Maybe Integer -> (Int, Integer)
encodeResult Nothing = (1, 0)
encodeResult (Just x) = (0, x)

--------------------------------------------------------------------------------
-- Safe Division
--------------------------------------------------------------------------------

%export
proven_idris_math_div : Integer -> Integer -> (Int, Integer)
proven_idris_math_div numerator denominator =
  encodeResult (div numerator denominator)

%export
proven_idris_math_div_or : Integer -> Integer -> Integer -> Integer
proven_idris_math_div_or def numerator denominator =
  divOr def numerator denominator

%export
proven_idris_math_mod : Integer -> Integer -> (Int, Integer)
proven_idris_math_mod numerator denominator =
  encodeResult (mod numerator denominator)

--------------------------------------------------------------------------------
-- Checked Arithmetic (Overflow Detection)
--------------------------------------------------------------------------------

%export
proven_idris_math_add_checked : Integer -> Integer -> (Int, Integer)
proven_idris_math_add_checked a b =
  encodeResult (addChecked a b)

%export
proven_idris_math_sub_checked : Integer -> Integer -> (Int, Integer)
proven_idris_math_sub_checked a b =
  encodeResult (subChecked a b)

%export
proven_idris_math_mul_checked : Integer -> Integer -> (Int, Integer)
proven_idris_math_mul_checked a b =
  encodeResult (mulChecked a b)

--------------------------------------------------------------------------------
-- Safe Absolute Value
--------------------------------------------------------------------------------

%export
proven_idris_math_abs_safe : Integer -> (Int, Integer)
proven_idris_math_abs_safe n =
  encodeResult (absSafe n)

%export
proven_idris_math_abs_clamped : Integer -> Integer
proven_idris_math_abs_clamped n =
  absClamped n

--------------------------------------------------------------------------------
-- Safe Power
--------------------------------------------------------------------------------

||| Convert Nat to Integer for FFI (exponent must be >= 0)
||| Negative exponents return error status
natFromInteger : Integer -> Maybe Nat
natFromInteger n =
  if n < 0 then Nothing
  else Just (fromInteger n)

%export
proven_idris_math_pow_checked : Integer -> Integer -> (Int, Integer)
proven_idris_math_pow_checked base exp =
  case natFromInteger exp of
    Nothing => (1, 0)  -- Negative exponent error
    Just expNat =>
      case powChecked base expNat of
        Nothing => (1, 0)  -- Overflow error
        Just result => (0, result)

--------------------------------------------------------------------------------
-- Clamping Operations
--------------------------------------------------------------------------------

%export
proven_idris_math_clamp : Integer -> Integer -> Integer -> Integer
proven_idris_math_clamp lo hi value =
  clamp lo hi value

%export
proven_idris_math_clamp_non_negative : Integer -> Integer
proven_idris_math_clamp_non_negative n =
  clampNonNegative n

--------------------------------------------------------------------------------
-- Comparison Utilities
--------------------------------------------------------------------------------

||| Encode Ordering as Int: LT = -1, EQ = 0, GT = 1
encodeOrdering : Ordering -> Int
encodeOrdering LT = -1
encodeOrdering EQ = 0
encodeOrdering GT = 1

%export
proven_idris_math_compare : Integer -> Integer -> Int
proven_idris_math_compare a b =
  encodeOrdering (compareInt a b)

%export
proven_idris_math_min : Integer -> Integer -> Integer
proven_idris_math_min a b =
  minInt a b

%export
proven_idris_math_max : Integer -> Integer -> Integer
proven_idris_math_max a b =
  maxInt a b

--------------------------------------------------------------------------------
-- Percentage Operations
--------------------------------------------------------------------------------

%export
proven_idris_math_percent_of : Integer -> Integer -> (Int, Integer)
proven_idris_math_percent_of percent total =
  encodeResult (percentOf percent total)

%export
proven_idris_math_as_percent : Integer -> Integer -> (Int, Integer)
proven_idris_math_as_percent part whole =
  encodeResult (asPercent part whole)

--------------------------------------------------------------------------------
-- Bounds Checking
--------------------------------------------------------------------------------

%export
proven_idris_math_fits_int64 : Integer -> Bool
proven_idris_math_fits_int64 n =
  fitsInt64 n

%export
proven_idris_math_max_int64 : Integer
proven_idris_math_max_int64 = maxInt64

%export
proven_idris_math_min_int64 : Integer
proven_idris_math_min_int64 = minInt64
