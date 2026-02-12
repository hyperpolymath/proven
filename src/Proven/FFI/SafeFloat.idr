-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeFloat operations
|||
||| This module exports safe floating-point operations to the C ABI
||| via Idris2's RefC backend. All functions are proven total and handle NaN/Infinity.
|||
||| Return conventions:
||| - Float class → Int (0=Normal, 1=Subnormal, 2=Zero, 3=Infinite, 4=NaN)
||| - Safe operations → (Int, Double) where status 0 = success, 1 = error/invalid
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
|||
||| CRITICAL: NaN and Infinity can propagate through calculations and cause
|||           incorrect results. All operations must validate inputs and outputs.
|||
||| Precision: Default tolerance is 2.220446049250313e-14 (machine epsilon for Double)
|||            Epsilon for numerical stability is 1.0e-15
module Proven.FFI.SafeFloat

import Proven.SafeFloat
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode FloatClass as Int
encodeFloatClass : FloatClass -> Int
encodeFloatClass Normal = 0
encodeFloatClass Subnormal = 1
encodeFloatClass Zero = 2
encodeFloatClass Infinite = 3
encodeFloatClass NaN = 4

||| Decode Int to FloatClass
decodeFloatClass : Int -> Maybe FloatClass
decodeFloatClass 0 = Just Normal
decodeFloatClass 1 = Just Subnormal
decodeFloatClass 2 = Just Zero
decodeFloatClass 3 = Just Infinite
decodeFloatClass 4 = Just NaN
decodeFloatClass _ = Nothing

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

export
proven_idris_float_epsilon : Double
proven_idris_float_epsilon = epsilon

export
proven_idris_float_default_tolerance : Double
proven_idris_float_default_tolerance = defaultTolerance

export
proven_idris_float_positive_infinity : Double
proven_idris_float_positive_infinity = 1.0 / 0.0

export
proven_idris_float_negative_infinity : Double
proven_idris_float_negative_infinity = (-1.0) / 0.0

export
proven_idris_float_nan : Double
proven_idris_float_nan = 0.0 / 0.0

--------------------------------------------------------------------------------
-- Classification
--------------------------------------------------------------------------------

export
proven_idris_float_is_nan : Double -> Int
proven_idris_float_is_nan x = encodeBool (isNaN x)

export
proven_idris_float_is_infinite : Double -> Int
proven_idris_float_is_infinite x = encodeBool (isInfinite x)

export
proven_idris_float_is_finite : Double -> Int
proven_idris_float_is_finite x = encodeBool (isFinite x)

export
proven_idris_float_is_positive_infinity : Double -> Int
proven_idris_float_is_positive_infinity x =
  encodeBool (x == (1.0 / 0.0))

export
proven_idris_float_is_negative_infinity : Double -> Int
proven_idris_float_is_negative_infinity x =
  encodeBool (x == ((-1.0) / 0.0))

export
proven_idris_float_is_zero : Double -> Int
proven_idris_float_is_zero x = encodeBool (x == 0.0)

export
proven_idris_float_is_positive : Double -> Int
proven_idris_float_is_positive x = encodeBool (x > 0.0)

export
proven_idris_float_is_negative : Double -> Int
proven_idris_float_is_negative x = encodeBool (x < 0.0)

--------------------------------------------------------------------------------
-- Sanitization
--------------------------------------------------------------------------------

export
proven_idris_float_sanitize : Double -> Double -> Double
proven_idris_float_sanitize def x = sanitize def x

export
proven_idris_float_replace_nan : Double -> Double -> Double
proven_idris_float_replace_nan def x =
  if isNaN x then def else x

export
proven_idris_float_replace_infinite : Double -> Double -> Double
proven_idris_float_replace_infinite def x =
  if isInfinite x then def else x

--------------------------------------------------------------------------------
-- Safe Operations (return status, value)
--------------------------------------------------------------------------------

export
proven_idris_float_safe_div : Double -> Double -> (Int, Double)
proven_idris_float_safe_div x y =
  case safeDiv x y of
    Nothing => (1, 0.0)  -- Error
    Just result => (0, result)

export
proven_idris_float_safe_sqrt : Double -> (Int, Double)
proven_idris_float_safe_sqrt x =
  case safeSqrt x of
    Nothing => (1, 0.0)
    Just result => (0, result)

export
proven_idris_float_safe_log : Double -> (Int, Double)
proven_idris_float_safe_log x =
  case safeLog x of
    Nothing => (1, 0.0)
    Just result => (0, result)

export
proven_idris_float_safe_add : Double -> Double -> (Int, Double)
proven_idris_float_safe_add x y =
  case safeAdd x y of
    Nothing => (1, 0.0)
    Just result => (0, result)

export
proven_idris_float_safe_mul : Double -> Double -> (Int, Double)
proven_idris_float_safe_mul x y =
  case safeMul x y of
    Nothing => (1, 0.0)
    Just result => (0, result)

export
proven_idris_float_safe_pow : Double -> Double -> (Int, Double)
proven_idris_float_safe_pow base exp =
  case pow base exp of
    Nothing => (1, 0.0)
    Just result => (0, result)

--------------------------------------------------------------------------------
-- Comparison
--------------------------------------------------------------------------------

export
proven_idris_float_approx_equal : Double -> Double -> Double -> Int
proven_idris_float_approx_equal tol a b =
  encodeBool (approxEqual tol a b)

export
proven_idris_float_approx_equal_default : Double -> Double -> Int
proven_idris_float_approx_equal_default a b =
  encodeBool (approxEqual defaultTolerance a b)

export
proven_idris_float_compare : Double -> Double -> Int
proven_idris_float_compare a b =
  if a < b then (-1)
  else if a > b then 1
  else 0

--------------------------------------------------------------------------------
-- Clamping
--------------------------------------------------------------------------------

export
proven_idris_float_clamp : Double -> Double -> Double -> Double
proven_idris_float_clamp lo hi x = clampDouble lo hi x

export
proven_idris_float_clamp_unit : Double -> Double
proven_idris_float_clamp_unit x = clampUnit x

export
proven_idris_float_is_in_range : Double -> Double -> Double -> Int
proven_idris_float_is_in_range lo hi x =
  encodeBool (x >= lo && x <= hi)

--------------------------------------------------------------------------------
-- Rounding
--------------------------------------------------------------------------------

export
proven_idris_float_round_to_int : Double -> Int
proven_idris_float_round_to_int x = cast (roundToInt x)

export
proven_idris_float_floor_to_int : Double -> Int
proven_idris_float_floor_to_int x = cast (floorToInt x)

export
proven_idris_float_ceil_to_int : Double -> Int
proven_idris_float_ceil_to_int x = cast (ceilToInt x)

export
proven_idris_float_round_to_places : Int -> Double -> Double
proven_idris_float_round_to_places places x =
  roundTo (cast places) x

--------------------------------------------------------------------------------
-- Interpolation
--------------------------------------------------------------------------------

export
proven_idris_float_lerp : Double -> Double -> Double -> Double
proven_idris_float_lerp t a b = lerp t a b

export
proven_idris_float_inv_lerp : Double -> Double -> Double -> (Int, Double)
proven_idris_float_inv_lerp a b x =
  case invLerp a b x of
    Nothing => (1, 0.0)
    Just result => (0, result)

export
proven_idris_float_remap : Double -> Double -> Double -> Double -> Double -> Double
proven_idris_float_remap inMin inMax outMin outMax x =
  let t = (x - inMin) / (inMax - inMin)
  in lerp t outMin outMax

--------------------------------------------------------------------------------
-- Absolute and Sign
--------------------------------------------------------------------------------

export
proven_idris_float_abs : Double -> Double
proven_idris_float_abs x = abs x

export
proven_idris_float_sign : Double -> Int
proven_idris_float_sign x =
  if x > 0.0 then 1
  else if x < 0.0 then (-1)
  else 0

export
proven_idris_float_negate : Double -> Double
proven_idris_float_negate x = negate x

--------------------------------------------------------------------------------
-- Min/Max
--------------------------------------------------------------------------------

export
proven_idris_float_min : Double -> Double -> Double
proven_idris_float_min a b = if a < b then a else b

export
proven_idris_float_max : Double -> Double -> Double
proven_idris_float_max a b = if a > b then a else b

--------------------------------------------------------------------------------
-- Statistics Helpers (single values, not list operations)
--------------------------------------------------------------------------------

export
proven_idris_float_sum : Double -> Double -> Double
proven_idris_float_sum a b = a + b

export
proven_idris_float_product : Double -> Double -> Double
proven_idris_float_product a b = a * b

export
proven_idris_float_average : Double -> Double -> Double
proven_idris_float_average a b = (a + b) / 2.0

--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

export
proven_idris_float_from_int : Int -> Double
proven_idris_float_from_int n = cast n

export
proven_idris_float_to_int_truncate : Double -> Int
proven_idris_float_to_int_truncate x = cast {to = Int} x

export
proven_idris_float_to_int_round : Double -> Int
proven_idris_float_to_int_round x = proven_idris_float_round_to_int x

--------------------------------------------------------------------------------
-- Validation Helpers
--------------------------------------------------------------------------------

export
proven_idris_float_is_valid_for_division : Double -> Int
proven_idris_float_is_valid_for_division y =
  encodeBool (y /= 0.0 && isFinite y)

export
proven_idris_float_is_valid_for_sqrt : Double -> Int
proven_idris_float_is_valid_for_sqrt x =
  encodeBool (x >= 0.0 && isFinite x)

export
proven_idris_float_is_valid_for_log : Double -> Int
proven_idris_float_is_valid_for_log x =
  encodeBool (x > 0.0 && isFinite x)

export
proven_idris_float_is_representable : Double -> Int
proven_idris_float_is_representable x = encodeBool (isFinite x)

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_float_friendly_error : String -> String
proven_idris_float_friendly_error errorMsg =
  if isInfixOf "nan" (toLower errorMsg) || isInfixOf "not a number" (toLower errorMsg)
    then "Operation resulted in NaN (not a number)"
  else if isInfixOf "infinity" (toLower errorMsg) || isInfixOf "infinite" (toLower errorMsg)
    then "Operation resulted in infinity (overflow or division by zero)"
  else if isInfixOf "division" (toLower errorMsg) || isInfixOf "divide" (toLower errorMsg)
    then "Division by zero or invalid divisor"
  else if isInfixOf "sqrt" (toLower errorMsg) || isInfixOf "square root" (toLower errorMsg)
    then "Square root of negative number"
  else if isInfixOf "log" (toLower errorMsg) || isInfixOf "logarithm" (toLower errorMsg)
    then "Logarithm of non-positive number"
  else if isInfixOf "overflow" (toLower errorMsg)
    then "Arithmetic overflow (result too large)"
  else if isInfixOf "underflow" (toLower errorMsg)
    then "Arithmetic underflow (result too small)"
  else if isInfixOf "precision" (toLower errorMsg)
    then "Precision loss in floating-point operation"
  else
    "Floating-point operation error"
