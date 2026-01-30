-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeAngle operations
|||
||| This module exports angle operations and conversions to the C ABI
||| via Idris2's RefC backend. All functions are proven total and handle angles safely.
|||
||| Return conventions:
||| - Angle unit → Int (0=Degrees, 1=Radians, 2=Gradians, 3=Turns)
||| - Trigonometric functions → (Int, Double) where status 0 = success, 1 = error
||| - Conversions → Double
|||
||| CRITICAL: Angle normalization prevents accumulated errors in rotations.
|||           Always normalize angles before comparison or after arithmetic.
|||
||| Unit conversions:
||| - Degrees: 0-360° (full circle)
||| - Radians: 0-2π (full circle)
||| - Gradians: 0-400 (full circle, metric system)
||| - Turns: 0-1 (full circle, computer graphics)
|||
||| Normalization ranges:
||| - Positive: [0, 360), [0, 2π), [0, 400), [0, 1)
||| - Symmetric: [-180, 180), [-π, π), [-200, 200), [-0.5, 0.5)
module Proven.FFI.SafeAngle

import Proven.SafeAngle
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

||| Encode AngleUnit as Int
encodeAngleUnit : AngleUnit -> Int
encodeAngleUnit Degrees = 0
encodeAngleUnit Radians = 1
encodeAngleUnit Gradians = 2
encodeAngleUnit Turns = 3

||| Decode Int to AngleUnit
decodeAngleUnit : Int -> Maybe AngleUnit
decodeAngleUnit 0 = Just Degrees
decodeAngleUnit 1 = Just Radians
decodeAngleUnit 2 = Just Gradians
decodeAngleUnit 3 = Just Turns
decodeAngleUnit _ = Nothing

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

%export
proven_idris_angle_pi : Double
proven_idris_angle_pi = piConst

%export
proven_idris_angle_tau : Double
proven_idris_angle_tau = tauConst

%export
proven_idris_angle_degrees_per_radian : Double
proven_idris_angle_degrees_per_radian = 180.0 / piConst

%export
proven_idris_angle_radians_per_degree : Double
proven_idris_angle_radians_per_degree = piConst / 180.0

%export
proven_idris_angle_degrees_per_turn : Double
proven_idris_angle_degrees_per_turn = 360.0

%export
proven_idris_angle_gradians_per_turn : Double
proven_idris_angle_gradians_per_turn = 400.0

--------------------------------------------------------------------------------
-- Unit Conversion
--------------------------------------------------------------------------------

%export
proven_idris_angle_degrees_to_radians : Double -> Double
proven_idris_angle_degrees_to_radians d =
  toRadians (fromDegrees d)

%export
proven_idris_angle_radians_to_degrees : Double -> Double
proven_idris_angle_radians_to_degrees r =
  toDegrees (fromRadians r)

%export
proven_idris_angle_degrees_to_gradians : Double -> Double
proven_idris_angle_degrees_to_gradians d =
  toGradians (fromDegrees d)

%export
proven_idris_angle_gradians_to_degrees : Double -> Double
proven_idris_angle_gradians_to_degrees g =
  toDegrees (MkAngle g Gradians)

%export
proven_idris_angle_degrees_to_turns : Double -> Double
proven_idris_angle_degrees_to_turns d =
  toTurns (fromDegrees d)

%export
proven_idris_angle_turns_to_degrees : Double -> Double
proven_idris_angle_turns_to_degrees t =
  toDegrees (MkAngle t Turns)

%export
proven_idris_angle_radians_to_gradians : Double -> Double
proven_idris_angle_radians_to_gradians r =
  toGradians (fromRadians r)

%export
proven_idris_angle_gradians_to_radians : Double -> Double
proven_idris_angle_gradians_to_radians g =
  toRadians (MkAngle g Gradians)

%export
proven_idris_angle_radians_to_turns : Double -> Double
proven_idris_angle_radians_to_turns r =
  toTurns (fromRadians r)

%export
proven_idris_angle_turns_to_radians : Double -> Double
proven_idris_angle_turns_to_radians t =
  toRadians (MkAngle t Turns)

--------------------------------------------------------------------------------
-- Normalization (Positive Range)
--------------------------------------------------------------------------------

%export
proven_idris_angle_normalize_degrees : Double -> Double
proven_idris_angle_normalize_degrees d =
  normalizeDegrees d

%export
proven_idris_angle_normalize_radians : Double -> Double
proven_idris_angle_normalize_radians r =
  normalizeRadians r

%export
proven_idris_angle_normalize_gradians : Double -> Double
proven_idris_angle_normalize_gradians g =
  let a = MkAngle g Gradians
      normalized = normalize a
  in normalized.value

%export
proven_idris_angle_normalize_turns : Double -> Double
proven_idris_angle_normalize_turns t =
  let a = MkAngle t Turns
      normalized = normalize a
  in normalized.value

--------------------------------------------------------------------------------
-- Normalization (Symmetric Range)
--------------------------------------------------------------------------------

%export
proven_idris_angle_normalize_degrees_symmetric : Double -> Double
proven_idris_angle_normalize_degrees_symmetric d =
  normalizeDegreesSymmetric d

%export
proven_idris_angle_normalize_radians_symmetric : Double -> Double
proven_idris_angle_normalize_radians_symmetric r =
  normalizeRadiansSymmetric r

%export
proven_idris_angle_normalize_gradians_symmetric : Double -> Double
proven_idris_angle_normalize_gradians_symmetric g =
  let normalized = normalizeDegrees (g * 0.9)  -- Convert to degrees first
      symmetric = normalizeDegreesSymmetric normalized
  in symmetric * 400.0 / 360.0  -- Back to gradians

%export
proven_idris_angle_normalize_turns_symmetric : Double -> Double
proven_idris_angle_normalize_turns_symmetric t =
  let normalized = t - floor t
  in if normalized >= 0.5 then normalized - 1.0 else normalized

--------------------------------------------------------------------------------
-- Trigonometric Functions
--------------------------------------------------------------------------------

%export
proven_idris_angle_sin_degrees : Double -> Double
proven_idris_angle_sin_degrees d =
  safeSin (fromDegrees d)

%export
proven_idris_angle_cos_degrees : Double -> Double
proven_idris_angle_cos_degrees d =
  safeCos (fromDegrees d)

%export
proven_idris_angle_tan_degrees : Double -> (Int, Double)
proven_idris_angle_tan_degrees d =
  case safeTan (fromDegrees d) of
    Nothing => (1, 0.0)  -- Discontinuity
    Just result => (0, result)

%export
proven_idris_angle_sin_radians : Double -> Double
proven_idris_angle_sin_radians r =
  safeSin (fromRadians r)

%export
proven_idris_angle_cos_radians : Double -> Double
proven_idris_angle_cos_radians r =
  safeCos (fromRadians r)

%export
proven_idris_angle_tan_radians : Double -> (Int, Double)
proven_idris_angle_tan_radians r =
  case safeTan (fromRadians r) of
    Nothing => (1, 0.0)
    Just result => (0, result)

--------------------------------------------------------------------------------
-- Inverse Trigonometric Functions
--------------------------------------------------------------------------------

%export
proven_idris_angle_asin_degrees : Double -> (Int, Double)
proven_idris_angle_asin_degrees x =
  case safeAsin x of
    Nothing => (1, 0.0)  -- Invalid input
    Just angle => (0, toDegrees angle)

%export
proven_idris_angle_acos_degrees : Double -> (Int, Double)
proven_idris_angle_acos_degrees x =
  case safeAcos x of
    Nothing => (1, 0.0)
    Just angle => (0, toDegrees angle)

%export
proven_idris_angle_atan_degrees : Double -> Double
proven_idris_angle_atan_degrees x =
  toDegrees (safeAtan x)

%export
proven_idris_angle_atan2_degrees : Double -> Double -> Double
proven_idris_angle_atan2_degrees y x =
  toDegrees (safeAtan2 y x)

%export
proven_idris_angle_asin_radians : Double -> (Int, Double)
proven_idris_angle_asin_radians x =
  case safeAsin x of
    Nothing => (1, 0.0)
    Just angle => (0, toRadians angle)

%export
proven_idris_angle_acos_radians : Double -> (Int, Double)
proven_idris_angle_acos_radians x =
  case safeAcos x of
    Nothing => (1, 0.0)
    Just angle => (0, toRadians angle)

%export
proven_idris_angle_atan_radians : Double -> Double
proven_idris_angle_atan_radians x =
  toRadians (safeAtan x)

%export
proven_idris_angle_atan2_radians : Double -> Double -> Double
proven_idris_angle_atan2_radians y x =
  toRadians (safeAtan2 y x)

--------------------------------------------------------------------------------
-- Angle Arithmetic
--------------------------------------------------------------------------------

%export
proven_idris_angle_add_degrees : Double -> Double -> Double
proven_idris_angle_add_degrees a b =
  toDegrees (addAngle (fromDegrees a) (fromDegrees b))

%export
proven_idris_angle_subtract_degrees : Double -> Double -> Double
proven_idris_angle_subtract_degrees a b =
  toDegrees (subAngle (fromDegrees a) (fromDegrees b))

%export
proven_idris_angle_scale_degrees : Double -> Double -> Double
proven_idris_angle_scale_degrees scalar angle =
  toDegrees (scaleAngle scalar (fromDegrees angle))

%export
proven_idris_angle_difference_degrees : Double -> Double -> Double
proven_idris_angle_difference_degrees a b =
  toDegrees (angleDifference (fromDegrees a) (fromDegrees b))

%export
proven_idris_angle_add_radians : Double -> Double -> Double
proven_idris_angle_add_radians a b =
  toRadians (addAngle (fromRadians a) (fromRadians b))

%export
proven_idris_angle_subtract_radians : Double -> Double -> Double
proven_idris_angle_subtract_radians a b =
  toRadians (subAngle (fromRadians a) (fromRadians b))

%export
proven_idris_angle_scale_radians : Double -> Double -> Double
proven_idris_angle_scale_radians scalar angle =
  toRadians (scaleAngle scalar (fromRadians angle))

%export
proven_idris_angle_difference_radians : Double -> Double -> Double
proven_idris_angle_difference_radians a b =
  toRadians (angleDifference (fromRadians a) (fromRadians b))

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

%export
proven_idris_angle_is_valid_for_asin_acos : Double -> Int
proven_idris_angle_is_valid_for_asin_acos x =
  encodeBool (x >= -1.0 && x <= 1.0)

%export
proven_idris_angle_is_normalized_degrees : Double -> Int
proven_idris_angle_is_normalized_degrees d =
  encodeBool (d >= 0.0 && d < 360.0)

%export
proven_idris_angle_is_normalized_radians : Double -> Int
proven_idris_angle_is_normalized_radians r =
  encodeBool (r >= 0.0 && r < tauConst)

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_angle_friendly_error : String -> String
proven_idris_angle_friendly_error errorMsg =
  if isInfixOf "asin" (toLower errorMsg) || isInfixOf "acos" (toLower errorMsg)
    then "Inverse trig function input out of range (must be -1 to 1)"
  else if isInfixOf "tan" (toLower errorMsg) || isInfixOf "discontinuity" (toLower errorMsg)
    then "Tangent undefined at ±90° (±π/2)"
  else if isInfixOf "unit" (toLower errorMsg)
    then "Invalid angle unit (expected degrees, radians, gradians, or turns)"
  else if isInfixOf "normalize" (toLower errorMsg)
    then "Angle normalization error"
  else
    "Angle operation error"
