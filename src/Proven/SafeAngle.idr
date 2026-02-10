-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeAngle - Safe angle operations with normalization
|||
||| This module provides safe angle operations including
||| normalization, unit conversion, and trigonometry.
module Proven.SafeAngle
import Data.String
import Data.List

import public Proven.Core
import public Proven.SafeFloat

%default total

--------------------------------------------------------------------------------
-- Angle Types
--------------------------------------------------------------------------------

||| Angle unit
public export
data AngleUnit : Type where
  Degrees : AngleUnit
  Radians : AngleUnit
  Gradians : AngleUnit
  Turns : AngleUnit

public export
Eq AngleUnit where
  Degrees == Degrees = True
  Radians == Radians = True
  Gradians == Gradians = True
  Turns == Turns = True
  _ == _ = False

||| An angle with its unit
public export
record Angle where
  constructor MkAngle
  value : Double
  unit : AngleUnit

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

||| Pi constant
public export
piConst : Double
piConst = 3.141592653589793

||| Tau (2*pi) constant
public export
tauConst : Double
tauConst = 6.283185307179586

--------------------------------------------------------------------------------
-- Unit Conversion
--------------------------------------------------------------------------------

||| Convert angle to radians
public export
toRadians : Angle -> Double
toRadians (MkAngle v Radians) = v
toRadians (MkAngle v Degrees) = v * piConst / 180.0
toRadians (MkAngle v Gradians) = v * piConst / 200.0
toRadians (MkAngle v Turns) = v * tauConst

||| Convert angle to degrees
public export
toDegrees : Angle -> Double
toDegrees (MkAngle v Degrees) = v
toDegrees (MkAngle v Radians) = v * 180.0 / piConst
toDegrees (MkAngle v Gradians) = v * 0.9  -- 360/400
toDegrees (MkAngle v Turns) = v * 360.0

||| Convert angle to gradians
public export
toGradians : Angle -> Double
toGradians (MkAngle v Gradians) = v
toGradians (MkAngle v Degrees) = v * 400.0 / 360.0
toGradians (MkAngle v Radians) = v * 200.0 / piConst
toGradians (MkAngle v Turns) = v * 400.0

||| Convert angle to turns
public export
toTurns : Angle -> Double
toTurns (MkAngle v Turns) = v
toTurns (MkAngle v Degrees) = v / 360.0
toTurns (MkAngle v Radians) = v / tauConst
toTurns (MkAngle v Gradians) = v / 400.0

||| Create angle from degrees
public export
fromDegrees : Double -> Angle
fromDegrees d = MkAngle d Degrees

||| Create angle from radians
public export
fromRadians : Double -> Angle
fromRadians r = MkAngle r Radians

--------------------------------------------------------------------------------
-- Normalization
--------------------------------------------------------------------------------

||| Normalize angle to [0, 360) degrees
public export
normalizeDegrees : Double -> Double
normalizeDegrees d =
  let n = d - 360.0 * floor (d / 360.0)
  in if n < 0.0 then n + 360.0 else n

||| Normalize angle to [0, 2*pi) radians
public export
normalizeRadians : Double -> Double
normalizeRadians r =
  let n = r - tauConst * floor (r / tauConst)
  in if n < 0.0 then n + tauConst else n

||| Normalize angle to [-180, 180) degrees
public export
normalizeDegreesSymmetric : Double -> Double
normalizeDegreesSymmetric d =
  let n = normalizeDegrees d
  in if n >= 180.0 then n - 360.0 else n

||| Normalize angle to [-pi, pi) radians
public export
normalizeRadiansSymmetric : Double -> Double
normalizeRadiansSymmetric r =
  let n = normalizeRadians r
  in if n >= piConst then n - tauConst else n

||| Normalize an angle in its current unit
public export
normalize : Angle -> Angle
normalize (MkAngle v Degrees) = MkAngle (normalizeDegrees v) Degrees
normalize (MkAngle v Radians) = MkAngle (normalizeRadians v) Radians
normalize (MkAngle v Gradians) =
  let n = v - 400.0 * floor (v / 400.0)
  in MkAngle (if n < 0.0 then n + 400.0 else n) Gradians
normalize (MkAngle v Turns) =
  let n = v - floor v
  in MkAngle (if n < 0.0 then n + 1.0 else n) Turns

--------------------------------------------------------------------------------
-- Trigonometric Functions (safe wrappers)
--------------------------------------------------------------------------------

||| Safe sine
public export
safeSin : Angle -> Double
safeSin a = sin (toRadians a)

||| Safe cosine
public export
safeCos : Angle -> Double
safeCos a = cos (toRadians a)

||| Safe tangent (returns Nothing at discontinuities)
public export
safeTan : Angle -> Maybe Double
safeTan a =
  let r = normalizeRadians (toRadians a)
  in if approxEqual 0.0001 r (piConst / 2.0) ||
        approxEqual 0.0001 r (3.0 * piConst / 2.0)
     then Nothing
     else Just (tan r)

||| Safe arcsine (returns Nothing outside [-1, 1])
public export
safeAsin : Double -> Maybe Angle
safeAsin x =
  if x < -1.0 || x > 1.0
    then Nothing
    else Just (MkAngle (asin x) Radians)

||| Safe arccosine (returns Nothing outside [-1, 1])
public export
safeAcos : Double -> Maybe Angle
safeAcos x =
  if x < -1.0 || x > 1.0
    then Nothing
    else Just (MkAngle (acos x) Radians)

||| Safe arctangent
public export
safeAtan : Double -> Angle
safeAtan x = MkAngle (atan x) Radians

||| Safe arctangent with two arguments
public export
safeAtan2 : Double -> Double -> Angle
safeAtan2 y x = MkAngle (atan2 y x) Radians

--------------------------------------------------------------------------------
-- Angle Arithmetic
--------------------------------------------------------------------------------

||| Add two angles
public export
addAngle : Angle -> Angle -> Angle
addAngle a b = MkAngle (toRadians a + toRadians b) Radians

||| Subtract angles
public export
subAngle : Angle -> Angle -> Angle
subAngle a b = MkAngle (toRadians a - toRadians b) Radians

||| Multiply angle by scalar
public export
scaleAngle : Double -> Angle -> Angle
scaleAngle s a = MkAngle (s * toRadians a) Radians

||| Calculate the smallest difference between two angles
public export
angleDifference : Angle -> Angle -> Angle
angleDifference a b =
  let diff = normalizeRadiansSymmetric (toRadians a - toRadians b)
  in MkAngle diff Radians

public export
Show Angle where
  show a = show (toDegrees a) ++ "°"
