{- SPDX-License-Identifier: Apache-2.0 -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Safe angle operations with unit conversions.
--
-- Provides secure angle handling with automatic normalization
-- and conversion between degrees, radians, gradians, and turns.
module Proven.SafeAngle
  ( -- * Types
    Angle(..)
  , AngleUnit(..)
    -- * Construction
  , degrees
  , radians
  , gradians
  , turns
  , makeAngle
    -- * Conversion
  , toDegrees
  , toRadians
  , toGradians
  , toTurns
  , convert
    -- * Normalization
  , normalize
  , normalizeSigned
    -- * Arithmetic
  , addAngles
  , subAngles
  , mulAngle
  , divAngle
  , negateAngle
    -- * Trigonometry
  , sinAngle
  , cosAngle
  , tanAngle
  , asinAngle
  , acosAngle
  , atanAngle
  , atan2Angle
    -- * Comparison
  , angleEq
  , angleLt
  , angleLte
  , angleGt
  , angleGte
    -- * Analysis
  , isAcute
  , isRight
  , isObtuse
  , isStraight
  , isReflex
    -- * Constants
  , zeroDegrees
  , rightAngle
  , straightAngle
  , fullRotation
  , pi'
  ) where

import Proven.Core (ProvenError(..), Result)

-- | Angle unit types.
data AngleUnit = Degrees | Radians | Gradians | Turns
  deriving (Eq, Show)

-- | Angle value with unit tracking.
data Angle = Angle
  { angleValue :: !Double
  , angleUnit  :: !AngleUnit
  } deriving (Eq, Show)

-- | Pi constant.
pi' :: Double
pi' = 3.14159265358979323846

-- | Create angle in degrees.
degrees :: Double -> Angle
degrees v = Angle v Degrees

-- | Create angle in radians.
radians :: Double -> Angle
radians v = Angle v Radians

-- | Create angle in gradians.
gradians :: Double -> Angle
gradians v = Angle v Gradians

-- | Create angle in turns.
turns :: Double -> Angle
turns v = Angle v Turns

-- | Create angle with explicit unit.
makeAngle :: Double -> AngleUnit -> Angle
makeAngle = Angle

-- | Convert angle to degrees.
toDegrees :: Angle -> Double
toDegrees (Angle v Degrees) = v
toDegrees (Angle v Radians) = v * 180 / pi'
toDegrees (Angle v Gradians) = v * 0.9
toDegrees (Angle v Turns) = v * 360

-- | Convert angle to radians.
toRadians :: Angle -> Double
toRadians (Angle v Degrees) = v * pi' / 180
toRadians (Angle v Radians) = v
toRadians (Angle v Gradians) = v * pi' / 200
toRadians (Angle v Turns) = v * 2 * pi'

-- | Convert angle to gradians.
toGradians :: Angle -> Double
toGradians (Angle v Degrees) = v / 0.9
toGradians (Angle v Radians) = v * 200 / pi'
toGradians (Angle v Gradians) = v
toGradians (Angle v Turns) = v * 400

-- | Convert angle to turns.
toTurns :: Angle -> Double
toTurns (Angle v Degrees) = v / 360
toTurns (Angle v Radians) = v / (2 * pi')
toTurns (Angle v Gradians) = v / 400
toTurns (Angle v Turns) = v

-- | Convert angle to specified unit.
convert :: AngleUnit -> Angle -> Angle
convert Degrees a = Angle (toDegrees a) Degrees
convert Radians a = Angle (toRadians a) Radians
convert Gradians a = Angle (toGradians a) Gradians
convert Turns a = Angle (toTurns a) Turns

-- | Normalize angle to [0, full rotation).
normalize :: Angle -> Angle
normalize a =
  let fullRot = case angleUnit a of
        Degrees -> 360
        Radians -> 2 * pi'
        Gradians -> 400
        Turns -> 1
      v = angleValue a
      normalized = v - fullRot * fromIntegral (floor (v / fullRot) :: Int)
  in Angle (if normalized < 0 then normalized + fullRot else normalized) (angleUnit a)

-- | Normalize angle to [-half rotation, half rotation).
normalizeSigned :: Angle -> Angle
normalizeSigned a =
  let fullRot = case angleUnit a of
        Degrees -> 360
        Radians -> 2 * pi'
        Gradians -> 400
        Turns -> 1
      halfRot = fullRot / 2
      norm = normalize a
      v = angleValue norm
  in Angle (if v >= halfRot then v - fullRot else v) (angleUnit a)

-- | Add two angles.
addAngles :: Angle -> Angle -> Angle
addAngles a b =
  let unit = angleUnit a
      aVal = angleValue a
      bVal = angleValue (convert unit b)
  in Angle (aVal + bVal) unit

-- | Subtract two angles.
subAngles :: Angle -> Angle -> Angle
subAngles a b =
  let unit = angleUnit a
      aVal = angleValue a
      bVal = angleValue (convert unit b)
  in Angle (aVal - bVal) unit

-- | Multiply angle by scalar.
mulAngle :: Double -> Angle -> Angle
mulAngle scalar (Angle v u) = Angle (v * scalar) u

-- | Divide angle by scalar.
divAngle :: Angle -> Double -> Result Angle
divAngle _ 0 = Left DivisionByZero
divAngle (Angle v u) scalar = Right (Angle (v / scalar) u)

-- | Negate an angle.
negateAngle :: Angle -> Angle
negateAngle (Angle v u) = Angle (-v) u

-- | Sine of angle.
sinAngle :: Angle -> Double
sinAngle = sin . toRadians

-- | Cosine of angle.
cosAngle :: Angle -> Double
cosAngle = cos . toRadians

-- | Tangent of angle.
tanAngle :: Angle -> Double
tanAngle = tan . toRadians

-- | Arcsine returning angle in radians.
asinAngle :: Double -> Result Angle
asinAngle x
  | x < -1 || x > 1 = Left (OutOfRange "asin argument must be in [-1, 1]")
  | otherwise = Right (radians (asin x))

-- | Arccosine returning angle in radians.
acosAngle :: Double -> Result Angle
acosAngle x
  | x < -1 || x > 1 = Left (OutOfRange "acos argument must be in [-1, 1]")
  | otherwise = Right (radians (acos x))

-- | Arctangent returning angle in radians.
atanAngle :: Double -> Angle
atanAngle = radians . atan

-- | Two-argument arctangent returning angle in radians.
atan2Angle :: Double -> Double -> Angle
atan2Angle y x = radians (atan2 y x)

-- | Check if two angles are equal (normalized comparison).
angleEq :: Angle -> Angle -> Bool
angleEq a b =
  let aDeg = toDegrees (normalize a)
      bDeg = toDegrees (normalize b)
  in abs (aDeg - bDeg) < 1e-10

-- | Check if first angle is less than second.
angleLt :: Angle -> Angle -> Bool
angleLt a b = toDegrees (normalize a) < toDegrees (normalize b)

-- | Check if first angle is less than or equal to second.
angleLte :: Angle -> Angle -> Bool
angleLte a b = toDegrees (normalize a) <= toDegrees (normalize b)

-- | Check if first angle is greater than second.
angleGt :: Angle -> Angle -> Bool
angleGt a b = toDegrees (normalize a) > toDegrees (normalize b)

-- | Check if first angle is greater than or equal to second.
angleGte :: Angle -> Angle -> Bool
angleGte a b = toDegrees (normalize a) >= toDegrees (normalize b)

-- | Check if angle is acute (0 < angle < 90 degrees).
isAcute :: Angle -> Bool
isAcute a = let deg = toDegrees (normalize a) in deg > 0 && deg < 90

-- | Check if angle is right (90 degrees).
isRight :: Angle -> Bool
isRight a = abs (toDegrees (normalize a) - 90) < 1e-10

-- | Check if angle is obtuse (90 < angle < 180 degrees).
isObtuse :: Angle -> Bool
isObtuse a = let deg = toDegrees (normalize a) in deg > 90 && deg < 180

-- | Check if angle is straight (180 degrees).
isStraight :: Angle -> Bool
isStraight a = abs (toDegrees (normalize a) - 180) < 1e-10

-- | Check if angle is reflex (180 < angle < 360 degrees).
isReflex :: Angle -> Bool
isReflex a = let deg = toDegrees (normalize a) in deg > 180 && deg < 360

-- | Zero angle.
zeroDegrees :: Angle
zeroDegrees = degrees 0

-- | Right angle (90 degrees).
rightAngle :: Angle
rightAngle = degrees 90

-- | Straight angle (180 degrees).
straightAngle :: Angle
straightAngle = degrees 180

-- | Full rotation (360 degrees).
fullRotation :: Angle
fullRotation = degrees 360
