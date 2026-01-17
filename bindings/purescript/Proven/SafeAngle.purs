-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe angle operations with unit conversions.
-- |
-- | Provides validated angle handling with conversions between
-- | degrees, radians, gradians, and turns.

module Proven.SafeAngle
  ( SafeAngle
  , Angle(..)
  , AngleUnit(..)
  , fromDegrees
  , fromRadians
  , fromGradians
  , fromTurns
  , toDegrees
  , toRadians
  , toGradians
  , toTurns
  , normalize
  , add
  , subtract
  , sin
  , cos
  , tan
  , isRight
  , isStraight
  , isComplete
  ) where

import Prelude hiding (add)

import Data.Number as N
import Math as M

-- | SafeAngle namespace marker (not instantiated).
data SafeAngle

-- | Angle unit types.
data AngleUnit
  = Degrees
  | Radians
  | Gradians
  | Turns

derive instance eqAngleUnit :: Eq AngleUnit

instance showAngleUnit :: Show AngleUnit where
  show Degrees = "deg"
  show Radians = "rad"
  show Gradians = "grad"
  show Turns = "turn"

-- | Angle value (internally stored as radians for precision).
newtype Angle = Angle Number

derive instance eqAngle :: Eq Angle

instance showAngle :: Show Angle where
  show a = show (toDegrees a) <> "deg"

instance ordAngle :: Ord Angle where
  compare (Angle a) (Angle b) = compare a b

-- | Pi constant.
pi :: Number
pi = M.pi

-- | Create an angle from degrees.
fromDegrees :: Number -> Angle
fromDegrees deg = Angle (deg * pi / 180.0)

-- | Create an angle from radians.
fromRadians :: Number -> Angle
fromRadians rad = Angle rad

-- | Create an angle from gradians.
fromGradians :: Number -> Angle
fromGradians grad = Angle (grad * pi / 200.0)

-- | Create an angle from turns (1 turn = 360 degrees).
fromTurns :: Number -> Angle
fromTurns turns = Angle (turns * 2.0 * pi)

-- | Convert to degrees.
toDegrees :: Angle -> Number
toDegrees (Angle rad) = rad * 180.0 / pi

-- | Convert to radians.
toRadians :: Angle -> Number
toRadians (Angle rad) = rad

-- | Convert to gradians.
toGradians :: Angle -> Number
toGradians (Angle rad) = rad * 200.0 / pi

-- | Convert to turns.
toTurns :: Angle -> Number
toTurns (Angle rad) = rad / (2.0 * pi)

-- | Normalize angle to [0, 2pi) range.
normalize :: Angle -> Angle
normalize (Angle rad) =
  let normalized = mod' rad (2.0 * pi)
  in Angle (if normalized < 0.0 then normalized + 2.0 * pi else normalized)
  where
    mod' a b = a - b * M.floor (a / b)

-- | Add two angles.
add :: Angle -> Angle -> Angle
add (Angle a) (Angle b) = Angle (a + b)

-- | Subtract two angles.
subtract :: Angle -> Angle -> Angle
subtract (Angle a) (Angle b) = Angle (a - b)

-- | Calculate sine of angle.
sin :: Angle -> Number
sin (Angle rad) = M.sin rad

-- | Calculate cosine of angle.
cos :: Angle -> Number
cos (Angle rad) = M.cos rad

-- | Calculate tangent of angle.
tan :: Angle -> Number
tan (Angle rad) = M.tan rad

-- | Check if angle is approximately a right angle (90 degrees).
isRight :: Angle -> Boolean
isRight a =
  let deg = toDegrees (normalize a)
  in almostEqual 0.0001 deg 90.0

-- | Check if angle is approximately a straight angle (180 degrees).
isStraight :: Angle -> Boolean
isStraight a =
  let deg = toDegrees (normalize a)
  in almostEqual 0.0001 deg 180.0

-- | Check if angle is approximately a complete angle (360 degrees / 0).
isComplete :: Angle -> Boolean
isComplete a =
  let deg = toDegrees (normalize a)
  in almostEqual 0.0001 deg 0.0 || almostEqual 0.0001 deg 360.0

almostEqual :: Number -> Number -> Number -> Boolean
almostEqual tolerance x y = absNum (x - y) < tolerance

absNum :: Number -> Number
absNum n = if n < 0.0 then -n else n
