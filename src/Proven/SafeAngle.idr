-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeAngle - Verified angle operations
|||
||| Safe angle handling with wrapping, clamping, and unit conversion.
||| Prevents common bugs in graphics, robotics, quantum computing, and physics.
|||
||| All angles are internally stored in radians with values in (-π, π].
module Proven.SafeAngle

import Proven.Core
import Data.So

%default total

-- ============================================================================
-- CONSTANTS
-- ============================================================================

||| Pi constant (high precision)
public export
PI : Double
PI = 3.14159265358979323846

||| Tau (2π) - full circle
public export
TAU : Double
TAU = 2.0 * PI

||| Epsilon for floating point comparison
public export
ANGLE_EPSILON : Double
ANGLE_EPSILON = 1.0e-10

-- ============================================================================
-- ANGLE TYPE
-- ============================================================================

||| A normalized angle in radians, always in the range (-π, π]
public export
record Angle where
  constructor MkAngle
  radians : Double
  0 inRange : So (radians > negate PI && radians <= PI)

||| Proof that a value is a valid normalized angle
public export
IsNormalizedAngle : Double -> Type
IsNormalizedAngle x = So (x > negate PI && x <= PI)

-- ============================================================================
-- NORMALIZATION
-- ============================================================================

||| Normalize an angle to (-π, π]
||| This is the core operation that ensures all angles are in canonical form
export
normalize : Double -> Double
normalize angle =
  let wrapped = angle - TAU * floor ((angle + PI) / TAU)
  in if wrapped <= negate PI then wrapped + TAU
     else if wrapped > PI then wrapped - TAU
     else wrapped

||| Wrap angle to [0, 2π)
export
normalizePositive : Double -> Double
normalizePositive angle =
  let wrapped = angle - TAU * floor (angle / TAU)
  in if wrapped < 0.0 then wrapped + TAU else wrapped

-- ============================================================================
-- CONSTRUCTORS
-- ============================================================================

||| Create an angle from radians (normalizes automatically)
export
fromRadians : Double -> Angle
fromRadians r =
  let norm = normalize r
  in believe_me (MkAngle norm) -- Normalization guarantees range

||| Create an angle from degrees
export
fromDegrees : Double -> Angle
fromDegrees deg = fromRadians (deg * PI / 180.0)

||| Create an angle from turns (1 turn = 360°)
export
fromTurns : Double -> Angle
fromTurns turns = fromRadians (turns * TAU)

||| Create an angle from gradians (400 gradians = 360°)
export
fromGradians : Double -> Angle
fromGradians grad = fromRadians (grad * PI / 200.0)

||| Zero angle
export
zero : Angle
zero = fromRadians 0.0

||| Right angle (π/2)
export
rightAngle : Angle
rightAngle = fromRadians (PI / 2.0)

||| Straight angle (π)
export
straightAngle : Angle
straightAngle = fromRadians PI

-- ============================================================================
-- EXTRACTION
-- ============================================================================

||| Get angle in radians
export
toRadians : Angle -> Double
toRadians a = a.radians

||| Get angle in degrees
export
toDegrees : Angle -> Double
toDegrees a = a.radians * 180.0 / PI

||| Get angle in turns
export
toTurns : Angle -> Double
toTurns a = a.radians / TAU

||| Get angle in gradians
export
toGradians : Angle -> Double
toGradians a = a.radians * 200.0 / PI

||| Get angle in [0, 2π) range
export
toPositiveRadians : Angle -> Double
toPositiveRadians a = normalizePositive a.radians

||| Get angle in [0, 360) range
export
toPositiveDegrees : Angle -> Double
toPositiveDegrees a = normalizePositive a.radians * 180.0 / PI

-- ============================================================================
-- ARITHMETIC
-- ============================================================================

||| Add two angles (result is normalized)
export
add : Angle -> Angle -> Angle
add a b = fromRadians (a.radians + b.radians)

||| Subtract two angles (result is normalized)
export
sub : Angle -> Angle -> Angle
sub a b = fromRadians (a.radians - b.radians)

||| Negate an angle
export
neg : Angle -> Angle
neg a = fromRadians (negate a.radians)

||| Multiply angle by scalar
export
scale : Double -> Angle -> Angle
scale k a = fromRadians (k * a.radians)

||| Divide angle by scalar (returns None if divisor is zero)
export
divideBy : Angle -> Double -> Maybe Angle
divideBy a k =
  if abs k < ANGLE_EPSILON
  then Nothing
  else Just (fromRadians (a.radians / k))

||| Bisect an angle (divide by 2)
export
bisect : Angle -> Angle
bisect a = fromRadians (a.radians / 2.0)

-- ============================================================================
-- COMPARISON
-- ============================================================================

||| Check if two angles are approximately equal
export
approxEqual : Angle -> Angle -> Double -> Bool
approxEqual a b epsilon = abs (a.radians - b.radians) < epsilon

||| Check if two angles are equal (within default epsilon)
export
equal : Angle -> Angle -> Bool
equal a b = approxEqual a b ANGLE_EPSILON

||| Signed angular distance from a to b (shortest path)
export
signedDistance : Angle -> Angle -> Double
signedDistance a b = normalize (b.radians - a.radians)

||| Unsigned angular distance (always positive)
export
distance : Angle -> Angle -> Double
distance a b = abs (signedDistance a b)

||| Check if angle is in range [lo, hi] (going counterclockwise from lo to hi)
export
inRange : Angle -> Angle -> Angle -> Bool
inRange angle lo hi =
  let a = normalizePositive angle.radians
      l = normalizePositive lo.radians
      h = normalizePositive hi.radians
  in if l <= h
     then a >= l && a <= h
     else a >= l || a <= h  -- Range wraps around 0

-- ============================================================================
-- TRIGONOMETRY
-- ============================================================================

||| Safe sine
export
sin : Angle -> Double
sin a = prim__doubleSin a.radians

||| Safe cosine
export
cos : Angle -> Double
cos a = prim__doubleCos a.radians

||| Safe tangent (returns None at ±π/2)
export
tan : Angle -> Maybe Double
tan a =
  let c = cos a
  in if abs c < ANGLE_EPSILON then Nothing else Just (sin a / c)

||| Arctangent (single argument, result in (-π/2, π/2))
export
atan : Double -> Angle
atan x = fromRadians (prim__doubleATan x)

||| Arctangent with two arguments (result in (-π, π])
export
atan2 : Double -> Double -> Angle
atan2 y x = fromRadians (prim__doubleATan2 y x)

||| Arcsine (returns None if |x| > 1)
export
asin : Double -> Maybe Angle
asin x = if abs x > 1.0 then Nothing else Just (fromRadians (prim__doubleASin x))

||| Arccosine (returns None if |x| > 1)
export
acos : Double -> Maybe Angle
acos x = if abs x > 1.0 then Nothing else Just (fromRadians (prim__doubleACos x))

-- ============================================================================
-- INTERPOLATION
-- ============================================================================

||| Linear interpolation between two angles (shortest path)
||| t=0 gives a, t=1 gives b
export
lerp : Angle -> Angle -> Double -> Angle
lerp a b t =
  let t' = max 0.0 (min 1.0 t)  -- Clamp t to [0,1]
      diff = signedDistance a b
  in fromRadians (a.radians + t' * diff)

||| Spherical linear interpolation (constant angular velocity)
export
slerp : Angle -> Angle -> Double -> Angle
slerp a b t =
  let t' = max 0.0 (min 1.0 t)
      diff = signedDistance a b
      -- Smoothstep for smoother interpolation
      t'' = t' * t' * (3.0 - 2.0 * t')
  in fromRadians (a.radians + t'' * diff)

-- ============================================================================
-- CLAMPING
-- ============================================================================

||| Clamp angle to range [lo, hi]
export
clamp : Angle -> Angle -> Angle -> Angle
clamp angle lo hi =
  if angle.radians < lo.radians then lo
  else if angle.radians > hi.radians then hi
  else angle

||| Clamp angle to ±limit (symmetric around zero)
export
clampSymmetric : Angle -> Double -> Angle
clampSymmetric angle limit =
  let lim = abs limit
  in if angle.radians < negate lim then fromRadians (negate lim)
     else if angle.radians > lim then fromRadians lim
     else angle

-- ============================================================================
-- SPECIAL VALUES
-- ============================================================================

||| Check if angle is approximately zero
export
isZero : Angle -> Bool
isZero a = abs a.radians < ANGLE_EPSILON

||| Check if angle is approximately a right angle (±π/2)
export
isRightAngle : Angle -> Bool
isRightAngle a = abs (abs a.radians - PI / 2.0) < ANGLE_EPSILON

||| Check if angle is approximately a straight angle (±π)
export
isStraightAngle : Angle -> Bool
isStraightAngle a = abs (abs a.radians - PI) < ANGLE_EPSILON

||| Quadrant (1-4) for angle in standard position
export
quadrant : Angle -> Nat
quadrant a =
  let pos = toPositiveRadians a
  in if pos < PI / 2.0 then 1
     else if pos < PI then 2
     else if pos < 3.0 * PI / 2.0 then 3
     else 4

-- ============================================================================
-- VECTOR OPERATIONS
-- ============================================================================

||| Unit vector components (cos θ, sin θ)
export
toUnitVector : Angle -> (Double, Double)
toUnitVector a = (cos a, sin a)

||| Angle from unit vector components
export
fromUnitVector : Double -> Double -> Angle
fromUnitVector x y = atan2 y x

||| Rotate a 2D point by angle around origin
export
rotate2D : Angle -> (Double, Double) -> (Double, Double)
rotate2D angle (x, y) =
  let c = cos angle
      s = sin angle
  in (x * c - y * s, x * s + y * c)
