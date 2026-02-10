-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeFloat - Safe floating-point operations
|||
||| This module provides safe floating-point operations that handle
||| NaN, infinity, and precision issues explicitly.
module Proven.SafeFloat
import Data.String
import Data.List

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Float Classification
--------------------------------------------------------------------------------

||| Classification of floating-point values
public export
data FloatClass : Type where
  ||| Normal finite number
  Normal : FloatClass
  ||| Subnormal (denormalized) number
  Subnormal : FloatClass
  ||| Zero (positive or negative)
  Zero : FloatClass
  ||| Infinity (positive or negative)
  Infinite : FloatClass
  ||| Not a Number
  NaN : FloatClass

public export
Eq FloatClass where
  Normal == Normal = True
  Subnormal == Subnormal = True
  Zero == Zero = True
  Infinite == Infinite = True
  NaN == NaN = True
  _ == _ = False

--------------------------------------------------------------------------------
-- Safe Float Operations
--------------------------------------------------------------------------------

||| Check if a Double is NaN
public export
isNaN : Double -> Bool
isNaN x = x /= x

||| Check if a Double is infinite
public export
isInfinite : Double -> Bool
isInfinite x = x == (1.0 / 0.0) || x == (-1.0 / 0.0)

||| Check if a Double is finite (not NaN and not infinite)
public export
isFinite : Double -> Bool
isFinite x = not (isNaN x) && not (isInfinite x)

||| Sanitize a Double by replacing NaN/Inf with a default value
public export
sanitize : Double -> Double -> Double
sanitize def x = if isFinite x then x else def

||| Safe division that returns Nothing for NaN/Infinite results
public export
safeDiv : Double -> Double -> Maybe Double
safeDiv _ 0.0 = Nothing
safeDiv x y =
  let result = x / y
  in if isFinite result then Just result else Nothing

||| Safe square root (returns Nothing for negative numbers)
public export
safeSqrt : Double -> Maybe Double
safeSqrt x = if x < 0.0 then Nothing else Just (sqrt x)

||| Safe logarithm (returns Nothing for non-positive numbers)
public export
safeLog : Double -> Maybe Double
safeLog x = if x <= 0.0 then Nothing else Just (log x)

||| Compare two doubles with tolerance for floating-point errors
public export
approxEqual : (tolerance : Double) -> Double -> Double -> Bool
approxEqual tol a b = abs (a - b) <= tol

||| Default tolerance for approximate equality
public export
defaultTolerance : Double
defaultTolerance = 2.220446049250313e-14

||| Clamp a double to a range
public export
clampDouble : (lo : Double) -> (hi : Double) -> Double -> Double
clampDouble lo hi x = if x < lo then lo else if x > hi then hi else x

||| Alias for clampDouble
public export
clamp : (lo : Double) -> (hi : Double) -> Double -> Double
clamp = clampDouble

||| Clamp to unit interval [0, 1]
public export
clampUnit : Double -> Double
clampUnit = clampDouble 0.0 1.0

||| Mean of a list of Doubles
public export
mean : List Double -> Maybe Double
mean [] = Nothing
mean xs = Just (sum xs / cast (length xs))

||| Variance of a list of Doubles
public export
variance : List Double -> Maybe Double
variance [] = Nothing
variance xs = do
  m <- mean xs
  let diffs = map (\x => (x - m) * (x - m)) xs
  mean diffs

||| Standard deviation of a list of Doubles
public export
stdDev : List Double -> Maybe Double
stdDev xs = do
  v <- variance xs
  Just (sqrt v)

||| Small epsilon for numerical stability
public export
epsilon : Double
epsilon = 1.0e-15

||| Safe square root that handles negative numbers
public export
sqrt : Double -> Maybe Double
sqrt x = if x < 0.0 then Nothing else Just (Prelude.sqrt x)

||| Safe power function
public export
pow : Double -> Double -> Maybe Double
pow base exp =
  let result = Prelude.pow base exp
  in if isFinite result then Just result else Nothing

||| Safe division
public export
div : Double -> Double -> Maybe Double
div x y = if y == 0.0 then Nothing else Just (x / y)

--------------------------------------------------------------------------------
-- Rounding Operations
--------------------------------------------------------------------------------

||| Round to nearest integer
public export
roundToInt : Double -> Integer
roundToInt x = cast (floor (x + 0.5))

||| Round to specified decimal places
public export
roundTo : (places : Nat) -> Double -> Double
roundTo places x =
  let factor = pow 10.0 (cast places)
  in floor (x * factor + 0.5) / factor

||| Floor to integer
public export
floorToInt : Double -> Integer
floorToInt = cast . floor

||| Ceiling to integer
public export
ceilToInt : Double -> Integer
ceilToInt = cast . ceiling

--------------------------------------------------------------------------------
-- Safe Arithmetic
--------------------------------------------------------------------------------

||| Safe addition that checks for overflow to infinity
public export
safeAdd : Double -> Double -> Maybe Double
safeAdd x y =
  let result = x + y
  in if isFinite result then Just result else Nothing

||| Safe multiplication that checks for overflow
public export
safeMul : Double -> Double -> Maybe Double
safeMul x y =
  let result = x * y
  in if isFinite result then Just result else Nothing

||| Linear interpolation between two values
public export
lerp : (t : Double) -> (a : Double) -> (b : Double) -> Double
lerp t a b = a + t * (b - a)

||| Inverse linear interpolation
public export
invLerp : (a : Double) -> (b : Double) -> (x : Double) -> Maybe Double
invLerp a b x = if a == b then Nothing else Just ((x - a) / (b - a))
