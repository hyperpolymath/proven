{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Safe floating-point operations.
--
-- Provides safe handling of floating-point numbers with
-- protection against NaN, infinity, and precision issues.
module Proven.SafeFloat
  ( -- * Types
    SafeFloat(..)
    -- * Construction
  , safeFloat
  , safeFloatStrict
  , fromDouble
  , fromRational'
    -- * Validation
  , isFinite
  , isInfinite'
  , isNaN'
  , isNormal
  , isSubnormal
  , isZero
  , isPositive
  , isNegative
    -- * Arithmetic
  , safeFloatAdd
  , safeFloatSub
  , safeFloatMul
  , safeFloatDiv
  , safeFloatAbs
  , safeFloatNegate
  , safeFloatRecip
    -- * Comparison
  , safeFloatEq
  , safeFloatLt
  , safeFloatLte
  , safeFloatGt
  , safeFloatGte
  , almostEqual
    -- * Rounding
  , safeFloor
  , safeCeil
  , safeRound
  , safeTrunc
    -- * Clamping
  , clamp
  , clampPositive
  , clampNegative
  , clampUnit
    -- * Conversion
  , toDouble
  , toInt
  , toRational'
    -- * Constants
  , epsilon
  , maxFinite
  , minPositive
  , infinity
  , negInfinity
  , nan
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Proven.Core (ProvenError(..), Result)

-- | Safe float wrapper that rejects NaN by default.
newtype SafeFloat = SafeFloat { unSafeFloat :: Double }
  deriving (Eq, Ord, Show)

-- | Create a safe float, rejecting NaN but allowing infinity.
safeFloat :: Double -> Result SafeFloat
safeFloat d
  | isNaN d = Left (InvalidInput "NaN not allowed")
  | otherwise = Right (SafeFloat d)

-- | Create a safe float, rejecting NaN and infinity.
safeFloatStrict :: Double -> Result SafeFloat
safeFloatStrict d
  | isNaN d = Left (InvalidInput "NaN not allowed")
  | isInfinite d = Left (InvalidInput "Infinity not allowed")
  | otherwise = Right (SafeFloat d)

-- | Create from Double.
fromDouble :: Double -> Maybe SafeFloat
fromDouble d = case safeFloat d of
  Right sf -> Just sf
  Left _ -> Nothing

-- | Create from Rational.
fromRational' :: Rational -> SafeFloat
fromRational' = SafeFloat . fromRational

-- | Check if value is finite.
isFinite :: SafeFloat -> Bool
isFinite (SafeFloat d) = not (isNaN d) && not (isInfinite d)

-- | Check if value is infinite.
isInfinite' :: SafeFloat -> Bool
isInfinite' (SafeFloat d) = isInfinite d

-- | Check if value is NaN.
isNaN' :: SafeFloat -> Bool
isNaN' (SafeFloat d) = isNaN d

-- | Check if value is normal (not subnormal, zero, infinite, or NaN).
isNormal :: SafeFloat -> Bool
isNormal (SafeFloat d) = not (isNaN d) && not (isInfinite d) &&
                         not (isDenormalized d) && d /= 0

-- | Check if value is subnormal (denormalized).
isSubnormal :: SafeFloat -> Bool
isSubnormal (SafeFloat d) = isDenormalized d

-- | Check if value is zero.
isZero :: SafeFloat -> Bool
isZero (SafeFloat d) = d == 0

-- | Check if value is positive.
isPositive :: SafeFloat -> Bool
isPositive (SafeFloat d) = d > 0

-- | Check if value is negative.
isNegative :: SafeFloat -> Bool
isNegative (SafeFloat d) = d < 0

-- | Safe addition.
safeFloatAdd :: SafeFloat -> SafeFloat -> Result SafeFloat
safeFloatAdd (SafeFloat a) (SafeFloat b) = safeFloat (a + b)

-- | Safe subtraction.
safeFloatSub :: SafeFloat -> SafeFloat -> Result SafeFloat
safeFloatSub (SafeFloat a) (SafeFloat b) = safeFloat (a - b)

-- | Safe multiplication.
safeFloatMul :: SafeFloat -> SafeFloat -> Result SafeFloat
safeFloatMul (SafeFloat a) (SafeFloat b) = safeFloat (a * b)

-- | Safe division.
safeFloatDiv :: SafeFloat -> SafeFloat -> Result SafeFloat
safeFloatDiv _ (SafeFloat 0) = Left DivisionByZero
safeFloatDiv (SafeFloat a) (SafeFloat b) = safeFloat (a / b)

-- | Safe absolute value.
safeFloatAbs :: SafeFloat -> SafeFloat
safeFloatAbs (SafeFloat d) = SafeFloat (abs d)

-- | Safe negation.
safeFloatNegate :: SafeFloat -> SafeFloat
safeFloatNegate (SafeFloat d) = SafeFloat (negate d)

-- | Safe reciprocal.
safeFloatRecip :: SafeFloat -> Result SafeFloat
safeFloatRecip (SafeFloat 0) = Left DivisionByZero
safeFloatRecip (SafeFloat d) = safeFloat (1 / d)

-- | Safe equality comparison.
safeFloatEq :: SafeFloat -> SafeFloat -> Bool
safeFloatEq (SafeFloat a) (SafeFloat b) = a == b

-- | Safe less-than comparison.
safeFloatLt :: SafeFloat -> SafeFloat -> Bool
safeFloatLt (SafeFloat a) (SafeFloat b) = a < b

-- | Safe less-than-or-equal comparison.
safeFloatLte :: SafeFloat -> SafeFloat -> Bool
safeFloatLte (SafeFloat a) (SafeFloat b) = a <= b

-- | Safe greater-than comparison.
safeFloatGt :: SafeFloat -> SafeFloat -> Bool
safeFloatGt (SafeFloat a) (SafeFloat b) = a > b

-- | Safe greater-than-or-equal comparison.
safeFloatGte :: SafeFloat -> SafeFloat -> Bool
safeFloatGte (SafeFloat a) (SafeFloat b) = a >= b

-- | Check if two floats are almost equal within epsilon.
almostEqual :: Double -> SafeFloat -> SafeFloat -> Bool
almostEqual eps (SafeFloat a) (SafeFloat b) = abs (a - b) <= eps

-- | Safe floor operation.
safeFloor :: SafeFloat -> Result Integer
safeFloor sf@(SafeFloat d)
  | not (isFinite sf) = Left (InvalidInput "Cannot floor non-finite value")
  | otherwise = Right (floor d)

-- | Safe ceiling operation.
safeCeil :: SafeFloat -> Result Integer
safeCeil sf@(SafeFloat d)
  | not (isFinite sf) = Left (InvalidInput "Cannot ceil non-finite value")
  | otherwise = Right (ceiling d)

-- | Safe round operation.
safeRound :: SafeFloat -> Result Integer
safeRound sf@(SafeFloat d)
  | not (isFinite sf) = Left (InvalidInput "Cannot round non-finite value")
  | otherwise = Right (round d)

-- | Safe truncation.
safeTrunc :: SafeFloat -> Result Integer
safeTrunc sf@(SafeFloat d)
  | not (isFinite sf) = Left (InvalidInput "Cannot truncate non-finite value")
  | otherwise = Right (truncate d)

-- | Clamp value to range.
clamp :: SafeFloat -> SafeFloat -> SafeFloat -> SafeFloat
clamp (SafeFloat minVal) (SafeFloat maxVal) (SafeFloat val) =
  SafeFloat (max minVal (min maxVal val))

-- | Clamp to positive values.
clampPositive :: SafeFloat -> SafeFloat
clampPositive (SafeFloat d) = SafeFloat (max 0 d)

-- | Clamp to negative values.
clampNegative :: SafeFloat -> SafeFloat
clampNegative (SafeFloat d) = SafeFloat (min 0 d)

-- | Clamp to unit interval [0, 1].
clampUnit :: SafeFloat -> SafeFloat
clampUnit (SafeFloat d) = SafeFloat (max 0 (min 1 d))

-- | Convert to Double.
toDouble :: SafeFloat -> Double
toDouble = unSafeFloat

-- | Convert to Int (truncating).
toInt :: SafeFloat -> Maybe Int
toInt sf@(SafeFloat d)
  | not (isFinite sf) = Nothing
  | d > fromIntegral (maxBound :: Int) = Nothing
  | d < fromIntegral (minBound :: Int) = Nothing
  | otherwise = Just (truncate d)

-- | Convert to Rational.
toRational' :: SafeFloat -> Maybe Rational
toRational' sf
  | isFinite sf = Just (toRational (unSafeFloat sf))
  | otherwise = Nothing

-- | Machine epsilon.
epsilon :: SafeFloat
epsilon = SafeFloat 2.220446049250313e-16

-- | Maximum finite double.
maxFinite :: SafeFloat
maxFinite = SafeFloat 1.7976931348623157e308

-- | Minimum positive double.
minPositive :: SafeFloat
minPositive = SafeFloat 2.2250738585072014e-308

-- | Positive infinity.
infinity :: Double
infinity = 1 / 0

-- | Negative infinity.
negInfinity :: Double
negInfinity = -1 / 0

-- | Not a number.
nan :: Double
nan = 0 / 0

-- Helper to check for denormalized numbers
isDenormalized :: Double -> Bool
isDenormalized d = abs d > 0 && abs d < 2.2250738585072014e-308
