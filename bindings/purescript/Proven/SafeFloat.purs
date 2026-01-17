-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe floating-point operations.
-- |
-- | Provides safe handling of floating-point values with proper
-- | handling of special values (NaN, Infinity) and precision.

module Proven.SafeFloat
  ( SafeFloat
  , isFinite
  , isNaN
  , isInfinite
  , isPositiveInfinity
  , isNegativeInfinity
  , safeAdd
  , safeSub
  , safeMul
  , safeDiv
  , clamp
  , roundTo
  , truncateTo
  , almostEqual
  , epsilon
  ) where

import Prelude

import Data.Number (isFinite, isNaN) as N
import Proven.Result (Result(..), ProvenError(..))

-- | SafeFloat namespace marker (not instantiated).
data SafeFloat

-- | Machine epsilon for double precision.
epsilon :: Number
epsilon = 2.220446049250313e-16

-- | Check if a number is finite (not NaN or Infinity).
isFinite :: Number -> Boolean
isFinite = N.isFinite

-- | Check if a number is NaN.
isNaN :: Number -> Boolean
isNaN = N.isNaN

-- | Check if a number is infinite (positive or negative).
isInfinite :: Number -> Boolean
isInfinite n = not (isFinite n) && not (isNaN n)

-- | Check if a number is positive infinity.
isPositiveInfinity :: Number -> Boolean
isPositiveInfinity n = isInfinite n && n > 0.0

-- | Check if a number is negative infinity.
isNegativeInfinity :: Number -> Boolean
isNegativeInfinity n = isInfinite n && n < 0.0

-- | Safe addition that returns error on overflow.
safeAdd :: Number -> Number -> Result Number ProvenError
safeAdd a b =
  let result = a + b
  in if isFinite result
     then Ok result
     else Err Overflow

-- | Safe subtraction that returns error on underflow.
safeSub :: Number -> Number -> Result Number ProvenError
safeSub a b =
  let result = a - b
  in if isFinite result
     then Ok result
     else Err Underflow

-- | Safe multiplication that returns error on overflow.
safeMul :: Number -> Number -> Result Number ProvenError
safeMul a b =
  let result = a * b
  in if isFinite result
     then Ok result
     else Err Overflow

-- | Safe division that returns error on division by zero or overflow.
safeDiv :: Number -> Number -> Result Number ProvenError
safeDiv _ 0.0 = Err DivisionByZero
safeDiv a b =
  let result = a / b
  in if isFinite result
     then Ok result
     else Err Overflow

-- | Clamp a number to a range.
clamp :: Number -> Number -> Number -> Number
clamp minVal maxVal value
  | value < minVal = minVal
  | value > maxVal = maxVal
  | otherwise = value

-- | Round a number to a specified number of decimal places.
roundTo :: Int -> Number -> Number
roundTo places n =
  let factor = pow 10.0 places
  in roundImpl (n * factor) / factor

foreign import roundImpl :: Number -> Number
foreign import pow :: Number -> Int -> Number

-- | Truncate a number to a specified number of decimal places.
truncateTo :: Int -> Number -> Number
truncateTo places n =
  let factor = pow 10.0 places
  in truncImpl (n * factor) / factor

foreign import truncImpl :: Number -> Number

-- | Check if two numbers are almost equal within a tolerance.
almostEqual :: Number -> Number -> Number -> Boolean
almostEqual tolerance a b = absImpl (a - b) <= tolerance

foreign import absImpl :: Number -> Number
