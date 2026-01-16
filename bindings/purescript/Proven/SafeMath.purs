-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe math operations

module Proven.SafeMath
  ( SafeMath
  , safeAdd
  , safeSub
  , safeMul
  , safeDiv
  , safeMod
  , clamp
  , safeIntAdd
  , safeIntSub
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))
import Data.Int (toNumber, floor)
import Data.Number (isFinite)

-- | Safe math namespace (not instantiated)
data SafeMath

-- | Maximum safe integer (2^53 - 1)
maxSafeInteger :: Number
maxSafeInteger = 9007199254740991.0

-- | Minimum safe integer (-(2^53 - 1))
minSafeInteger :: Number
minSafeInteger = -9007199254740991.0

-- | Check if a number is a safe integer
isSafeInteger :: Number -> Boolean
isSafeInteger n = isFinite n && floor n == n && n >= minSafeInteger && n <= maxSafeInteger

-- | Safe addition with overflow check
safeAdd :: Number -> Number -> Result Number ProvenError
safeAdd a b =
  let result = a + b
  in if isFinite result
     then Ok result
     else Err Overflow

-- | Safe subtraction with underflow check
safeSub :: Number -> Number -> Result Number ProvenError
safeSub a b =
  let result = a - b
  in if isFinite result
     then Ok result
     else Err Underflow

-- | Safe multiplication with overflow check
safeMul :: Number -> Number -> Result Number ProvenError
safeMul a b =
  let result = a * b
  in if isFinite result
     then Ok result
     else Err Overflow

-- | Safe division with zero check
safeDiv :: Number -> Number -> Result Number ProvenError
safeDiv _ 0.0 = Err DivisionByZero
safeDiv a b =
  let result = a / b
  in if isFinite result
     then Ok result
     else Err Overflow

-- | Safe modulo with zero check
safeMod :: Int -> Int -> Result Int ProvenError
safeMod _ 0 = Err DivisionByZero
safeMod a b = Ok (mod a b)

-- | Clamp value to range
clamp :: forall a. Ord a => a -> a -> a -> a
clamp minVal maxVal value
  | value < minVal = minVal
  | value > maxVal = maxVal
  | otherwise = value

-- | Safe integer addition (53-bit safe)
safeIntAdd :: Number -> Number -> Result Number ProvenError
safeIntAdd a b
  | not (isSafeInteger a) || not (isSafeInteger b) = Err Overflow
  | otherwise =
      let result = a + b
      in if isSafeInteger result
         then Ok result
         else Err Overflow

-- | Safe integer subtraction (53-bit safe)
safeIntSub :: Number -> Number -> Result Number ProvenError
safeIntSub a b
  | not (isSafeInteger a) || not (isSafeInteger b) = Err Overflow
  | otherwise =
      let result = a - b
      in if isSafeInteger result
         then Ok result
         else Err Underflow
