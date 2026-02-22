{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe mathematical operations via libproven FFI.
--
-- All computation is performed by the Idris 2 verified core.
-- This module only marshals arguments and results.
module Proven.SafeMath
  ( safeDiv
  , safeMod
  , safeAdd
  , safeSub
  , safeMul
  , safeAbs
  , safeClamp
  , safePow
  ) where

import Proven.FFI (c_proven_math_div, c_proven_math_mod,
                   c_proven_math_add_checked, c_proven_math_sub_checked,
                   c_proven_math_mul_checked, c_proven_math_abs_safe,
                   c_proven_math_clamp, c_proven_math_pow_checked)
import Proven.Core (intResultToMaybe)

-- | Safely divide two integers. Returns Nothing on division by zero.
-- Delegates to @proven_math_div@ in libproven.
safeDiv :: Int -> Int -> IO (Maybe Int)
safeDiv a b = intResultToMaybe <$>
  c_proven_math_div (fromIntegral a) (fromIntegral b)

-- | Safely compute modulo. Returns Nothing on division by zero.
-- Delegates to @proven_math_mod@ in libproven.
safeMod :: Int -> Int -> IO (Maybe Int)
safeMod a b = intResultToMaybe <$>
  c_proven_math_mod (fromIntegral a) (fromIntegral b)

-- | Safely add two integers with overflow detection.
-- Delegates to @proven_math_add_checked@ in libproven.
safeAdd :: Int -> Int -> IO (Maybe Int)
safeAdd a b = intResultToMaybe <$>
  c_proven_math_add_checked (fromIntegral a) (fromIntegral b)

-- | Safely subtract two integers with underflow detection.
-- Delegates to @proven_math_sub_checked@ in libproven.
safeSub :: Int -> Int -> IO (Maybe Int)
safeSub a b = intResultToMaybe <$>
  c_proven_math_sub_checked (fromIntegral a) (fromIntegral b)

-- | Safely multiply two integers with overflow detection.
-- Delegates to @proven_math_mul_checked@ in libproven.
safeMul :: Int -> Int -> IO (Maybe Int)
safeMul a b = intResultToMaybe <$>
  c_proven_math_mul_checked (fromIntegral a) (fromIntegral b)

-- | Safe absolute value. Handles MIN_INT correctly.
-- Delegates to @proven_math_abs_safe@ in libproven.
safeAbs :: Int -> IO (Maybe Int)
safeAbs n = intResultToMaybe <$>
  c_proven_math_abs_safe (fromIntegral n)

-- | Clamp a value to the range [lo, hi].
-- Delegates to @proven_math_clamp@ in libproven.
safeClamp :: Int -> Int -> Int -> IO Int
safeClamp lo hi val = fromIntegral <$>
  c_proven_math_clamp (fromIntegral lo) (fromIntegral hi) (fromIntegral val)

-- | Safe integer power with overflow checking.
-- Delegates to @proven_math_pow_checked@ in libproven.
safePow :: Int -> Word -> IO (Maybe Int)
safePow base exponent' = intResultToMaybe <$>
  c_proven_math_pow_checked (fromIntegral base) (fromIntegral exponent')
