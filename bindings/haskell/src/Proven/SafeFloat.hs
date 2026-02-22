{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe floating-point operations via libproven FFI.
--
-- Division, sqrt, ln, and NaN/infinity detection are performed by the
-- Idris 2 verified core.
module Proven.SafeFloat
  ( safeFloatDiv
  , isFinite
  , isNaN'
  , safeSqrt
  , safeLn
  ) where

import Proven.FFI (c_proven_float_div, c_proven_float_is_finite,
                   c_proven_float_is_nan, c_proven_float_sqrt, c_proven_float_ln)
import Proven.Core (floatResultToMaybe)

-- | Safe floating-point division. Returns Nothing on division by zero.
-- Delegates to @proven_float_div@ in libproven.
safeFloatDiv :: Double -> Double -> IO (Maybe Double)
safeFloatDiv a b = floatResultToMaybe <$>
  c_proven_float_div (realToFrac a) (realToFrac b)

-- | Check if a float is finite (not NaN or infinity).
-- Delegates to @proven_float_is_finite@ in libproven.
isFinite :: Double -> IO Bool
isFinite x = do
  result <- c_proven_float_is_finite (realToFrac x)
  return (result /= 0)

-- | Check if a float is NaN.
-- Delegates to @proven_float_is_nan@ in libproven.
isNaN' :: Double -> IO Bool
isNaN' x = do
  result <- c_proven_float_is_nan (realToFrac x)
  return (result /= 0)

-- | Safe square root. Returns Nothing for negative values.
-- Delegates to @proven_float_sqrt@ in libproven.
safeSqrt :: Double -> IO (Maybe Double)
safeSqrt x = floatResultToMaybe <$>
  c_proven_float_sqrt (realToFrac x)

-- | Safe natural logarithm. Returns Nothing for non-positive values.
-- Delegates to @proven_float_ln@ in libproven.
safeLn :: Double -> IO (Maybe Double)
safeLn x = floatResultToMaybe <$>
  c_proven_float_ln (realToFrac x)
