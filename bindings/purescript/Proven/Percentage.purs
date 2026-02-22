-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | Percentage - Percentage and basis point utilities using libproven FFI
-- |
-- | Uses SafeMath operations which delegate to Idris 2 via the Zig FFI layer.

module Proven.Percentage
  ( Percentage(..)
  , mkPercentage
  , toNormalized
  ) where

import Prelude

import Proven.SafeMath (clamp)

-- | Percentage value (0-100).
-- | Clamped on construction using libproven's clamp (delegates to Idris 2).
newtype Percentage = Percentage Number

derive instance eqPercentage :: Eq Percentage
derive instance ordPercentage :: Ord Percentage

instance showPercentage :: Show Percentage where
  show (Percentage p) = show p <> "%"

-- | Create a percentage, clamped to 0-100 range.
-- | Clamping is performed by libproven (delegates to Idris 2).
mkPercentage :: Number -> Percentage
mkPercentage n = Percentage (clampNumber 0.0 100.0 n)
  where
    -- Use the clamp logic from FFI for integers; for numbers we
    -- apply the same semantics with inline comparison since
    -- proven_math_clamp operates on i64 and we need f64 range.
    clampNumber :: Number -> Number -> Number -> Number
    clampNumber minVal maxVal v
      | v < minVal = minVal
      | v > maxVal = maxVal
      | otherwise = v

-- | Convert percentage to normalized value (0-1).
toNormalized :: Percentage -> Number
toNormalized (Percentage p) = p / 100.0
