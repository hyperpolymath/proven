-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | Bounded - Bounded value types using libproven FFI for clamping
-- |
-- | Uses SafeMath.clamp which delegates to Idris 2 via the Zig FFI layer.

module Proven.Bounded
  ( BoundedInt(..)
  , mkBoundedInt
  , getValue
  , getBounds
  , isAtMin
  , isAtMax
  ) where

import Prelude

import Proven.SafeMath (clamp)

-- | Bounded integer with min/max constraints.
-- | Uses libproven clamp for range enforcement.
newtype BoundedInt = BoundedInt
  { value :: Int
  , min :: Int
  , max :: Int
  }

derive instance eqBoundedInt :: Eq BoundedInt

instance showBoundedInt :: Show BoundedInt where
  show (BoundedInt r) = "BoundedInt { value: " <> show r.value
    <> ", min: " <> show r.min
    <> ", max: " <> show r.max <> " }"

-- | Create a bounded integer, clamped to range using libproven (delegates to Idris 2).
mkBoundedInt :: Int -> Int -> Int -> BoundedInt
mkBoundedInt minVal maxVal value = BoundedInt
  { value: clamp minVal maxVal value
  , min: minVal
  , max: maxVal
  }

-- | Get the value from a bounded integer.
getValue :: BoundedInt -> Int
getValue (BoundedInt r) = r.value

-- | Get the bounds from a bounded integer.
getBounds :: BoundedInt -> { min :: Int, max :: Int }
getBounds (BoundedInt r) = { min: r.min, max: r.max }

-- | Check if bounded integer is at minimum.
isAtMin :: BoundedInt -> Boolean
isAtMin (BoundedInt r) = r.value <= r.min

-- | Check if bounded integer is at maximum.
isAtMax :: BoundedInt -> Boolean
isAtMax (BoundedInt r) = r.value >= r.max
