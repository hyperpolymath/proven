-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafeFloat - FFI bindings to libproven floating-point operations
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafeFloat
  ( safeDiv
  , safeSqrt
  , safeLn
  , isFinite
  , isNaN
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))

-- | Safe division that returns error on division by zero (delegates to Idris 2).
foreign import safeDivImpl :: Number -> Number -> { status :: Int, value :: Number }

safeDiv :: Number -> Number -> Result Number ProvenError
safeDiv a b =
  let r = safeDivImpl a b
  in if r.status == 0 then Ok r.value else Err DivisionByZero

-- | Safe square root that returns error for negative input (delegates to Idris 2).
foreign import safeSqrtImpl :: Number -> { status :: Int, value :: Number }

safeSqrt :: Number -> Result Number ProvenError
safeSqrt x =
  let r = safeSqrtImpl x
  in if r.status == 0 then Ok r.value else Err (InvalidInput "Negative value for sqrt")

-- | Safe natural logarithm that returns error for non-positive input (delegates to Idris 2).
foreign import safeLnImpl :: Number -> { status :: Int, value :: Number }

safeLn :: Number -> Result Number ProvenError
safeLn x =
  let r = safeLnImpl x
  in if r.status == 0 then Ok r.value else Err (InvalidInput "Non-positive value for ln")

-- | Check if a number is finite (delegates to Idris 2).
foreign import isFiniteImpl :: Number -> Boolean

isFinite :: Number -> Boolean
isFinite = isFiniteImpl

-- | Check if a number is NaN (delegates to Idris 2).
foreign import isNaNImpl :: Number -> Boolean

isNaN :: Number -> Boolean
isNaN = isNaNImpl
