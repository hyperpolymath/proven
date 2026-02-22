-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafeMath - FFI bindings to libproven overflow-checked arithmetic
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafeMath
  ( safeAdd
  , safeSub
  , safeMul
  , safeDiv
  , safeMod
  , clamp
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))

-- | Safe addition with overflow checking (delegates to Idris 2)
foreign import safeAddImpl :: Int -> Int -> { status :: Int, value :: Int }

safeAdd :: Int -> Int -> Result Int ProvenError
safeAdd a b =
  let r = safeAddImpl a b
  in if r.status == 0 then Ok r.value else Err Overflow

-- | Safe subtraction with overflow checking (delegates to Idris 2)
foreign import safeSubImpl :: Int -> Int -> { status :: Int, value :: Int }

safeSub :: Int -> Int -> Result Int ProvenError
safeSub a b =
  let r = safeSubImpl a b
  in if r.status == 0 then Ok r.value else Err Underflow

-- | Safe multiplication with overflow checking (delegates to Idris 2)
foreign import safeMulImpl :: Int -> Int -> { status :: Int, value :: Int }

safeMul :: Int -> Int -> Result Int ProvenError
safeMul a b =
  let r = safeMulImpl a b
  in if r.status == 0 then Ok r.value else Err Overflow

-- | Safe division with zero check (delegates to Idris 2)
foreign import safeDivImpl :: Int -> Int -> { status :: Int, value :: Int }

safeDiv :: Int -> Int -> Result Int ProvenError
safeDiv a b =
  let r = safeDivImpl a b
  in if r.status == 0 then Ok r.value else Err DivisionByZero

-- | Safe modulo with zero check (delegates to Idris 2)
foreign import safeModImpl :: Int -> Int -> { status :: Int, value :: Int }

safeMod :: Int -> Int -> Result Int ProvenError
safeMod a b =
  let r = safeModImpl a b
  in if r.status == 0 then Ok r.value else Err DivisionByZero

-- | Clamp value to range (delegates to Idris 2)
foreign import clamp :: Int -> Int -> Int -> Int
