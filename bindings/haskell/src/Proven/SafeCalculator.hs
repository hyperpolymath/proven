{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe expression evaluation via libproven FFI.
--
-- Expression parsing and evaluation are performed by the
-- Idris 2 verified core.
module Proven.SafeCalculator
  ( evalExpression
  ) where

import Proven.FFI (c_proven_calculator_eval)
import Proven.Core (withCStringLen', floatResultToMaybe)

-- | Evaluate a mathematical expression string.
-- Delegates to @proven_calculator_eval@ in libproven.
evalExpression :: String -> IO (Maybe Double)
evalExpression expr = withCStringLen' expr $ \ptr len ->
  floatResultToMaybe <$> c_proven_calculator_eval ptr len
