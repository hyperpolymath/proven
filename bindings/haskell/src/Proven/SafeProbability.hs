{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe probability operations via libproven FFI.
--
-- Probability values are clamped to [0, 1] by the Idris 2 verified core.
module Proven.SafeProbability
  ( createProbability
  , probabilityAnd
  , probabilityOrExclusive
  , probabilityNot
  ) where

import Proven.FFI (c_proven_probability_create, c_proven_probability_and,
                   c_proven_probability_or_exclusive, c_proven_probability_not)

-- | Create a probability value (clamped to [0, 1]).
-- Delegates to @proven_probability_create@ in libproven.
createProbability :: Double -> IO Double
createProbability v = realToFrac <$> c_proven_probability_create (realToFrac v)

-- | Multiply probabilities (independent events: P(A and B) = P(A) * P(B)).
-- Delegates to @proven_probability_and@ in libproven.
probabilityAnd :: Double -> Double -> IO Double
probabilityAnd a b = realToFrac <$>
  c_proven_probability_and (realToFrac a) (realToFrac b)

-- | Add probabilities (mutually exclusive: P(A or B) = P(A) + P(B)).
-- Delegates to @proven_probability_or_exclusive@ in libproven.
probabilityOrExclusive :: Double -> Double -> IO Double
probabilityOrExclusive a b = realToFrac <$>
  c_proven_probability_or_exclusive (realToFrac a) (realToFrac b)

-- | Complement probability (P(not A) = 1 - P(A)).
-- Delegates to @proven_probability_not@ in libproven.
probabilityNot :: Double -> IO Double
probabilityNot p = realToFrac <$> c_proven_probability_not (realToFrac p)
