{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe machine learning utility functions via libproven FFI.
--
-- Activation functions are performed by the Idris 2 verified core.
module Proven.SafeML
  ( sigmoid
  , relu
  , leakyRelu
  , mlClamp
  ) where

import Proven.FFI (c_proven_ml_sigmoid, c_proven_ml_relu,
                   c_proven_ml_leaky_relu, c_proven_ml_clamp)

-- | Sigmoid activation function: 1 / (1 + exp(-x)).
-- Delegates to @proven_ml_sigmoid@ in libproven.
sigmoid :: Double -> IO Double
sigmoid x = realToFrac <$> c_proven_ml_sigmoid (realToFrac x)

-- | ReLU activation function: max(0, x).
-- Delegates to @proven_ml_relu@ in libproven.
relu :: Double -> IO Double
relu x = realToFrac <$> c_proven_ml_relu (realToFrac x)

-- | Leaky ReLU: max(alpha*x, x).
-- Delegates to @proven_ml_leaky_relu@ in libproven.
leakyRelu :: Double -> Double -> IO Double
leakyRelu x alpha = realToFrac <$>
  c_proven_ml_leaky_relu (realToFrac x) (realToFrac alpha)

-- | Clamp value to range [minVal, maxVal].
-- Delegates to @proven_ml_clamp@ in libproven.
mlClamp :: Double -> Double -> Double -> IO Double
mlClamp x minVal maxVal = realToFrac <$>
  c_proven_ml_clamp (realToFrac x) (realToFrac minVal) (realToFrac maxVal)
