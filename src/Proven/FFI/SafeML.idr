-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeML operations
|||
||| This module exports machine learning safety primitives to the C ABI
||| via Idris2's RefC backend.
|||
||| Return conventions:
||| - Probability -> Double (clamped to [0,1])
||| - Maybe results -> (status: Int, value: Double/String)
||| - Bool checks -> Int (0 = false, 1 = true)
|||
||| IMPORTANT: All numerical operations use safe variants that handle
||| overflow, underflow, and division-by-zero gracefully.
module Proven.FFI.SafeML

import Proven.SafeML

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

--------------------------------------------------------------------------------
-- Probability Operations
--------------------------------------------------------------------------------

export
proven_idris_ml_mk_prob : Double -> (Int, Double)
proven_idris_ml_mk_prob x = case mkProb x of
  Nothing => (1, 0.0)
  Just (MkProb v) => (0, v)

export
proven_idris_ml_clamp_prob : Double -> Double
proven_idris_ml_clamp_prob x = probValue (clampProb x)

export
proven_idris_ml_complement : Double -> Double
proven_idris_ml_complement x = probValue (complement (clampProb x))

--------------------------------------------------------------------------------
-- Numerically Stable Functions
--------------------------------------------------------------------------------

export
proven_idris_ml_safe_log : Double -> Double
proven_idris_ml_safe_log = safeLog

export
proven_idris_ml_safe_exp : Double -> Double
proven_idris_ml_safe_exp = safeExp

export
proven_idris_ml_sigmoid : Double -> Double
proven_idris_ml_sigmoid x = probValue (sigmoid x)

export
proven_idris_ml_relu : Double -> Double
proven_idris_ml_relu = relu

export
proven_idris_ml_leaky_relu : Double -> Double -> Double
proven_idris_ml_leaky_relu alpha x = leakyRelu alpha x

export
proven_idris_ml_gelu : Double -> Double
proven_idris_ml_gelu = gelu

--------------------------------------------------------------------------------
-- Vector Operations (scalar building blocks)
-- Note: Array operations are handled at the Zig FFI layer.
-- These provide element-wise building blocks that the Zig bridge
-- uses to compute over arrays.
--------------------------------------------------------------------------------

||| Element-wise multiply-accumulate: acc + x * y
||| Used by the Zig bridge to compute dot products element by element
export
proven_idris_ml_multiply_accumulate : Double -> Double -> Double -> Double
proven_idris_ml_multiply_accumulate acc x y = acc + x * y

||| Compute the norm contribution for one element: acc + x * x
||| Used by the Zig bridge to compute L2 norms element by element
export
proven_idris_ml_norm_accumulate : Double -> Double -> Double
proven_idris_ml_norm_accumulate acc x = acc + x * x

||| Safe division for cosine similarity: dot / (normX * normY)
||| Returns (1, 0.0) if either norm is near-zero
export
proven_idris_ml_safe_cosine_div : Double -> Double -> Double -> (Int, Double)
proven_idris_ml_safe_cosine_div dotProduct normX normY =
  if normX < 1.0e-15 || normY < 1.0e-15
    then (1, 0.0)
    else (0, dotProduct / (normX * normY))

--------------------------------------------------------------------------------
-- Loss Functions (scalar for FFI simplicity)
--------------------------------------------------------------------------------

export
proven_idris_ml_binary_cross_entropy : Double -> Double -> Double
proven_idris_ml_binary_cross_entropy target pred =
  binaryCrossEntropy (clampProb target) (clampProb pred)

export
proven_idris_ml_mse_single : Double -> Double -> Double
proven_idris_ml_mse_single pred actual =
  (pred - actual) * (pred - actual)

export
proven_idris_ml_mae_single : Double -> Double -> Double
proven_idris_ml_mae_single pred actual =
  abs (pred - actual)

export
proven_idris_ml_rmse_single : Double -> Double -> Double
proven_idris_ml_rmse_single pred actual =
  sqrt ((pred - actual) * (pred - actual))
