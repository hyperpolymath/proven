-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeTensor operations (57/75)
module Proven.FFI.SafeTensor

import Proven.SafeTensor
import Proven.Core

%default total

encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

export
proven_idris_tensor_same_shape : Int -> Int -> Int
proven_idris_tensor_same_shape len1 len2 = encodeBool (len1 == len2)

export
proven_idris_tensor_valid_index : Int -> Int -> Int
proven_idris_tensor_valid_index idx len = encodeBool (idx >= 0 && idx < len)

export
proven_idris_tensor_matmul_compatible : Int -> Int -> Int
proven_idris_tensor_matmul_compatible cols1 rows2 = encodeBool (cols1 == rows2)
