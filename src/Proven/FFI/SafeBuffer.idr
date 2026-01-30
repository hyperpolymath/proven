-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeBuffer operations (62/75)
module Proven.FFI.SafeBuffer

import Proven.SafeBuffer
import Proven.Core

%default total

encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

export
proven_idris_buffer_is_empty : Int -> Int
proven_idris_buffer_is_empty length = encodeBool (length == 0)

export
proven_idris_buffer_is_full : Int -> Int -> Int
proven_idris_buffer_is_full length capacity = encodeBool (length >= capacity)

export
proven_idris_buffer_remaining : Int -> Int -> Int
proven_idris_buffer_remaining capacity length =
  if capacity >= length then capacity - length else 0

export
proven_idris_buffer_can_write : Int -> Int -> Int -> Int
proven_idris_buffer_can_write writeCount length capacity =
  encodeBool (writeCount <= (capacity - length))

export
proven_idris_buffer_index_valid : Int -> Int -> Int
proven_idris_buffer_index_valid idx length = encodeBool (idx >= 0 && idx < length)
