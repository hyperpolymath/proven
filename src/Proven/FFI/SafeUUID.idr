-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeUUID operations (58/75)
module Proven.FFI.SafeUUID

import Proven.SafeUUID
import Proven.Core

%default total

encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

-- UUID version encoding: V1=1, V2=2, V3=3, V4=4, V5=5
encodeVersion : UUIDVersion -> Int
encodeVersion V1 = 1
encodeVersion V2 = 2
encodeVersion V3 = 3
encodeVersion V4 = 4
encodeVersion V5 = 5

-- UUID variant encoding: NCS=0, RFC4122=1, Microsoft=2, Future=3
encodeVariant : UUIDVariant -> Int
encodeVariant NCS = 0
encodeVariant RFC4122 = 1
encodeVariant Microsoft = 2
encodeVariant Future = 3

export
proven_idris_uuid_version_encode : Int -> Int
proven_idris_uuid_version_encode v =
  if v == 1 then 1
  else if v == 2 then 2
  else if v == 3 then 3
  else if v == 4 then 4
  else if v == 5 then 5
  else 0

export
proven_idris_uuid_variant_encode : Int -> Int
proven_idris_uuid_variant_encode v =
  if v == 0 then 0
  else if v == 1 then 1
  else if v == 2 then 2
  else if v == 3 then 3
  else 0

export
proven_idris_uuid_valid_hex_length : Int -> Int
proven_idris_uuid_valid_hex_length len = encodeBool (len == 32 || len == 36)

export
proven_idris_uuid_is_nil : String -> Int
proven_idris_uuid_is_nil hex = encodeBool (hex == "00000000000000000000000000000000")
