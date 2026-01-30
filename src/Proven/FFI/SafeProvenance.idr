-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeProvenance operations (64/75)
module Proven.FFI.SafeProvenance

import Proven.SafeProvenance
import Proven.Core

%default total

encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

-- Change types: Create=0, Update=1, Delete=2, Derive=3, Transform=4, Annotate=5, Link=6
encodeChangeType : ChangeType -> Int
encodeChangeType Create = 0
encodeChangeType Update = 1
encodeChangeType Delete = 2
encodeChangeType Derive = 3
encodeChangeType Transform = 4
encodeChangeType Annotate = 5
encodeChangeType Link = 6

export
proven_idris_provenance_hash_valid : Int -> Int -> Int
proven_idris_provenance_hash_valid expectedHash computedHash =
  encodeBool (expectedHash == computedHash)

export
proven_idris_audit_trail_sealed : Int -> Int
proven_idris_audit_trail_sealed sealed = encodeBool (sealed /= 0)

export
proven_idris_tamper_detected : Int -> Int
proven_idris_tamper_detected tamperCode = encodeBool (tamperCode /= 0)

export
proven_idris_version_compare : Int -> Int -> Int
proven_idris_version_compare v1 v2 =
  if v1 < v2 then (-1)
  else if v1 > v2 then 1
  else 0
