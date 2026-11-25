-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
||| ABI word specification for the pinned 64-bit RefC Bits8 profile.
||| The Zig carrier checks the actual SDK shift; this is not a pointer lease.
module Proven.FFI.RefCBits8

%default total

public export
encodeWord : Bits8 -> Bits64
encodeWord value = cast value * 4294967296 + 1

||| A byte is returned only after checking the entire canonical word.
||| This is a word contract, not validity of an allocated pointer.
public export
decodeWord : Bits64 -> Maybe Bits8
decodeWord word =
  let payload = word `div` 4294967296 in
  if payload > 255 then Nothing else
    let byte = the Bits8 (cast payload) in
    if word == encodeWord byte then Just byte else Nothing
