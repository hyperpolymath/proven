# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe hexadecimal encoding and decoding.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

type
  HexError* = object of CatchableError
    ## Error raised for hex operations.

  InvalidHexError* = object of HexError
    ## Error raised for invalid hex input.

proc encode*(data: openArray[byte]): string =
  ## Encode bytes to lowercase hex string via libproven.
  if data.len == 0:
    return ""
  let res = provenHexEncode(unsafeAddr data[0], csize_t(data.len), false)
  if res.status != PROVEN_OK or res.value == nil:
    return ""
  result = $res.value
  provenFreeString(res.value)

proc encodeUpper*(data: openArray[byte]): string =
  ## Encode bytes to uppercase hex string via libproven.
  if data.len == 0:
    return ""
  let res = provenHexEncode(unsafeAddr data[0], csize_t(data.len), true)
  if res.status != PROVEN_OK or res.value == nil:
    return ""
  result = $res.value
  provenFreeString(res.value)

proc decode*(hexString: string): Option[seq[byte]] =
  ## Decode hex string to bytes via libproven.
  ## Returns None on invalid input.
  if hexString.len == 0:
    return some(newSeq[byte](0))
  var res = provenHexDecode(unsafeAddr hexString[0], csize_t(hexString.len))
  if res.status != PROVEN_OK or res.data == nil:
    return none(seq[byte])
  var resultBytes = newSeq[byte](int(res.length))
  if res.length > 0:
    copyMem(addr resultBytes[0], res.data, int(res.length))
  provenHexFree(addr res)
  some(resultBytes)

proc decodeStrict*(hexString: string): seq[byte] {.raises: [InvalidHexError].} =
  ## Decode hex string to bytes via libproven.
  ## Raises InvalidHexError on invalid input.
  let decoded = decode(hexString)
  if decoded.isNone:
    raise newException(InvalidHexError, "Invalid hex string: " & hexString)
  decoded.get

proc isValidHex*(hexString: string): bool =
  ## Check if string is valid hex by attempting a decode via libproven.
  if hexString.len == 0:
    return true
  if hexString.len mod 2 != 0:
    return false
  var res = provenHexDecode(unsafeAddr hexString[0], csize_t(hexString.len))
  if res.status == PROVEN_OK:
    provenHexFree(addr res)
    true
  else:
    false

proc isValidHexBytes*(hexString: string): bool =
  ## Check if string is valid hex with even length (complete bytes).
  isValidHex(hexString)

proc constantTimeEqual*(a, b: string): bool =
  ## Compare two hex strings in constant time via libproven's
  ## proven_crypto_constant_time_eq to prevent timing attacks.
  if a.len == 0 and b.len == 0:
    return true
  if a.len == 0 or b.len == 0:
    # One empty, one not: use FFI (it handles length mismatch)
    var dummyA, dummyB: uint8
    let ptrA = if a.len > 0: cast[pointer](unsafeAddr a[0]) else: addr dummyA
    let ptrB = if b.len > 0: cast[pointer](unsafeAddr b[0]) else: addr dummyB
    let res = provenCryptoConstantTimeEq(ptrA, csize_t(a.len), ptrB, csize_t(b.len))
    return res.status == PROVEN_OK and res.value
  let res = provenCryptoConstantTimeEq(
    unsafeAddr a[0], csize_t(a.len),
    unsafeAddr b[0], csize_t(b.len))
  res.status == PROVEN_OK and res.value

proc constantTimeEqualBytes*(a, b: openArray[byte]): bool =
  ## Compare two byte arrays in constant time via libproven's
  ## proven_crypto_constant_time_eq to prevent timing attacks.
  if a.len == 0 and b.len == 0:
    return true
  if a.len == 0 or b.len == 0:
    var dummyA, dummyB: uint8
    let ptrA = if a.len > 0: cast[pointer](unsafeAddr a[0]) else: addr dummyA
    let ptrB = if b.len > 0: cast[pointer](unsafeAddr b[0]) else: addr dummyB
    let res = provenCryptoConstantTimeEq(ptrA, csize_t(a.len), ptrB, csize_t(b.len))
    return res.status == PROVEN_OK and res.value
  let res = provenCryptoConstantTimeEq(
    unsafeAddr a[0], csize_t(a.len),
    unsafeAddr b[0], csize_t(b.len))
  res.status == PROVEN_OK and res.value
