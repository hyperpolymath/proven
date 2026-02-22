# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Cryptographic safety operations with constant-time guarantees.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

proc constantTimeCompare*(a, b: string): bool =
  ## Compare two strings in constant time to prevent timing attacks.
  if a.len == 0 and b.len == 0:
    return true
  let ptrA = if a.len > 0: unsafeAddr a[0] else: nil
  let ptrB = if b.len > 0: unsafeAddr b[0] else: nil
  let res = provenCryptoConstantTimeEq(ptrA, csize_t(a.len), ptrB, csize_t(b.len))
  if res.status == PROVEN_OK:
    return res.value
  false

proc constantTimeCompare*(a, b: openArray[byte]): bool =
  ## Compare two byte arrays in constant time.
  if a.len == 0 and b.len == 0:
    return true
  let ptrA = if a.len > 0: unsafeAddr a[0] else: nil
  let ptrB = if b.len > 0: unsafeAddr b[0] else: nil
  let res = provenCryptoConstantTimeEq(ptrA, csize_t(a.len), ptrB, csize_t(b.len))
  if res.status == PROVEN_OK:
    return res.value
  false

proc randomBytes*(n: int): Option[seq[byte]] =
  ## Generate cryptographically secure random bytes.
  ## Returns None if the system RNG fails.
  if n <= 0:
    return some(newSeq[byte](0))
  var buf = newSeq[byte](n)
  let status = provenCryptoRandomBytes(addr buf[0], csize_t(n))
  if status == PROVEN_OK:
    return some(buf)
  none(seq[byte])

proc randomHex*(n: int): Option[string] =
  ## Generate a cryptographically secure random hex string.
  ## Returns None if the system RNG fails.
  let bytes = randomBytes(n)
  if bytes.isNone:
    return none(string)
  const hexChars = "0123456789abcdef"
  var hex = newStringOfCap(n * 2)
  for b in bytes.get:
    hex.add hexChars[int(b shr 4)]
    hex.add hexChars[int(b and 0x0F)]
  some(hex)
