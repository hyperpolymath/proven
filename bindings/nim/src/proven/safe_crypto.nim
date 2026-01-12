# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Cryptographic safety operations with constant-time guarantees.

import std/sysrand

proc constantTimeCompare*(a, b: string): bool =
  ## Compare two strings in constant time to prevent timing attacks.
  ## Both strings must be the same length for a valid comparison.
  if a.len != b.len:
    return false

  if a.len == 0 and b.len == 0:
    return true

  var result: byte = 0
  for i in 0 ..< a.len:
    result = result or (byte(a[i]) xor byte(b[i]))

  result == 0

proc constantTimeCompare*(a, b: openArray[byte]): bool =
  ## Compare two byte arrays in constant time.
  if a.len != b.len:
    return false

  if a.len == 0 and b.len == 0:
    return true

  var result: byte = 0
  for i in 0 ..< a.len:
    result = result or (a[i] xor b[i])

  result == 0

proc secureZero*(length: int): string =
  ## Create a zeroed string of the specified length.
  result = newString(length)
  for i in 0 ..< length:
    result[i] = '\0'

proc randomBytes*(n: int): seq[byte] =
  ## Generate cryptographically secure random bytes.
  result = newSeq[byte](n)
  if not urandom(result):
    raise newException(IOError, "Failed to generate random bytes")

proc randomHex*(n: int): string =
  ## Generate a cryptographically secure random hex string.
  const hexChars = "0123456789abcdef"
  let bytes = randomBytes(n)
  result = newStringOfCap(n * 2)
  for b in bytes:
    result.add hexChars[int(b shr 4)]
    result.add hexChars[int(b and 0x0F)]
