# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe hexadecimal encoding and decoding with constant-time operations.

import std/[options, strutils]

type
  HexError* = object of CatchableError
    ## Error raised for hex operations.

  InvalidHexError* = object of HexError
    ## Error raised for invalid hex input.

const
  HexCharsLower = "0123456789abcdef"
  HexCharsUpper = "0123456789ABCDEF"

proc isHexChar*(c: char): bool =
  ## Check if character is a valid hex digit.
  c in {'0'..'9', 'a'..'f', 'A'..'F'}

proc hexCharToNibble*(c: char): Option[byte] =
  ## Convert hex character to nibble value (0-15).
  case c
  of '0'..'9': some(byte(ord(c) - ord('0')))
  of 'a'..'f': some(byte(ord(c) - ord('a') + 10))
  of 'A'..'F': some(byte(ord(c) - ord('A') + 10))
  else: none(byte)

proc nibbleToHexChar*(nibble: byte): char =
  ## Convert nibble (0-15) to lowercase hex character.
  HexCharsLower[int(nibble and 0x0F)]

proc nibbleToHexCharUpper*(nibble: byte): char =
  ## Convert nibble (0-15) to uppercase hex character.
  HexCharsUpper[int(nibble and 0x0F)]

proc encode*(data: openArray[byte]): string =
  ## Encode bytes to lowercase hex string.
  result = newStringOfCap(data.len * 2)
  for b in data:
    result.add nibbleToHexChar(b shr 4)
    result.add nibbleToHexChar(b and 0x0F)

proc encodeUpper*(data: openArray[byte]): string =
  ## Encode bytes to uppercase hex string.
  result = newStringOfCap(data.len * 2)
  for b in data:
    result.add nibbleToHexCharUpper(b shr 4)
    result.add nibbleToHexCharUpper(b and 0x0F)

proc decode*(hexString: string): Option[seq[byte]] =
  ## Decode hex string to bytes. Returns None on invalid input.
  if hexString.len mod 2 != 0:
    return none(seq[byte])

  var resultBytes = newSeq[byte](hexString.len div 2)

  for i in 0..<resultBytes.len:
    let highNibble = hexCharToNibble(hexString[i * 2])
    let lowNibble = hexCharToNibble(hexString[i * 2 + 1])

    if highNibble.isNone or lowNibble.isNone:
      return none(seq[byte])

    resultBytes[i] = (highNibble.get shl 4) or lowNibble.get

  result = some(resultBytes)

proc decodeStrict*(hexString: string): seq[byte] {.raises: [InvalidHexError].} =
  ## Decode hex string to bytes. Raises on invalid input.
  let decoded = decode(hexString)
  if decoded.isNone:
    raise newException(InvalidHexError, "Invalid hex string: " & hexString)
  result = decoded.get

proc isValidHex*(hexString: string): bool =
  ## Check if string contains only valid hex characters.
  for c in hexString:
    if not isHexChar(c):
      return false
  result = true

proc isValidHexBytes*(hexString: string): bool =
  ## Check if string is valid hex with even length (complete bytes).
  hexString.len mod 2 == 0 and isValidHex(hexString)

proc constantTimeEqual*(a, b: string): bool =
  ## Compare two hex strings in constant time to prevent timing attacks.
  ## Case-insensitive comparison.
  if a.len != b.len:
    return false

  if a.len == 0 and b.len == 0:
    return true

  var diff: byte = 0
  for i in 0..<a.len:
    let charA = a[i].toLowerAscii
    let charB = b[i].toLowerAscii
    diff = diff or (byte(ord(charA)) xor byte(ord(charB)))

  result = diff == 0

proc constantTimeEqualBytes*(a, b: openArray[byte]): bool =
  ## Compare two byte arrays in constant time to prevent timing attacks.
  if a.len != b.len:
    return false

  if a.len == 0 and b.len == 0:
    return true

  var diff: byte = 0
  for i in 0..<a.len:
    diff = diff or (a[i] xor b[i])

  result = diff == 0

proc formatSpaced*(hexString: string): Option[string] =
  ## Format hex string with spaces between each byte (aa bb cc).
  if hexString.len mod 2 != 0:
    return none(string)

  if hexString.len == 0:
    return some("")

  var resultStr = newStringOfCap(hexString.len + hexString.len div 2 - 1)
  var first = true

  for i in countup(0, hexString.len - 2, 2):
    if not first:
      resultStr.add ' '
    first = false
    resultStr.add hexString[i]
    resultStr.add hexString[i + 1]

  result = some(resultStr)

proc formatColons*(hexString: string): Option[string] =
  ## Format hex string with colons between each byte (aa:bb:cc).
  if hexString.len mod 2 != 0:
    return none(string)

  if hexString.len == 0:
    return some("")

  var resultStr = newStringOfCap(hexString.len + hexString.len div 2 - 1)
  var first = true

  for i in countup(0, hexString.len - 2, 2):
    if not first:
      resultStr.add ':'
    first = false
    resultStr.add hexString[i]
    resultStr.add hexString[i + 1]

  result = some(resultStr)

proc stripSpaces*(hexString: string): string =
  ## Remove spaces, colons, and dashes from hex string.
  result = newStringOfCap(hexString.len)
  for c in hexString:
    if c notin {' ', ':', '-'}:
      result.add c

proc intToHex*(value: uint64, minWidth: int = 0): string =
  ## Convert integer to hex string with optional minimum width.
  result = value.toHex.strip(leading = true, chars = {'0'})
  if result.len == 0:
    result = "0"

  # Pad with leading zeros if needed
  if result.len < minWidth:
    result = repeat('0', minWidth - result.len) & result

  result = result.toLowerAscii

proc intToHexUpper*(value: uint64, minWidth: int = 0): string =
  ## Convert integer to uppercase hex string with optional minimum width.
  result = value.toHex.strip(leading = true, chars = {'0'})
  if result.len == 0:
    result = "0"

  # Pad with leading zeros if needed
  if result.len < minWidth:
    result = repeat('0', minWidth - result.len) & result

proc hexToInt*(hexString: string): Option[uint64] =
  ## Parse hex string to integer.
  if hexString.len == 0:
    return none(uint64)

  if hexString.len > 16:  # Max 64-bit value is 16 hex digits
    return none(uint64)

  try:
    result = some(fromHex[uint64](hexString))
  except ValueError:
    result = none(uint64)

proc hexToIntStrict*(hexString: string): uint64 {.raises: [InvalidHexError].} =
  ## Parse hex string to integer. Raises on invalid input.
  let parsed = hexToInt(hexString)
  if parsed.isNone:
    raise newException(InvalidHexError, "Invalid hex integer: " & hexString)
  result = parsed.get

proc xorHex*(a, b: string): Option[string] =
  ## XOR two hex strings of equal length.
  if a.len != b.len or a.len mod 2 != 0:
    return none(string)

  let aBytes = decode(a)
  let bBytes = decode(b)

  if aBytes.isNone or bBytes.isNone:
    return none(string)

  var resultBytes = newSeq[byte](aBytes.get.len)
  for i in 0..<resultBytes.len:
    resultBytes[i] = aBytes.get[i] xor bBytes.get[i]

  result = some(encode(resultBytes))
