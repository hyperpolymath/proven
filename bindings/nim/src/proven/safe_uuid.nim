# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe UUID generation and validation following RFC 4122.

import std/[options, strutils, sysrand]

type
  UuidVersion* = enum
    ## UUID version types per RFC 4122.
    uvNil = 0     ## Nil UUID (all zeros)
    uvV1 = 1      ## Time-based
    uvV2 = 2      ## DCE Security
    uvV3 = 3      ## Name-based (MD5)
    uvV4 = 4      ## Random
    uvV5 = 5      ## Name-based (SHA-1)

  UuidVariant* = enum
    ## UUID variant types per RFC 4122.
    uvNcs         ## NCS backward compatibility
    uvRfc4122     ## RFC 4122 (standard)
    uvMicrosoft   ## Microsoft backward compatibility
    uvFuture      ## Reserved for future use

  Uuid* = object
    ## A validated UUID (128 bits).
    bytes*: array[16, byte]

  UuidParseError* = object of CatchableError
    ## Error raised when UUID parsing fails.

const
  NilUuid*: Uuid = Uuid(bytes: [0'u8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])
    ## The nil UUID (all zeros).

  NamespaceDns*: Uuid = Uuid(bytes: [
    0x6b'u8, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
  ])
    ## DNS namespace UUID for name-based UUIDs.

  NamespaceUrl*: Uuid = Uuid(bytes: [
    0x6b'u8, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
  ])
    ## URL namespace UUID for name-based UUIDs.

  NamespaceOid*: Uuid = Uuid(bytes: [
    0x6b'u8, 0xa7, 0xb8, 0x12, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
  ])
    ## OID namespace UUID for name-based UUIDs.

  NamespaceX500*: Uuid = Uuid(bytes: [
    0x6b'u8, 0xa7, 0xb8, 0x14, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
  ])
    ## X.500 namespace UUID for name-based UUIDs.

proc version*(uuid: Uuid): UuidVersion =
  ## Get the UUID version from byte 6.
  let versionByte = (uuid.bytes[6] shr 4) and 0x0F
  case versionByte
  of 1: uvV1
  of 2: uvV2
  of 3: uvV3
  of 4: uvV4
  of 5: uvV5
  else: uvNil

proc variant*(uuid: Uuid): UuidVariant =
  ## Get the UUID variant from byte 8.
  let variantByte = uuid.bytes[8]
  if (variantByte shr 7) == 0:
    uvNcs
  elif (variantByte shr 6) == 0b10:
    uvRfc4122
  elif (variantByte shr 5) == 0b110:
    uvMicrosoft
  else:
    uvFuture

proc isNil*(uuid: Uuid): bool =
  ## Check if this is the nil UUID (all zeros).
  for b in uuid.bytes:
    if b != 0:
      return false
  result = true

proc `==`*(a, b: Uuid): bool =
  ## Check equality of two UUIDs.
  a.bytes == b.bytes

proc `$`*(uuid: Uuid): string =
  ## Format UUID as canonical string (8-4-4-4-12).
  const hexChars = "0123456789abcdef"
  result = newStringOfCap(36)

  for i, b in uuid.bytes:
    result.add hexChars[int(b shr 4)]
    result.add hexChars[int(b and 0x0F)]
    if i in [3, 5, 7, 9]:
      result.add '-'

proc toUrn*(uuid: Uuid): string =
  ## Format UUID as URN (urn:uuid:...).
  result = "urn:uuid:" & $uuid

proc toUppercase*(uuid: Uuid): string =
  ## Format UUID as uppercase string.
  ($uuid).toUpperAscii

proc hexCharToNibble(c: char): Option[byte] =
  ## Convert a hex character to its nibble value.
  case c
  of '0'..'9': some(byte(ord(c) - ord('0')))
  of 'a'..'f': some(byte(ord(c) - ord('a') + 10))
  of 'A'..'F': some(byte(ord(c) - ord('A') + 10))
  else: none(byte)

proc parseUuid*(uuidString: string): Option[Uuid] =
  ## Parse UUID from canonical string format.
  ## Returns None if parsing fails.
  if uuidString.len != 36:
    return none(Uuid)

  # Validate hyphen positions
  if uuidString[8] != '-' or uuidString[13] != '-' or
     uuidString[18] != '-' or uuidString[23] != '-':
    return none(Uuid)

  # Extract hex characters only
  var hexChars = newStringOfCap(32)
  for i, c in uuidString:
    if i notin [8, 13, 18, 23]:
      hexChars.add c

  if hexChars.len != 32:
    return none(Uuid)

  var resultUuid: Uuid
  for i in 0..<16:
    let highNibble = hexCharToNibble(hexChars[i * 2])
    let lowNibble = hexCharToNibble(hexChars[i * 2 + 1])

    if highNibble.isNone or lowNibble.isNone:
      return none(Uuid)

    resultUuid.bytes[i] = (highNibble.get shl 4) or lowNibble.get

  result = some(resultUuid)

proc parseUuidStrict*(uuidString: string): Uuid {.raises: [UuidParseError].} =
  ## Parse UUID from canonical string format.
  ## Raises UuidParseError if parsing fails.
  let parsed = parseUuid(uuidString)
  if parsed.isNone:
    raise newException(UuidParseError, "Invalid UUID format: " & uuidString)
  result = parsed.get

proc isValidUuid*(uuidString: string): bool =
  ## Check if a string is a valid UUID format.
  parseUuid(uuidString).isSome

proc fromBytes*(bytes: array[16, byte]): Uuid =
  ## Create UUID from raw bytes.
  Uuid(bytes: bytes)

proc fromBytes*(bytes: openArray[byte]): Option[Uuid] =
  ## Create UUID from byte sequence. Returns None if length is not 16.
  if bytes.len != 16:
    return none(Uuid)

  var resultUuid: Uuid
  for i in 0..<16:
    resultUuid.bytes[i] = bytes[i]
  result = some(resultUuid)

proc toBytes*(uuid: Uuid): array[16, byte] =
  ## Get the raw bytes of a UUID.
  uuid.bytes

proc v4FromBytes*(randomBytes: array[16, byte]): Uuid =
  ## Create a v4 (random) UUID from provided random bytes.
  ## Sets version and variant bits according to RFC 4122.
  var resultBytes = randomBytes
  # Set version to 4 (random)
  resultBytes[6] = (resultBytes[6] and 0x0F) or 0x40
  # Set variant to RFC 4122
  resultBytes[8] = (resultBytes[8] and 0x3F) or 0x80
  Uuid(bytes: resultBytes)

proc generateV4*(): Uuid =
  ## Generate a new random v4 UUID using system random.
  var randomBytes: array[16, byte]
  if not urandom(randomBytes):
    raise newException(IOError, "Failed to generate random bytes for UUID")
  result = v4FromBytes(randomBytes)

proc hash*(uuid: Uuid): int =
  ## Hash function for UUID (for use in hash tables).
  var h = 0
  for b in uuid.bytes:
    h = h xor int(b)
    h = h * 31
  result = h
