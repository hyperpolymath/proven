# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe UUID generation and validation following RFC 4122.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

type
  Uuid* = object
    ## A validated UUID (128 bits) backed by libproven's verified
    ## implementation.
    bytes*: array[16, byte]

  UuidParseError* = object of CatchableError
    ## Error raised when UUID parsing fails.

proc generateV4*(): Option[Uuid] =
  ## Generate a new random v4 UUID via libproven.
  ## Returns None if generation fails.
  let res = provenUuidV4()
  if res.status != PROVEN_OK:
    return none(Uuid)
  some(Uuid(bytes: res.uuid.bytes))

proc generateV4Strict*(): Uuid {.raises: [IOError].} =
  ## Generate a new random v4 UUID via libproven.
  ## Raises IOError if generation fails.
  let res = provenUuidV4()
  if res.status != PROVEN_OK:
    raise newException(IOError, "Failed to generate UUID v4")
  Uuid(bytes: res.uuid.bytes)

proc parseUuid*(uuidString: string): Option[Uuid] =
  ## Parse UUID from canonical string format (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx)
  ## via libproven.  Returns None if parsing fails.
  if uuidString.len == 0:
    return none(Uuid)
  let res = provenUuidParse(unsafeAddr uuidString[0], csize_t(uuidString.len))
  if res.status != PROVEN_OK:
    return none(Uuid)
  some(Uuid(bytes: res.uuid.bytes))

proc parseUuidStrict*(uuidString: string): Uuid {.raises: [UuidParseError].} =
  ## Parse UUID from canonical string format.
  ## Raises UuidParseError if parsing fails.
  let parsed = parseUuid(uuidString)
  if parsed.isNone:
    raise newException(UuidParseError,
      "Invalid UUID format: " & uuidString)
  parsed.get

proc isValidUuid*(uuidString: string): bool =
  ## Check if a string is a valid UUID format.
  parseUuid(uuidString).isSome

proc isNil*(uuid: Uuid): bool =
  ## Check if this is the nil UUID (all zeros) via libproven.
  let provenUuid = ProvenUUID(bytes: uuid.bytes)
  provenUuidIsNil(provenUuid)

proc version*(uuid: Uuid): uint8 =
  ## Get the UUID version via libproven.
  let provenUuid = ProvenUUID(bytes: uuid.bytes)
  provenUuidVersion(provenUuid)

proc `$`*(uuid: Uuid): string =
  ## Format UUID as canonical string (8-4-4-4-12) via libproven.
  let provenUuid = ProvenUUID(bytes: uuid.bytes)
  let res = provenUuidToString(provenUuid)
  if res.status != PROVEN_OK or res.value == nil:
    # Fallback should not normally happen; return empty string to avoid crash.
    return ""
  result = $res.value
  provenFreeString(res.value)

proc toUrn*(uuid: Uuid): string =
  ## Format UUID as URN (urn:uuid:...).
  "urn:uuid:" & $uuid

proc `==`*(a, b: Uuid): bool =
  ## Check equality of two UUIDs.
  a.bytes == b.bytes

proc fromBytes*(bytes: array[16, byte]): Uuid =
  ## Create UUID from raw bytes.
  Uuid(bytes: bytes)

proc fromBytes*(bytes: openArray[byte]): Option[Uuid] =
  ## Create UUID from byte sequence.  Returns None if length is not 16.
  if bytes.len != 16:
    return none(Uuid)
  var resultUuid: Uuid
  for i in 0..<16:
    resultUuid.bytes[i] = bytes[i]
  some(resultUuid)

proc toBytes*(uuid: Uuid): array[16, byte] =
  ## Get the raw bytes of a UUID.
  uuid.bytes

proc hash*(uuid: Uuid): int =
  ## Hash function for UUID (for use in hash tables).
  ## Delegates to a simple XOR-multiply chain.
  var h = 0
  for b in uuid.bytes:
    h = h xor int(b)
    h = h * 31
  h
