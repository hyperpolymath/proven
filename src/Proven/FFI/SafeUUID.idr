-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeUUID operations
|||
||| This module exports UUID validation and formatting to the C ABI via Idris2's RefC backend.
||| All functions are proven total and validate UUID format and structure.
|||
||| Return conventions:
||| - UUID parsing → (status: Int, result: String)
|||   - status = 0: Valid UUID, result contains formatted UUID
|||   - status = 1: Invalid UUID, result contains error message
||| - Validation → Int (0 = invalid, 1 = valid)
||| - Version → Int (1-5 for known versions, 0 for unknown)
||| - Variant → Int (0=NCS, 1=RFC4122, 2=Microsoft, 3=Future)
|||
||| CRITICAL: Always validate UUIDs before use. Accept both hyphenated and compact formats.
module Proven.FFI.SafeUUID

import Proven.SafeUUID
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode Maybe UUID as (status, result)
encodeMaybeUUID : Maybe UUID -> (Int, String)
encodeMaybeUUID Nothing = (1, "invalid UUID format")
encodeMaybeUUID (Just uuid) = (0, format uuid)

||| Encode UUIDVersion as Int
encodeUUIDVersion : UUIDVersion -> Int
encodeUUIDVersion V1 = 1
encodeUUIDVersion V2 = 2
encodeUUIDVersion V3 = 3
encodeUUIDVersion V4 = 4
encodeUUIDVersion V5 = 5
encodeUUIDVersion (Unknown _) = 0

||| Encode UUIDVariant as Int
encodeUUIDVariant : UUIDVariant -> Int
encodeUUIDVariant NCS = 0
encodeUUIDVariant RFC4122 = 1
encodeUUIDVariant Microsoft = 2
encodeUUIDVariant Future = 3

--------------------------------------------------------------------------------
-- UUID Validation
--------------------------------------------------------------------------------

%export
proven_idris_uuid_is_valid : String -> Int
proven_idris_uuid_is_valid s = encodeBool (isValid s)

%export
proven_idris_uuid_parse : String -> (Int, String)
proven_idris_uuid_parse s = encodeMaybeUUID (parse s)

%export
proven_idris_uuid_is_nil : String -> Int
proven_idris_uuid_is_nil s =
  case parse s of
    Nothing => 0
    Just uuid => encodeBool (isNil uuid)

--------------------------------------------------------------------------------
-- UUID Formatting
--------------------------------------------------------------------------------

%export
proven_idris_uuid_format : String -> (Int, String)
proven_idris_uuid_format s =
  case parse s of
    Nothing => (1, "invalid UUID")
    Just uuid => (0, format uuid)

%export
proven_idris_uuid_format_compact : String -> (Int, String)
proven_idris_uuid_format_compact s =
  case parse s of
    Nothing => (1, "invalid UUID")
    Just uuid => (0, formatCompact uuid)

%export
proven_idris_uuid_format_upper : String -> (Int, String)
proven_idris_uuid_format_upper s =
  case parse s of
    Nothing => (1, "invalid UUID")
    Just uuid => (0, formatUpper uuid)

%export
proven_idris_uuid_format_urn : String -> (Int, String)
proven_idris_uuid_format_urn s =
  case parse s of
    Nothing => (1, "invalid UUID")
    Just uuid => (0, formatURN uuid)

--------------------------------------------------------------------------------
-- UUID Version and Variant
--------------------------------------------------------------------------------

%export
proven_idris_uuid_get_version : String -> Int
proven_idris_uuid_get_version s =
  case parse s of
    Nothing => 0
    Just uuid => encodeUUIDVersion uuid.version

%export
proven_idris_uuid_get_variant : String -> Int
proven_idris_uuid_get_variant s =
  case parse s of
    Nothing => 0
    Just uuid => encodeUUIDVariant uuid.variant

%export
proven_idris_uuid_is_version : String -> Int -> Int
proven_idris_uuid_is_version s expectedVersion =
  case parse s of
    Nothing => 0
    Just uuid => encodeBool (encodeUUIDVersion uuid.version == expectedVersion)

%export
proven_idris_uuid_is_rfc4122 : String -> Int
proven_idris_uuid_is_rfc4122 s =
  case parse s of
    Nothing => 0
    Just uuid => encodeBool (uuid.variant == RFC4122)

--------------------------------------------------------------------------------
-- Well-Known UUIDs
--------------------------------------------------------------------------------

%export
proven_idris_uuid_nil : String
proven_idris_uuid_nil = format nilUUID

%export
proven_idris_uuid_nil_compact : String
proven_idris_uuid_nil_compact = formatCompact nilUUID

--------------------------------------------------------------------------------
-- UUID Comparison
--------------------------------------------------------------------------------

%export
proven_idris_uuid_compare : String -> String -> Int
proven_idris_uuid_compare a b =
  case (parse a, parse b) of
    (Just ua, Just ub) =>
      if ua == ub then 0
      else if ua < ub then (-1)
      else 1
    _ => (-2)  -- Invalid UUID(s)

%export
proven_idris_uuid_equals : String -> String -> Int
proven_idris_uuid_equals a b =
  case (parse a, parse b) of
    (Just ua, Just ub) => encodeBool (ua == ub)
    _ => 0  -- Invalid UUIDs are never equal

--------------------------------------------------------------------------------
-- Format Detection
--------------------------------------------------------------------------------

%export
proven_idris_uuid_has_hyphens : String -> Int
proven_idris_uuid_has_hyphens s =
  encodeBool (isInfixOf "-" s)

%export
proven_idris_uuid_normalize : String -> (Int, String)
proven_idris_uuid_normalize s =
  case parse s of
    Nothing => (1, "invalid UUID")
    Just uuid => (0, formatCompact uuid)

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_uuid_friendly_error : String -> String
proven_idris_uuid_friendly_error errorMsg =
  if isInfixOf "invalid" (toLower errorMsg)
    then "Invalid UUID format (expected 32 hex digits or 8-4-4-4-12 pattern)"
  else
    "UUID validation error"
