-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeUUID - Safe UUID parsing and validation
|||
||| This module provides safe operations for UUID handling
||| with proper validation of format and variants.
module Proven.SafeUUID

import public Proven.Core
import public Proven.SafeHex
import Data.String

%default total

--------------------------------------------------------------------------------
-- UUID Types
--------------------------------------------------------------------------------

||| UUID version
public export
data UUIDVersion : Type where
  V1 : UUIDVersion  -- Time-based
  V2 : UUIDVersion  -- DCE Security
  V3 : UUIDVersion  -- MD5 hash
  V4 : UUIDVersion  -- Random
  V5 : UUIDVersion  -- SHA-1 hash
  Unknown : Nat -> UUIDVersion

public export
Eq UUIDVersion where
  V1 == V1 = True
  V2 == V2 = True
  V3 == V3 = True
  V4 == V4 = True
  V5 == V5 = True
  (Unknown a) == (Unknown b) = a == b
  _ == _ = False

public export
Show UUIDVersion where
  show V1 = "v1"
  show V2 = "v2"
  show V3 = "v3"
  show V4 = "v4"
  show V5 = "v5"
  show (Unknown n) = "v" ++ show n

||| UUID variant
public export
data UUIDVariant : Type where
  NCS : UUIDVariant         -- Reserved for NCS compatibility
  RFC4122 : UUIDVariant     -- Standard UUID (most common)
  Microsoft : UUIDVariant   -- Reserved for Microsoft compatibility
  Future : UUIDVariant      -- Reserved for future definition

public export
Eq UUIDVariant where
  NCS == NCS = True
  RFC4122 == RFC4122 = True
  Microsoft == Microsoft = True
  Future == Future = True
  _ == _ = False

||| A validated UUID
public export
record UUID where
  constructor MkUUID
  hex : String
  version : UUIDVersion
  variant : UUIDVariant

--------------------------------------------------------------------------------
-- UUID Parsing
--------------------------------------------------------------------------------

||| Parse a UUID string (with or without hyphens)
public export
parse : String -> Maybe UUID
parse s =
  let normalized = removeHyphens s
  in if length normalized /= 32 then Nothing
     else if not (isValidHex normalized) then Nothing
     else Just (MkUUID normalized (extractVersion normalized) (extractVariant normalized))
  where
    removeHyphens : String -> String
    removeHyphens = pack . filter (/= '-') . unpack

    extractVersion : String -> UUIDVersion
    extractVersion hex =
      case strIndex hex 12 of
        Just '1' => V1
        Just '2' => V2
        Just '3' => V3
        Just '4' => V4
        Just '5' => V5
        Just c => Unknown (cast (ord c - ord '0'))
        Nothing => Unknown 0

    extractVariant : String -> UUIDVariant
    extractVariant hex =
      case strIndex hex 16 >>= hexToNibble of
        Just n =>
          if n < 8 then NCS
          else if n < 12 then RFC4122
          else if n < 14 then Microsoft
          else Future
        Nothing => RFC4122

||| Validate a UUID string
public export
isValid : String -> Bool
isValid s = isJust (parse s)

||| Check if a UUID is the nil UUID (all zeros)
public export
isNil : UUID -> Bool
isNil uuid = all (== '0') (unpack uuid.hex)

--------------------------------------------------------------------------------
-- UUID Formatting
--------------------------------------------------------------------------------

||| Format a UUID with hyphens (8-4-4-4-12)
public export
format : UUID -> String
format uuid =
  let h = uuid.hex
  in substr 0 8 h ++ "-" ++
     substr 8 4 h ++ "-" ++
     substr 12 4 h ++ "-" ++
     substr 16 4 h ++ "-" ++
     substr 20 12 h

||| Format a UUID without hyphens
public export
formatCompact : UUID -> String
formatCompact uuid = uuid.hex

||| Format a UUID in uppercase
public export
formatUpper : UUID -> String
formatUpper uuid = toUpper (format uuid)

||| Format a UUID as a URN
public export
formatURN : UUID -> String
formatURN uuid = "urn:uuid:" ++ toLower (format uuid)

--------------------------------------------------------------------------------
-- Well-Known UUIDs
--------------------------------------------------------------------------------

||| The nil UUID (all zeros)
public export
nilUUID : UUID
nilUUID = MkUUID (replicate 32 '0') (Unknown 0) NCS

--------------------------------------------------------------------------------
-- UUID Comparison
--------------------------------------------------------------------------------

public export
Eq UUID where
  a == b = toLower a.hex == toLower b.hex

public export
Ord UUID where
  compare a b = compare (toLower a.hex) (toLower b.hex)

public export
Show UUID where
  show = format
