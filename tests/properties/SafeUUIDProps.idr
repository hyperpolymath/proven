-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeUUIDProps

import Proven.Core
import Proven.SafeUUID
import Data.Maybe
import Data.String
import System

%default total

-- Rewritten against the real API: `parse : String -> Maybe UUID`,
-- `isValid`, `isNil`, `format`, `formatCompact`, record fields `version` /
-- `variant`. There is no `parseUUID`, `ValidUUID`, `toString`, `getVersion`,
-- `generateUUIDv4`, `toBytes` or `fromBytes`; bytes are reached through the
-- re-exported `Proven.SafeHex` (`decode`/`encode`).

private
v4Sample : String
v4Sample = "550e8400-e29b-41d4-a716-446655440000"

--------------------------------------------------------------------------------
-- Type-level proofs (closed computations that reduce). Accepting a UUID needs
-- the hex-character test in `isValidHex`, which does not reduce at the type
-- level, so the positive parses are execution checks below.
--------------------------------------------------------------------------------

||| A non-hex string is rejected.
prop_invalidFormatFails : isValid "not-a-uuid" = False
prop_invalidFormatFails = Refl

||| A truncated UUID is rejected.
prop_wrongLengthFails : isValid "550e8400-e29b-41d4-a716" = False
prop_wrongLengthFails = Refl

--------------------------------------------------------------------------------
-- Outstanding obligations (named propositions, not witnesses)
--------------------------------------------------------------------------------

||| OWED: formatting a parsed UUID and parsing it again gives the same UUID
||| (was prop_formatConsistent). Blocked by: `substr`/`++`/`unpack` over an
||| abstract String do not reduce at the type level. Holds only for parsed
||| UUIDs: `MkUUID` is public, so an arbitrary record need not be well-formed.
||| This names the proposition; it is not a witness.
public export
UuidFormatRoundTripObligation : Type
UuidFormatRoundTripObligation =
  (s : String) -> (u : UUID) -> Proven.SafeUUID.parse s = Just u ->
  Proven.SafeUUID.parse (format u) = Just u

||| OWED: UUID equality is reflexive (was prop_uuidEqReflexive). Blocked by:
||| `==` compares `toLower` of the hex strings, and `toLower`/String equality do
||| not reduce for an abstract String. This names the proposition; it is not a
||| witness.
public export
UuidEqReflexiveObligation : Type
UuidEqReflexiveObligation = (u : UUID) -> (u == u) = True

||| OWED: a parsed UUID decodes to 16 bytes that re-encode to an equal UUID
||| (was prop_bytesRoundtrip). Blocked by: String primitives in `decode`,
||| `encode` and `parse` do not reduce for an abstract String.
||| This names the proposition; it is not a witness.
public export
UuidBytesRoundTripObligation : Type
UuidBytesRoundTripObligation =
  (s : String) -> (u : UUID) -> Proven.SafeUUID.parse s = Just u ->
  (bs : List Nat ** (decode (formatCompact u) = Just bs,
                     length bs = 16,
                     map (== u) (Proven.SafeUUID.parse (encode bs)) = Just True))

--------------------------------------------------------------------------------
-- Execution checks
--------------------------------------------------------------------------------

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

private
samples : List String
samples = [ v4Sample
          , "550E8400-E29B-41D4-A716-446655440000"
          , "00000000-0000-0000-0000-000000000000"
          , "6ba7b810-9dad-11d1-80b4-00c04fd430c8"
          , "6fa459ea-ee8a-3ca4-894e-db77e160355e"
          , "886313e1-3b8a-5372-9b90-0c9aee199e5d" ]

private
versionOf : String -> Maybe UUIDVersion
versionOf s = map (.version) (Proven.SafeUUID.parse s)

private
roundTrips : String -> Bool
roundTrips s = case Proven.SafeUUID.parse s of
  Nothing => False
  Just u => case Proven.SafeUUID.parse (format u) of
    Nothing => False
    Just u' => u' == u && u'.hex == u.hex && u'.version == u.version &&
               u'.variant == u.variant

private
bytesRoundTrip : String -> Bool
bytesRoundTrip s = case Proven.SafeUUID.parse s of
  Nothing => False
  Just u => case decode (formatCompact u) of
    Nothing => False
    Just bs => length bs == 16 &&
               map (== u) (Proven.SafeUUID.parse (encode bs)) == Just True

||| Test runner for UUID properties
export
runUUIDProperties : IO ()
runUUIDProperties = do
  putStrLn "SafeUUID Property Tests"
  putStrLn "======================="
  putStrLn "prop_invalidFormatFails: PASS (proven by type)"
  putStrLn "prop_wrongLengthFails: PASS (proven by type)"
  check "prop_validUUIDv4Parses: canonical v4 UUID accepted"
    (isValid v4Sample)
  check "prop_validUUIDv4Parses: version V4, variant RFC 4122"
    (map (\u => u.version == V4 && u.variant == RFC4122) (Proven.SafeUUID.parse v4Sample) == Just True)
  check "prop_uppercaseUUIDParses: uppercase hex accepted"
    (isValid "550E8400-E29B-41D4-A716-446655440000")
  check "prop_nilUUIDParses: nil UUID accepted"
    (isValid "00000000-0000-0000-0000-000000000000")
  check "nil UUID string is recognised as nil; v4 sample is not"
    (map isNil (Proven.SafeUUID.parse "00000000-0000-0000-0000-000000000000") == Just True &&
     map isNil (Proven.SafeUUID.parse v4Sample) == Just False && isNil nilUUID)
  check "prop_generatedUUIDValid: library-produced nilUUID formats to a valid UUID"
    (isValid (format nilUUID) && format nilUUID == "00000000-0000-0000-0000-000000000000")
  check "prop_versionExtractable: versions 1, 3, 4, 5 read from the version nibble"
    (versionOf "6ba7b810-9dad-11d1-80b4-00c04fd430c8" == Just V1 &&
     versionOf "6fa459ea-ee8a-3ca4-894e-db77e160355e" == Just V3 &&
     versionOf v4Sample == Just V4 &&
     versionOf "886313e1-3b8a-5372-9b90-0c9aee199e5d" == Just V5)
  check "prop_formatConsistent: format/parse round-trip on samples"
    (all roundTrips samples)
  check "format restores 8-4-4-4-12 hyphenation from compact input"
    (map format (Proven.SafeUUID.parse "550e8400e29b41d4a716446655440000") == Just v4Sample)
  check "prop_uuidEqReflexive: == reflexive and case-insensitive on samples"
    (all (\s => map (\u => u == u) (Proven.SafeUUID.parse s) == Just True) samples &&
     Proven.SafeUUID.parse v4Sample == Proven.SafeUUID.parse "550E8400-E29B-41D4-A716-446655440000")
  check "prop_bytesRoundtrip: 16 bytes that re-encode to an equal UUID"
    (all bytesRoundTrip samples)
  check "rejects non-hex characters and wrong lengths"
    (not (isValid "550e8400-e29b-41d4-a716-44665544000g") &&
     not (isValid "550e8400-e29b-41d4-a716-4466554400000") && not (isValid ""))
  check "prop_versionExtractable: hex version nibble 'a' is version 10"
    (versionOf "550e8400-e29b-a1d4-a716-446655440000" == Just (Unknown 10))
  check "prop_versionExtractable: version nibble is case-insensitive hex ('F' and 'f' are 15)"
    (versionOf "550e8400-e29b-F1d4-a716-446655440000" == Just (Unknown 15) &&
     versionOf "550e8400-e29b-f1d4-a716-446655440000" == Just (Unknown 15) &&
     versionOf "550e8400-e29b-91d4-a716-446655440000" == Just (Unknown 9))
  putStrLn ""
