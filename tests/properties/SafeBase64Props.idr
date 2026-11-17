-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeBase64Props

import Proven.Core
import Proven.SafeBase64
import Data.List
import System

%default total

-- Real API: encode/decodeString (standard, padded), encodeRaw/decodeRaw
-- (bytes), encodeForURL/decodeFromURL (URL-safe, unpadded). The encode and
-- decode loops are private and `covering` in SafeBase64.Encode/Decode, so
-- closed encode/decode results do not reduce at the type level; those
-- properties are execution checks below. Because the library's encoders and
-- decoders are only `covering`, every definition here that mentions them is
-- annotated `covering` (an honest, weaker claim); the module default stays
-- `total` and nothing is marked `partial`.

-- No type-level proofs: closed `div` on Nat (used by encodedSize) does not
-- reduce in Idris2 0.8.0, so even encodedSize 0 = 0 is an execution check.

--------------------------------------------------------------------------------
-- Outstanding obligations
--------------------------------------------------------------------------------

||| OWED: string encode/decode round-trip. Blocked by: private `covering`
||| encode/decode loops over Bits8 primitives and String pack/unpack.
||| NOTE: currently REFUTED by execution for code points >= 256 (encode
||| truncates each Char to its low byte; see runBase64Properties). This
||| names the proposition; it is not a witness.
public export
covering
Base64StringRoundTripObligation : Type
Base64StringRoundTripObligation =
  (s : String) -> decodeString (encode s) = Ok s

||| OWED: byte encode/decode round-trip preserves binary data. Blocked by:
||| private `covering` encodeLoop/decodeLoop over Bits8 shift/mask
||| primitives, which do not reduce symbolically. This names the
||| proposition; it is not a witness.
public export
covering
Base64BytesRoundTripObligation : Type
Base64BytesRoundTripObligation =
  (bytes : List Bits8) -> decodeRaw (encodeRaw bytes) = Ok bytes

||| OWED: standard and URL-safe encodings decode to the same bytes. Blocked
||| by: same private covering loops. This names the proposition; it is not
||| a witness.
public export
covering
Base64VariantAgreementObligation : Type
Base64VariantAgreementObligation =
  (bytes : List Bits8) ->
  decodeRaw (encodeRaw bytes) = decodeFromURLRaw (encodeForURLRaw bytes)

||| OWED: encoded length is predictable, length(encodeRaw b) = encodedSize
||| (length b). Blocked by: private covering encodeLoop and String
||| pack/unpack. This names the proposition; it is not a witness.
public export
covering
Base64EncodedLengthObligation : Type
Base64EncodedLengthObligation =
  (bytes : List Bits8) -> length (unpack (encodeRaw bytes)) = encodedSize (length bytes)

||| OWED: URL-safe encoding emits only URL-safe alphabet characters (no
||| '+', '/', '='). Blocked by: private covering encodeLoop and Char
||| comparison primitives. This names the proposition; it is not a witness.
public export
covering
Base64URLSafeAlphabetObligation : Type
Base64URLSafeAlphabetObligation =
  (bytes : List Bits8) ->
  all (isValidBase64Char URLSafeNoPad) (unpack (encodeForURLRaw bytes)) = True

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
latin1Corpus : List String
latin1Corpus = ["", "a", "ab", "abc", "abcd", "Hello World", "test?query=value",
                "binary\255data", "\0\1\2", "caf\233", "~~~>>>???"]

private
nonLatin1Corpus : List String
nonLatin1Corpus = ["\x20AC", "\x65E5\x672C"]

private
allBytes : List Bits8
allBytes = map cast [the Integer 0 .. 255]

private
byteCorpus : List (List Bits8)
byteCorpus = [[], [0], [255, 0], [1, 2, 3], [251, 255, 191], allBytes,
              drop 1 allBytes, drop 2 allBytes]

private covering
roundTrips : String -> Bool
roundTrips s = resultToMaybe (decodeString (encode s)) == Just s

||| Test runner for base64 properties
export covering
runBase64Properties : IO ()
runBase64Properties = do
  putStrLn "SafeBase64 Property Tests"
  putStrLn "========================="
  check "encodedSize is ceil(n/3)*4 at group boundaries (0,1,2,3,4,6 -> 0,4,4,4,8,8)"
    (map encodedSize [0, 1, 2, 3, 4, 6] == [0, 4, 4, 4, 8, 8])
  check "empty string encodes to empty" (encode "" == "")
  check "valid base64 decodes: SGVsbG8gV29ybGQ= -> Hello World"
    (resultToMaybe (decodeString "SGVsbG8gV29ybGQ=") == Just "Hello World")
  check "invalid characters fail: Invalid@#$%" (isErr (decodeString "Invalid@#$%"))
  check "incorrect padding fails: SGVsbG8===" (isErr (decodeString "SGVsbG8==="))
  check "URL-safe encoding of test?query=value round-trips"
    (resultToMaybe (decodeFromURL (encodeForURL "test?query=value")) == Just "test?query=value")
  check "URL-safe output uses only URL-safe chars (binary\\255data, all bytes)"
    (all (all (isValidBase64Char URLSafeNoPad) . unpack)
         [encodeForURL "binary\255data", encodeForURLRaw allBytes])
  check "byte round-trip preserves binary data (corpus incl. all 256 bytes)"
    (all (\b => resultToMaybe (decodeRaw (encodeRaw b)) == Just b) byteCorpus)
  check "standard and URL-safe decode to same bytes"
    (all (\b => resultToMaybe (decodeRaw (encodeRaw b)) ==
                resultToMaybe (decodeFromURLRaw (encodeForURLRaw b))) byteCorpus)
  check "encoded length = encodedSize (input length)"
    (all (\b => length (unpack (encodeRaw b)) == encodedSize (length b)) byteCorpus)
  check "string round-trip on Latin-1 corpus" (all roundTrips latin1Corpus)
  -- Last: currently FAILS (suspected library defect, reported). encode is
  -- documented as UTF-8 but maps each Char to `cast (ord c)` (low byte only),
  -- e.g. encode "\x20AC" = "rA==" instead of UTF-8 "4oKs".
  check "string round-trip on non-Latin-1 corpus (euro sign, CJK)"
    (all roundTrips nonLatin1Corpus)
  putStrLn ""
