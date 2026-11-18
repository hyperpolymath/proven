-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeHexProps

import Proven.Core
import Proven.SafeHex
import Data.List
import Data.Maybe
import Data.String
import System

%default total

-- Real API: `encode : List Nat -> String` (lower-case), `encodeUpper`,
-- `decode : String -> Maybe (List Nat)` (no prefix handling),
-- `isValidHex`, and `hexToInt : String -> Maybe Integer`, which is the
-- function that accepts a `0x`/`0X` prefix. Bytes are `Nat` (taken mod 256),
-- not `Bits8`; there is no `ValidHex` type.

--------------------------------------------------------------------------------
-- Type-level proofs (closed decodes reduce)
--------------------------------------------------------------------------------

||| Lower-case hex decodes to the expected bytes.
prop_lowercaseHexParses : decode "deadbeef" = Just [222, 173, 190, 239]
prop_lowercaseHexParses = Refl

||| Upper-case hex decodes to the same bytes.
prop_uppercaseHexParses : decode "DEADBEEF" = Just [222, 173, 190, 239]
prop_uppercaseHexParses = Refl

||| Mixed-case hex decodes to the same bytes.
prop_mixedCaseHexParses : decode "DeAdBeEf" = Just [222, 173, 190, 239]
prop_mixedCaseHexParses = Refl

||| The empty string decodes to no bytes.
prop_emptyStringParses : decode "" = Just []
prop_emptyStringParses = Refl

||| Non-hex characters are rejected.
prop_invalidCharsFail : decode "xyz123" = Nothing
prop_invalidCharsFail = Refl

||| An odd number of digits (incomplete byte) is rejected.
prop_oddLengthFails : decode "abc" = Nothing
prop_oddLengthFails = Refl

||| `decode` does not strip a `0x` prefix (that is `hexToInt`; see below).
prop_decodeRejectsPrefix : decode "0xdeadbeef" = Nothing
prop_decodeRejectsPrefix = Refl

--------------------------------------------------------------------------------
-- Outstanding obligations
--------------------------------------------------------------------------------

||| OWED: encode/decode round-trip for byte-range values.
||| Blocked by: `byteToHex` uses Nat `mod`/`div` and String `++`, neither of
||| which reduces for an abstract list. Names the proposition only.
public export
EncodeDecodeRoundTripObligation : Type
EncodeDecodeRoundTripObligation =
  (bytes : List Nat) -> all (\b => b < 256) bytes = True ->
  decode (encode bytes) = Just bytes

||| OWED: decode/encode round-trip normalises to lower case.
||| Blocked by: String `toLower`/`unpack` are primitives over an abstract
||| string. Names the proposition only.
public export
DecodeEncodeRoundTripObligation : Type
DecodeEncodeRoundTripObligation =
  (s : String) -> isValidHex s = True -> map encode (decode s) = Just (toLower s)

||| OWED: encoding doubles the length.
||| Blocked by: `length` of a primitive String concatenation does not reduce.
||| Names the proposition only.
public export
HexDoublesLengthObligation : Type
HexDoublesLengthObligation =
  (bytes : List Nat) -> length (encode bytes) = 2 * length bytes

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
byteCorpus : List (List Nat)
byteCorpus = [[], [0], [255], [0, 1, 15, 16, 127, 128, 254, 255], [222, 173, 190, 239]]

private
hexCorpus : List String
hexCorpus = ["", "00", "ff", "FF", "DeAdBeEf", "0123456789abcdefABCDEF"]

||| Test runner for hex properties
export
runHexProperties : IO ()
runHexProperties = do
  putStrLn "SafeHex Property Tests"
  putStrLn "======================"
  check "single byte encodes: [255] -> \"ff\"" (encode [255] == "ff")
  check "zero byte encodes: [0] -> \"00\"" (encode [0] == "00")
  check "upper-case encoding" (encodeUpper [222, 173] == "DEAD")
  check "0x / 0X prefix handled by hexToInt"
    (hexToInt "0xdeadbeef" == Just 3735928559 && hexToInt "0XFF" == Just 255 &&
     hexToInt "0x" == Nothing)
  check "encode/decode round-trip on byte corpus"
    (all (\bs => decode (encode bs) == Just bs) byteCorpus)
  check "decode/encode round-trip normalises to lower case"
    (all (\s => map encode (decode s) == Just (toLower s)) hexCorpus)
  check "encoding doubles the length"
    (all (\bs => length (encode bs) == 2 * length bs) byteCorpus)
  check "isValidHex agrees with decode on corpus"
    (all (\s => isValidHex s == isJust (decode s))
         (hexCorpus ++ ["abc", "xyz123", "0xff", "g0"]))
  putStrLn ""
