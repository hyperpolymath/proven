-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeHexProps

import Proven.Core
import Proven.SafeHex

%default total

||| Property: Valid lowercase hex parses
prop_lowercaseHexParses : isOk (parseHex "deadbeef") = True
prop_lowercaseHexParses = Refl

||| Property: Valid uppercase hex parses
prop_uppercaseHexParses : isOk (parseHex "DEADBEEF") = True
prop_uppercaseHexParses = Refl

||| Property: Mixed case hex parses
prop_mixedCaseHexParses : isOk (parseHex "DeAdBeEf") = True
prop_mixedCaseHexParses = Refl

||| Property: Empty string parses to empty
prop_emptyStringParses : isOk (parseHex "") = True
prop_emptyStringParses = Refl

||| Property: Invalid characters fail
prop_invalidCharsFail : isErr (parseHex "xyz123") = True
prop_invalidCharsFail = Refl

||| Property: Odd length fails (incomplete byte)
prop_oddLengthFails : isErr (parseHex "abc") = True
prop_oddLengthFails = Refl

||| OWED: Encode-decode roundtrip
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_encodeDecodeRoundtrip_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_encodeDecodeRoundtrip : (bytes : List Bits8) ->
                               parseHex (toHex bytes) = Ok bytes

||| OWED: Decode-encode roundtrip
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_decodeEncodeRoundtrip_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_decodeEncodeRoundtrip : (hex : ValidHex) ->
                               toHex (fromHex hex) = toLower hex

||| OWED: Hex encoding doubles length
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_hexDoublesLength_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_hexDoublesLength : (bytes : List Bits8) ->
                          length (toHex bytes) = 2 * length bytes

||| Property: 0x prefix handled
prop_prefixHandled : isOk (parseHex "0xdeadbeef") = True
prop_prefixHandled = Refl

||| Property: Single byte encodes correctly
prop_singleByteEncode : toHex [0xff] = "ff"
prop_singleByteEncode = Refl

||| Property: Zero byte encodes correctly
prop_zeroByteEncode : toHex [0x00] = "00"
prop_zeroByteEncode = Refl

||| Test runner for hex properties
export
runHexProperties : IO ()
runHexProperties = do
  putStrLn "SafeHex Property Tests"
  putStrLn "======================"
  putStrLn "prop_lowercaseHexParses: PASS (proven by type)"
  putStrLn "prop_uppercaseHexParses: PASS (proven by type)"
  putStrLn "prop_mixedCaseHexParses: PASS (proven by type)"
  putStrLn "prop_emptyStringParses: PASS (proven by type)"
  putStrLn "prop_invalidCharsFail: PASS (proven by type)"
  putStrLn "prop_oddLengthFails: PASS (proven by type)"
  putStrLn "prop_prefixHandled: PASS (proven by type)"
  putStrLn "prop_singleByteEncode: PASS (proven by type)"
  putStrLn "prop_zeroByteEncode: PASS (proven by type)"
  putStrLn ""
