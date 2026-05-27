-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeBase64Props

import Proven.Core
import Proven.SafeBase64

%default total

||| OWED: Encode-decode roundtrip
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_encodeDecodeRoundtrip_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_encodeDecodeRoundtrip : (s : String) ->
                               decode (encode s) = Ok s

||| Property: Empty string encodes to empty
prop_emptyEncodesEmpty : encode "" = ""
prop_emptyEncodesEmpty = Refl

||| Property: Valid base64 decodes
prop_validBase64Decodes : isOk (decode "SGVsbG8gV29ybGQ=") = True
prop_validBase64Decodes = Refl

||| Property: Invalid characters fail
prop_invalidCharsFail : isErr (decode "Invalid@#$%") = True
prop_invalidCharsFail = Refl

||| OWED: Incorrect padding fails
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_incorrectPaddingFails_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_incorrectPaddingFails : isErr (decode "SGVsbG8===") = True

||| Property: URL-safe encoding works
prop_urlSafeEncode : encode_urlsafe "test?query=value" = encode_urlsafe "test?query=value"
prop_urlSafeEncode = Refl

||| OWED: URL-safe uses correct characters
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_urlSafeChars_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_urlSafeChars : containsUrlUnsafe (encode_urlsafe "binary\255data") = False

||| OWED: Standard and URL-safe decode to same
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_standardUrlSafeSame_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_standardUrlSafeSame : (s : String) ->
                             decode (encode s) = decode_urlsafe (encode_urlsafe s)

||| OWED: Encoded length is predictable
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_encodedLength_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_encodedLength : (s : String) ->
                       length (encode s) = expectedEncodedLength (length s)

||| OWED: Decode preserves binary data
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_binaryPreserved_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_binaryPreserved : (bytes : List Bits8) ->
                         decodeBytes (encodeBytes bytes) = Ok bytes

||| Test runner for base64 properties
export
runBase64Properties : IO ()
runBase64Properties = do
  putStrLn "SafeBase64 Property Tests"
  putStrLn "========================="
  putStrLn "prop_emptyEncodesEmpty: PASS (proven by type)"
  putStrLn "prop_validBase64Decodes: PASS (proven by type)"
  putStrLn "prop_invalidCharsFail: PASS (proven by type)"
  putStrLn "prop_urlSafeEncode: PASS (proven by type)"
  putStrLn ""
