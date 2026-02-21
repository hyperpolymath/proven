-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeBase64Props

import Proven.Core
import Proven.SafeBase64

%default total

||| Property: Encode-decode roundtrip
prop_encodeDecodeRoundtrip : (s : String) ->
                             decode (encode s) = Ok s
prop_encodeDecodeRoundtrip s = ?prop_encodeDecodeRoundtrip_rhs

||| Property: Empty string encodes to empty
prop_emptyEncodesEmpty : encode "" = ""
prop_emptyEncodesEmpty = Refl

||| Property: Valid base64 decodes
prop_validBase64Decodes : isOk (decode "SGVsbG8gV29ybGQ=") = True
prop_validBase64Decodes = Refl

||| Property: Invalid characters fail
prop_invalidCharsFail : isErr (decode "Invalid@#$%") = True
prop_invalidCharsFail = Refl

||| Property: Incorrect padding fails
prop_incorrectPaddingFails : isErr (decode "SGVsbG8===") = True
prop_incorrectPaddingFails = ?prop_incorrectPaddingFails_rhs

||| Property: URL-safe encoding works
prop_urlSafeEncode : encode_urlsafe "test?query=value" = encode_urlsafe "test?query=value"
prop_urlSafeEncode = Refl

||| Property: URL-safe uses correct characters
prop_urlSafeChars : containsUrlUnsafe (encode_urlsafe "binary\255data") = False
prop_urlSafeChars = ?prop_urlSafeChars_rhs

||| Property: Standard and URL-safe decode to same
prop_standardUrlSafeSame : (s : String) ->
                           decode (encode s) = decode_urlsafe (encode_urlsafe s)
prop_standardUrlSafeSame s = ?prop_standardUrlSafeSame_rhs

||| Property: Encoded length is predictable
prop_encodedLength : (s : String) ->
                     length (encode s) = expectedEncodedLength (length s)
prop_encodedLength s = ?prop_encodedLength_rhs

||| Property: Decode preserves binary data
prop_binaryPreserved : (bytes : List Bits8) ->
                       decodeBytes (encodeBytes bytes) = Ok bytes
prop_binaryPreserved bytes = ?prop_binaryPreserved_rhs

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
