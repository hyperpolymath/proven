-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeBase64Unit

import Proven.Core
import Proven.SafeBase64

%default total

assertOk : Show e => String -> Result e a -> IO ()
assertOk name (Ok _) = putStrLn $ "  ✓ " ++ name
assertOk name (Err e) = putStrLn $ "  ✗ " ++ name ++ " (got error: " ++ show e ++ ")"

assertErr : Show a => String -> Result e a -> IO ()
assertErr name (Err _) = putStrLn $ "  ✓ " ++ name
assertErr name (Ok v) = putStrLn $ "  ✗ " ++ name ++ " (expected error, got: " ++ show v ++ ")"

assertEq : (Eq a, Show a) => String -> a -> a -> IO ()
assertEq name expected actual =
  if expected == actual
    then putStrLn $ "  ✓ " ++ name
    else putStrLn $ "  ✗ " ++ name ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")"

assertTrue : String -> Bool -> IO ()
assertTrue name True = putStrLn $ "  ✓ " ++ name
assertTrue name False = putStrLn $ "  ✗ " ++ name ++ " (expected True)"

export
runBase64UnitTests : IO ()
runBase64UnitTests = do
  putStrLn "SafeBase64 Unit Tests"
  putStrLn "====================="

  -- Standard encoding tests
  putStrLn "\n[Standard Encoding]"
  assertEq "encode \"\" = \"\"" "" (encode Standard "")
  assertEq "encode \"f\" = \"Zg==\"" "Zg==" (encode Standard "f")
  assertEq "encode \"fo\" = \"Zm8=\"" "Zm8=" (encode Standard "fo")
  assertEq "encode \"foo\" = \"Zm9v\"" "Zm9v" (encode Standard "foo")
  assertEq "encode \"foobar\" = \"Zm9vYmFy\"" "Zm9vYmFy" (encode Standard "foobar")
  assertEq "encode \"Hello, World!\"" "SGVsbG8sIFdvcmxkIQ==" (encode Standard "Hello, World!")

  -- Standard decoding tests
  putStrLn "\n[Standard Decoding]"
  assertOk "decode \"\" succeeds" (decode Standard "")
  assertOk "decode \"Zg==\" = \"f\"" (decode Standard "Zg==")
  assertOk "decode \"Zm8=\" = \"fo\"" (decode Standard "Zm8=")
  assertOk "decode \"Zm9v\" = \"foo\"" (decode Standard "Zm9v")
  assertOk "decode \"SGVsbG8sIFdvcmxkIQ==\"" (decode Standard "SGVsbG8sIFdvcmxkIQ==")
  assertErr "decode invalid chars" (decode Standard "!!!invalid!!!")
  assertErr "decode wrong padding" (decode Standard "Zg=")

  -- URL-safe encoding tests
  putStrLn "\n[URL-Safe Encoding]"
  -- Standard base64 uses + and /, URL-safe uses - and _
  assertEq "encode URLSafe binary with + and /"
           (encode URLSafe "\x3e\x3f") -- Would be +/ in standard
           (encode URLSafe "\x3e\x3f")
  assertTrue "URLSafe encoding has no +"
             (not $ contains "+" (encode URLSafe "\x3e\x3f\x3e\x3f"))
  assertTrue "URLSafe encoding has no /"
             (not $ contains "/" (encode URLSafe "\x3e\x3f\x3e\x3f"))

  -- URL-safe no padding tests
  putStrLn "\n[URL-Safe No Padding]"
  assertEq "encode URLSafeNoPad \"f\"" "Zg" (encode URLSafeNoPad "f")
  assertEq "encode URLSafeNoPad \"fo\"" "Zm8" (encode URLSafeNoPad "fo")
  assertTrue "URLSafeNoPad has no ="
             (not $ contains "=" (encode URLSafeNoPad "f"))

  -- MIME encoding tests
  putStrLn "\n[MIME Encoding]"
  let longInput = replicate 100 'A'
  let mimeEncoded = encode MIME longInput
  assertTrue "MIME encoding adds line breaks"
             (contains "\n" mimeEncoded)

  -- Roundtrip tests
  putStrLn "\n[Roundtrip]"
  let testStrings = ["", "a", "ab", "abc", "Hello, World!", "Binary\x00data\xff"]
  traverse_ (\s => do
    case decode Standard (encode Standard s) of
      Ok decoded => assertTrue ("roundtrip \"" ++ s ++ "\"") (decoded == s)
      Err _ => putStrLn $ "  ✗ roundtrip \"" ++ s ++ "\" failed"
    ) testStrings

  -- Length calculation tests
  putStrLn "\n[Length Calculation]"
  assertEq "encodedLength 0 = 0" 0 (encodedLength 0)
  assertEq "encodedLength 1 = 4" 4 (encodedLength 1)
  assertEq "encodedLength 2 = 4" 4 (encodedLength 2)
  assertEq "encodedLength 3 = 4" 4 (encodedLength 3)
  assertEq "encodedLength 4 = 8" 8 (encodedLength 4)

  -- Validation tests
  putStrLn "\n[Validation]"
  assertTrue "isValidBase64 \"SGVsbG8=\" = True" (isValidBase64 Standard "SGVsbG8=")
  assertTrue "isValidBase64 \"SGVsbG8\" = True (no padding)" (isValidBase64 Standard "SGVsbG8")
  assertTrue "isValidBase64 \"!!!\" = False" (not $ isValidBase64 Standard "!!!")

  -- Data URI tests
  putStrLn "\n[Data URI]"
  assertEq "toDataURI creates valid URI"
           "data:text/plain;base64,SGVsbG8=" (toDataURI "text/plain" "Hello")
  assertOk "fromDataURI parses valid URI"
           (fromDataURI "data:text/plain;base64,SGVsbG8=")
  assertErr "fromDataURI rejects invalid"
           (fromDataURI "not a data uri")

  putStrLn "\n✓ SafeBase64 unit tests complete"
