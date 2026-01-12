-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeCryptoUnit

import Proven.Core
import Proven.SafeCrypto

%default total

assertOk : Show e => String -> Result e a -> IO ()
assertOk name (Ok _) = putStrLn $ "  ✓ " ++ name
assertOk name (Err e) = putStrLn $ "  ✗ " ++ name ++ " (got error: " ++ show e ++ ")"

assertEq : (Eq a, Show a) => String -> a -> a -> IO ()
assertEq name expected actual =
  if expected == actual
    then putStrLn $ "  ✓ " ++ name
    else putStrLn $ "  ✗ " ++ name ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")"

assertTrue : String -> Bool -> IO ()
assertTrue name True = putStrLn $ "  ✓ " ++ name
assertTrue name False = putStrLn $ "  ✗ " ++ name ++ " (expected True)"

export
runCryptoUnitTests : IO ()
runCryptoUnitTests = do
  putStrLn "SafeCrypto Unit Tests"
  putStrLn "====================="

  -- Hash algorithm tests
  putStrLn "\n[Hash Algorithms]"
  assertOk "hash SHA256 \"hello\"" (hash SHA256 "hello")
  assertOk "hash SHA384 \"hello\"" (hash SHA384 "hello")
  assertOk "hash SHA512 \"hello\"" (hash SHA512 "hello")
  assertOk "hash SHA3_256 \"hello\"" (hash SHA3_256 "hello")
  assertOk "hash BLAKE2b \"hello\"" (hash BLAKE2b "hello")

  -- Hash length tests
  putStrLn "\n[Hash Lengths]"
  case hash SHA256 "test" of
    Ok h => assertEq "SHA256 output length" 64 (length h)  -- hex encoded
    Err _ => putStrLn "  ✗ Failed to hash"
  case hash SHA512 "test" of
    Ok h => assertEq "SHA512 output length" 128 (length h)
    Err _ => putStrLn "  ✗ Failed to hash"

  -- HMAC tests
  putStrLn "\n[HMAC]"
  assertOk "hmac SHA256 key message" (hmac SHA256 "secret" "message")
  assertOk "hmac SHA512 key message" (hmac SHA512 "secret" "message")

  -- Determinism tests
  putStrLn "\n[Determinism]"
  case (hash SHA256 "same input", hash SHA256 "same input") of
    (Ok h1, Ok h2) => assertTrue "same input = same hash" (h1 == h2)
    _ => putStrLn "  ✗ Failed to hash"
  case (hash SHA256 "input1", hash SHA256 "input2") of
    (Ok h1, Ok h2) => assertTrue "different input = different hash" (h1 /= h2)
    _ => putStrLn "  ✗ Failed to hash"

  -- Secure random tests (deterministic stub for testing)
  putStrLn "\n[Secure Random]"
  assertOk "randomBytes 16" (randomBytes 16)
  assertOk "randomBytes 32" (randomBytes 32)
  case randomBytes 32 of
    Ok bytes => assertEq "randomBytes length" 64 (length bytes)  -- hex
    Err _ => putStrLn "  ✗ Failed to generate random"

  -- Constant time comparison tests
  putStrLn "\n[Constant Time Comparison]"
  assertTrue "secureCompare equal" (secureCompare "abc123" "abc123")
  assertTrue "secureCompare different" (not $ secureCompare "abc123" "xyz789")
  assertTrue "secureCompare different length" (not $ secureCompare "short" "longerstring")

  putStrLn "\n✓ SafeCrypto unit tests complete"
