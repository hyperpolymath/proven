-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeUUIDUnit

import Proven.Core
import Proven.SafeUUID

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
runUUIDUnitTests : IO ()
runUUIDUnitTests = do
  putStrLn "SafeUUID Unit Tests"
  putStrLn "==================="

  -- Parse tests
  putStrLn "\n[Parsing]"
  assertOk "parseUUID standard format" (parseUUID "550e8400-e29b-41d4-a716-446655440000")
  assertOk "parseUUID uppercase" (parseUUID "550E8400-E29B-41D4-A716-446655440000")
  assertOk "parseUUID lowercase" (parseUUID "550e8400-e29b-41d4-a716-446655440000")
  assertErr "parseUUID invalid length" (parseUUID "550e8400-e29b")
  assertErr "parseUUID invalid chars" (parseUUID "550e8400-e29b-41d4-a716-44665544000g")
  assertErr "parseUUID no hyphens" (parseUUID "550e8400e29b41d4a716446655440000")
  assertErr "parseUUID empty" (parseUUID "")

  -- Nil UUID tests
  putStrLn "\n[Nil UUID]"
  assertOk "parseUUID nil UUID" (parseUUID "00000000-0000-0000-0000-000000000000")
  case parseUUID "00000000-0000-0000-0000-000000000000" of
    Ok uuid => assertTrue "isNil nil UUID" (isNil uuid)
    Err _ => putStrLn "  ✗ Failed to parse nil UUID"

  -- Version detection tests
  putStrLn "\n[Version Detection]"
  case parseUUID "550e8400-e29b-41d4-a716-446655440000" of
    Ok uuid => assertEq "getVersion v4 UUID" (Just 4) (getVersion uuid)
    Err _ => putStrLn "  ✗ Failed to parse v4 UUID"

  case parseUUID "6ba7b810-9dad-11d1-80b4-00c04fd430c8" of
    Ok uuid => assertEq "getVersion v1 UUID" (Just 1) (getVersion uuid)
    Err _ => putStrLn "  ✗ Failed to parse v1 UUID"

  -- Format tests
  putStrLn "\n[Formatting]"
  case parseUUID "550E8400-E29B-41D4-A716-446655440000" of
    Ok uuid => do
      assertEq "formatUUID lowercase" "550e8400-e29b-41d4-a716-446655440000" (formatUUID Lowercase uuid)
      assertEq "formatUUID uppercase" "550E8400-E29B-41D4-A716-446655440000" (formatUUID Uppercase uuid)
      assertEq "formatUUID compact" "550e8400e29b41d4a716446655440000" (formatUUID Compact uuid)
      assertEq "formatUUID URN" "urn:uuid:550e8400-e29b-41d4-a716-446655440000" (formatUUID URN uuid)
    Err _ => putStrLn "  ✗ Failed to parse format test UUID"

  -- Validation tests
  putStrLn "\n[Validation]"
  assertTrue "isValidUUID valid" (isValidUUID "550e8400-e29b-41d4-a716-446655440000")
  assertTrue "isValidUUID invalid" (not $ isValidUUID "not-a-uuid")
  assertTrue "isValidUUID empty" (not $ isValidUUID "")

  -- Generation tests (v4)
  putStrLn "\n[Generation]"
  -- Note: In pure Idris, we use deterministic generation for testing
  let uuid1 = generateV4 "seed1"
  let uuid2 = generateV4 "seed2"
  assertTrue "generateV4 produces valid UUID" (isValidUUID (formatUUID Standard uuid1))
  assertTrue "generateV4 different seeds differ" (formatUUID Standard uuid1 /= formatUUID Standard uuid2)

  case uuid1 of
    uuid => assertEq "generateV4 version is 4" (Just 4) (getVersion uuid)

  -- Comparison tests
  putStrLn "\n[Comparison]"
  case (parseUUID "00000000-0000-0000-0000-000000000001", parseUUID "00000000-0000-0000-0000-000000000002") of
    (Ok u1, Ok u2) => do
      assertTrue "UUID comparison u1 < u2" (compareUUID u1 u2 == LT)
      assertTrue "UUID comparison u2 > u1" (compareUUID u2 u1 == GT)
    _ => putStrLn "  ✗ Failed to parse comparison UUIDs"

  case (parseUUID "550e8400-e29b-41d4-a716-446655440000", parseUUID "550e8400-e29b-41d4-a716-446655440000") of
    (Ok u1, Ok u2) => assertTrue "UUID equality" (compareUUID u1 u2 == EQ)
    _ => putStrLn "  ✗ Failed to parse equality UUIDs"

  putStrLn "\n✓ SafeUUID unit tests complete"
