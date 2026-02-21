-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeJsonUnit

import Proven.Core
import Proven.SafeJson

%default total

assertOk : Show e => String -> Result e a -> IO ()
assertOk name (Ok _) = putStrLn $ "  ✓ " ++ name
assertOk name (Err e) = putStrLn $ "  ✗ " ++ name ++ " (got error: " ++ show e ++ ")"

assertErr : Show a => String -> Result e a -> IO ()
assertErr name (Err _) = putStrLn $ "  ✓ " ++ name
assertErr name (Ok v) = putStrLn $ "  ✗ " ++ name ++ " (expected error, got: " ++ show v ++ ")"

assertTrue : String -> Bool -> IO ()
assertTrue name True = putStrLn $ "  ✓ " ++ name
assertTrue name False = putStrLn $ "  ✗ " ++ name ++ " (expected True)"

export
runJsonUnitTests : IO ()
runJsonUnitTests = do
  putStrLn "SafeJson Unit Tests"
  putStrLn "==================="

  -- Parse tests
  putStrLn "\n[Parsing]"
  assertOk "parseJson \"{\\\"key\\\": \\\"value\\\"}\" parses object" (parseJson "{\"key\": \"value\"}")
  assertOk "parseJson \"[1, 2, 3]\" parses array" (parseJson "[1, 2, 3]")
  assertOk "parseJson \"null\" parses null" (parseJson "null")
  assertOk "parseJson \"true\" parses boolean" (parseJson "true")
  assertOk "parseJson \"42\" parses number" (parseJson "42")
  assertOk "parseJson \"\\\"string\\\"\" parses string" (parseJson "\"string\"")
  assertErr "parseJson \"invalid\" fails" (parseJson "invalid json {")

  -- Type access tests
  putStrLn "\n[Type-Safe Access]"
  let validJson = "{\"name\": \"Alice\", \"age\": 30, \"active\": true}"
  case parseJson validJson of
    Ok json => do
      assertOk "getString \"name\" works" (getString json "name")
      assertOk "getInt \"age\" works" (getInt json "age")
      assertOk "getBool \"active\" works" (getBool json "active")
      assertErr "getString \"missing\" fails" (getString json "missing")
      assertErr "getInt \"name\" type mismatch fails" (getInt json "name")
    Err _ => putStrLn "  ✗ Failed to parse test JSON"

  -- Nested access tests
  putStrLn "\n[Nested Access]"
  let nestedJson = "{\"user\": {\"profile\": {\"name\": \"Bob\"}}}"
  case parseJson nestedJson of
    Ok json => do
      assertOk "getPath [\"user\", \"profile\", \"name\"] works" (getPath json ["user", "profile", "name"])
      assertErr "getPath [\"user\", \"invalid\"] fails" (getPath json ["user", "invalid"])
    Err _ => putStrLn "  ✗ Failed to parse nested JSON"

  -- Array access tests
  putStrLn "\n[Array Access]"
  let arrayJson = "{\"items\": [10, 20, 30]}"
  case parseJson arrayJson of
    Ok json => do
      assertOk "getArray \"items\" works" (getArray json "items")
      assertOk "getArrayIndex 0 works" (getArrayIndex json "items" 0)
      assertErr "getArrayIndex 99 out of bounds" (getArrayIndex json "items" 99)
    Err _ => putStrLn "  ✗ Failed to parse array JSON"

  -- Depth limit tests
  putStrLn "\n[Depth Limits]"
  assertTrue "isValidJson accepts reasonable depth" (isValidJson "{\"a\": {\"b\": {\"c\": 1}}}")

  putStrLn "\n✓ SafeJson unit tests complete"
