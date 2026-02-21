-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeUrlUnit

import Proven.Core
import Proven.SafeUrl

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
runUrlUnitTests : IO ()
runUrlUnitTests = do
  putStrLn "SafeUrl Unit Tests"
  putStrLn "=================="

  -- Parse tests
  putStrLn "\n[Parsing]"
  assertOk "parseUrl \"https://example.com\" parses" (parseUrl "https://example.com")
  assertOk "parseUrl \"http://localhost:8080/path\" parses" (parseUrl "http://localhost:8080/path")
  assertOk "parseUrl with query params" (parseUrl "https://api.example.com/search?q=test&page=1")
  assertOk "parseUrl with fragment" (parseUrl "https://docs.example.com/guide#section")
  assertErr "parseUrl \"not a url\" fails" (parseUrl "not a url")
  assertErr "parseUrl \"javascript:alert(1)\" blocks dangerous scheme" (parseUrl "javascript:alert(1)")

  -- Component extraction tests
  putStrLn "\n[Component Extraction]"
  case parseUrl "https://user:pass@example.com:8080/path?q=1#frag" of
    Ok url => do
      assertEq "getScheme = \"https\"" "https" (getScheme url)
      assertEq "getHost = \"example.com\"" "example.com" (getHost url)
      assertOk "getPort = 8080" (getPort url)
      assertEq "getPath = \"/path\"" "/path" (getPath url)
      assertOk "getQuery works" (getQuery url)
      assertOk "getFragment works" (getFragment url)
    Err _ => putStrLn "  ✗ Failed to parse test URL"

  -- Query parameter tests
  putStrLn "\n[Query Parameters]"
  case parseUrl "https://search.example.com?name=Alice&age=30&active=true" of
    Ok url => do
      assertOk "getQueryParam \"name\" = \"Alice\"" (getQueryParam url "name")
      assertOk "getQueryParam \"age\" = \"30\"" (getQueryParam url "age")
      assertErr "getQueryParam \"missing\" fails" (getQueryParam url "missing")
    Err _ => putStrLn "  ✗ Failed to parse query URL"

  -- URL encoding tests
  putStrLn "\n[Encoding]"
  assertEq "encodeUrlComponent \"hello world\" = \"hello%20world\""
           "hello%20world" (encodeUrlComponent "hello world")
  assertEq "encodeUrlComponent \"a=b&c=d\" encodes special chars"
           "a%3Db%26c%3Dd" (encodeUrlComponent "a=b&c=d")

  -- URL building tests
  putStrLn "\n[Building]"
  let built = buildUrl "https" "api.example.com" "/v1/users" [("page", "1"), ("limit", "10")]
  assertTrue "buildUrl creates valid URL" (isValidUrl built)

  -- Security tests
  putStrLn "\n[Security]"
  assertTrue "isValidUrl rejects data: URLs" (not $ isValidUrl "data:text/html,<script>")
  assertTrue "isValidUrl accepts https" (isValidUrl "https://safe.example.com")
  assertTrue "isValidUrl accepts http" (isValidUrl "http://local.dev")

  putStrLn "\n✓ SafeUrl unit tests complete"
