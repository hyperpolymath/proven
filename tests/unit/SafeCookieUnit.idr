-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeCookieUnit

import Proven.Core
import Proven.SafeCookie

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
runCookieUnitTests : IO ()
runCookieUnitTests = do
  putStrLn "SafeCookie Unit Tests"
  putStrLn "====================="

  -- Cookie parsing tests
  putStrLn "\n[Cookie Parsing]"
  assertOk "parseCookie \"name=value\"" (parseCookie "name=value")
  assertOk "parseCookie \"session=abc123\"" (parseCookie "session=abc123")
  assertOk "parseCookie with special chars" (parseCookie "data=hello%20world")
  assertErr "parseCookie \"\" empty" (parseCookie "")
  assertErr "parseCookie \"noequals\" invalid" (parseCookie "noequals")
  assertErr "parseCookie \"=value\" no name" (parseCookie "=value")

  -- Cookie value extraction tests
  putStrLn "\n[Value Extraction]"
  case parseCookie "session=xyz789" of
    Ok c => do
      assertEq "getName = \"session\"" "session" (getName c)
      assertEq "getValue = \"xyz789\"" "xyz789" (getValue c)
    Err _ => putStrLn "  ✗ Failed to parse cookie"

  -- Set-Cookie parsing tests
  putStrLn "\n[Set-Cookie Parsing]"
  assertOk "parseSetCookie simple" (parseSetCookie "name=value")
  assertOk "parseSetCookie with attributes" (parseSetCookie "name=value; Path=/; Secure; HttpOnly")
  assertOk "parseSetCookie with expires" (parseSetCookie "name=value; Expires=Thu, 01 Jan 2030 00:00:00 GMT")
  assertOk "parseSetCookie with max-age" (parseSetCookie "name=value; Max-Age=3600")
  assertOk "parseSetCookie with domain" (parseSetCookie "name=value; Domain=example.com")
  assertOk "parseSetCookie with SameSite" (parseSetCookie "name=value; SameSite=Strict")

  -- Attribute extraction tests
  putStrLn "\n[Attribute Extraction]"
  case parseSetCookie "session=abc; Path=/app; Secure; HttpOnly; SameSite=Lax" of
    Ok c => do
      assertEq "getPath = \"/app\"" (Just "/app") (getPath c)
      assertTrue "isSecure = True" (isSecure c)
      assertTrue "isHttpOnly = True" (isHttpOnly c)
      assertEq "getSameSite = Lax" (Just Lax) (getSameSite c)
    Err _ => putStrLn "  ✗ Failed to parse Set-Cookie"

  -- Cookie building tests
  putStrLn "\n[Cookie Building]"
  let built = cookie "session" "abc123"
              |> withPath "/"
              |> withDomain "example.com"
              |> withSecure
              |> withHttpOnly
              |> withSameSite Strict
              |> withMaxAge 3600
              |> build
  assertOk "cookie builder creates valid cookie" built

  -- SameSite tests
  putStrLn "\n[SameSite Attribute]"
  case parseSetCookie "name=value; SameSite=Strict" of
    Ok c => assertEq "SameSite=Strict" (Just Strict) (getSameSite c)
    Err _ => putStrLn "  ✗ Failed to parse"
  case parseSetCookie "name=value; SameSite=Lax" of
    Ok c => assertEq "SameSite=Lax" (Just Lax) (getSameSite c)
    Err _ => putStrLn "  ✗ Failed to parse"
  case parseSetCookie "name=value; SameSite=None; Secure" of
    Ok c => assertEq "SameSite=None" (Just None) (getSameSite c)
    Err _ => putStrLn "  ✗ Failed to parse"

  -- Security validation tests
  putStrLn "\n[Security Validation]"
  -- SameSite=None requires Secure
  assertErr "SameSite=None without Secure rejected"
            (validateCookie (parseSetCookie "name=value; SameSite=None"))

  let secureCookie = cookie "session" "xyz"
                     |> withSecure
                     |> withHttpOnly
                     |> withSameSite Strict
                     |> build
  assertOk "secure cookie passes validation" (validateCookie secureCookie)

  -- Multiple cookies parsing tests
  putStrLn "\n[Multiple Cookies]"
  let cookieHeader = "session=abc; user=john; theme=dark"
  assertOk "parseCookies multiple" (parseCookies cookieHeader)
  case parseCookies cookieHeader of
    Ok cs => do
      assertEq "count = 3" 3 (length cs)
      assertOk "getCookieByName \"session\"" (getCookieByName cs "session")
      assertOk "getCookieByName \"user\"" (getCookieByName cs "user")
      assertErr "getCookieByName \"missing\"" (getCookieByName cs "missing")
    Err _ => putStrLn "  ✗ Failed to parse cookies"

  -- Formatting tests
  putStrLn "\n[Formatting]"
  case cookie "name" "value" |> withPath "/" |> withSecure |> build of
    Ok c => assertTrue "formatSetCookie includes attributes"
                       (contains "Secure" (formatSetCookie c))
    Err _ => putStrLn "  ✗ Failed to build cookie"

  putStrLn "\n✓ SafeCookie unit tests complete"
