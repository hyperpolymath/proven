-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeHeaderUnit

import Proven.Core
import Proven.SafeHeader

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
runHeaderUnitTests : IO ()
runHeaderUnitTests = do
  putStrLn "SafeHeader Unit Tests"
  putStrLn "====================="

  -- Header name validation tests
  putStrLn "\n[Header Name Validation]"
  assertOk "validateHeaderName \"Content-Type\"" (validateHeaderName "Content-Type")
  assertOk "validateHeaderName \"X-Custom-Header\"" (validateHeaderName "X-Custom-Header")
  assertOk "validateHeaderName \"Accept\"" (validateHeaderName "Accept")
  assertErr "validateHeaderName \"\" empty" (validateHeaderName "")
  assertErr "validateHeaderName with CRLF" (validateHeaderName "Header\r\nInjection")
  assertErr "validateHeaderName with colon" (validateHeaderName "Header:Name")

  -- Header value validation tests
  putStrLn "\n[Header Value Validation]"
  assertOk "validateHeaderValue \"application/json\"" (validateHeaderValue "application/json")
  assertOk "validateHeaderValue \"text/html; charset=utf-8\"" (validateHeaderValue "text/html; charset=utf-8")
  assertErr "validateHeaderValue with CRLF" (validateHeaderValue "value\r\nX-Injected: evil")
  assertErr "validateHeaderValue with null" (validateHeaderValue "value\x00null")

  -- CRLF injection detection tests
  putStrLn "\n[CRLF Injection Detection]"
  assertTrue "hasCRLF \"normal\" = False" (not $ hasCRLF "normal value")
  assertTrue "hasCRLF with \\r\\n = True" (hasCRLF "value\r\ninjected")
  assertTrue "hasCRLF with \\r = True" (hasCRLF "value\rinjected")
  assertTrue "hasCRLF with \\n = True" (hasCRLF "value\ninjected")

  -- Header parsing tests
  putStrLn "\n[Header Parsing]"
  assertOk "parseHeader \"Content-Type: application/json\"" (parseHeader "Content-Type: application/json")
  assertOk "parseHeader with spaces" (parseHeader "Accept:   text/html")
  assertErr "parseHeader no colon" (parseHeader "InvalidHeader")
  assertErr "parseHeader empty name" (parseHeader ": value")

  -- Header building tests
  putStrLn "\n[Header Building]"
  let h1 = header "Content-Type" "application/json"
  assertOk "header builder creates valid header" h1
  case h1 of
    Ok hdr => assertEq "formatHeader" "Content-Type: application/json" (formatHeader hdr)
    Err _ => putStrLn "  ✗ Failed to build header"

  -- Common headers tests
  putStrLn "\n[Common Headers]"
  assertOk "contentType \"application/json\"" (contentType "application/json")
  assertOk "accept \"text/html\"" (accept "text/html")
  assertOk "authorization \"Bearer token\"" (authorization "Bearer token")
  assertOk "cacheControl \"no-cache\"" (cacheControl "no-cache")

  -- Security header tests
  putStrLn "\n[Security Headers]"
  assertOk "strictTransportSecurity maxAge=31536000" (strictTransportSecurity 31536000 True True)
  assertOk "contentSecurityPolicy default-src" (contentSecurityPolicy "default-src 'self'")
  assertOk "xContentTypeOptions nosniff" xContentTypeOptions
  assertOk "xFrameOptions DENY" (xFrameOptions Deny)
  assertOk "xXSSProtection" xXSSProtection

  -- Header list tests
  putStrLn "\n[Header List]"
  let headers = buildHeaders
                [ ("Content-Type", "application/json")
                , ("Accept", "application/json")
                , ("Authorization", "Bearer xyz")
                ]
  assertOk "buildHeaders creates valid list" headers
  case headers of
    Ok hs => do
      assertOk "getHeader finds existing" (getHeader hs "Content-Type")
      assertErr "getHeader missing" (getHeader hs "X-Missing")
    Err _ => putStrLn "  ✗ Failed to build headers"

  -- Case insensitivity tests
  putStrLn "\n[Case Insensitivity]"
  case buildHeaders [("Content-Type", "text/plain")] of
    Ok hs => do
      assertOk "getHeader case insensitive lower" (getHeader hs "content-type")
      assertOk "getHeader case insensitive upper" (getHeader hs "CONTENT-TYPE")
    Err _ => putStrLn "  ✗ Failed to build headers"

  putStrLn "\n✓ SafeHeader unit tests complete"
