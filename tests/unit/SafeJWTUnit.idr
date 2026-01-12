-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeJWTUnit

import Proven.Core
import Proven.SafeJWT

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
runJWTUnitTests : IO ()
runJWTUnitTests = do
  putStrLn "SafeJWT Unit Tests"
  putStrLn "=================="

  -- Decode tests
  putStrLn "\n[Decoding]"
  -- Valid JWT structure (header.payload.signature)
  let validJwt = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"
  assertOk "decodeJwt parses valid JWT" (decodeJwt validJwt)

  assertErr "decodeJwt rejects malformed token" (decodeJwt "not.a.jwt")
  assertErr "decodeJwt rejects single segment" (decodeJwt "onlyone")
  assertErr "decodeJwt rejects empty string" (decodeJwt "")

  -- Header parsing tests
  putStrLn "\n[Header Parsing]"
  case decodeJwt validJwt of
    Ok jwt => do
      assertEq "getAlgorithm = HS256" HS256 (getAlgorithm jwt)
      assertEq "getType = JWT" "JWT" (getType jwt)
    Err _ => putStrLn "  ✗ Failed to decode test JWT"

  -- Claims access tests
  putStrLn "\n[Claims Access]"
  case decodeJwt validJwt of
    Ok jwt => do
      assertOk "getClaim \"sub\" works" (getClaim jwt "sub")
      assertOk "getClaim \"name\" works" (getClaim jwt "name")
      assertOk "getClaim \"iat\" works" (getClaim jwt "iat")
      assertErr "getClaim \"missing\" fails" (getClaim jwt "missing")
    Err _ => putStrLn "  ✗ Failed to decode test JWT"

  -- Validation tests (time-based)
  putStrLn "\n[Validation]"
  let options = defaultValidationOptions
  -- Note: These test structural validation, not actual signature verification
  assertTrue "isStructurallyValid accepts valid JWT" (isStructurallyValid validJwt)
  assertTrue "isStructurallyValid rejects malformed" (not $ isStructurallyValid "bad")

  -- Algorithm safety tests
  putStrLn "\n[Algorithm Safety]"
  assertTrue "isSecureAlgorithm HS256 = True" (isSecureAlgorithm HS256)
  assertTrue "isSecureAlgorithm RS256 = True" (isSecureAlgorithm RS256)
  assertTrue "isSecureAlgorithm ES256 = True" (isSecureAlgorithm ES256)
  assertTrue "isSecureAlgorithm None = False" (not $ isSecureAlgorithm None)

  -- Base64URL tests
  putStrLn "\n[Base64URL]"
  assertEq "base64UrlEncode \"hello\"" "aGVsbG8" (base64UrlEncode "hello")
  assertOk "base64UrlDecode \"aGVsbG8\"" (base64UrlDecode "aGVsbG8")

  -- Token building tests
  putStrLn "\n[Token Building]"
  let claims = [("sub", "user123"), ("role", "admin")]
  assertOk "buildClaims creates valid structure" (buildClaims claims)

  putStrLn "\n✓ SafeJWT unit tests complete"
