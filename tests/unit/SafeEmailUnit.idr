-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeEmailUnit

import Proven.Core
import Proven.SafeEmail

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
runEmailUnitTests : IO ()
runEmailUnitTests = do
  putStrLn "SafeEmail Unit Tests"
  putStrLn "===================="

  -- Valid email tests
  putStrLn "\n[Valid Emails]"
  assertOk "parseEmail \"user@example.com\" parses" (parseEmail "user@example.com")
  assertOk "parseEmail \"user.name@example.com\" parses" (parseEmail "user.name@example.com")
  assertOk "parseEmail \"user+tag@example.com\" parses" (parseEmail "user+tag@example.com")
  assertOk "parseEmail \"user@sub.example.com\" parses" (parseEmail "user@sub.example.com")
  assertOk "parseEmail \"USER@EXAMPLE.COM\" parses" (parseEmail "USER@EXAMPLE.COM")

  -- Invalid email tests
  putStrLn "\n[Invalid Emails]"
  assertErr "parseEmail \"\" fails" (parseEmail "")
  assertErr "parseEmail \"invalid\" fails" (parseEmail "invalid")
  assertErr "parseEmail \"@example.com\" fails" (parseEmail "@example.com")
  assertErr "parseEmail \"user@\" fails" (parseEmail "user@")
  assertErr "parseEmail \"user@@example.com\" fails" (parseEmail "user@@example.com")
  assertErr "parseEmail \"user@.com\" fails" (parseEmail "user@.com")
  assertErr "parseEmail \"user @example.com\" fails" (parseEmail "user @example.com")

  -- Component extraction tests
  putStrLn "\n[Component Extraction]"
  case parseEmail "alice.smith+work@mail.example.org" of
    Ok email => do
      assertEq "getLocalPart = \"alice.smith+work\"" "alice.smith+work" (getLocalPart email)
      assertEq "getDomain = \"mail.example.org\"" "mail.example.org" (getDomain email)
    Err _ => putStrLn "  ✗ Failed to parse test email"

  -- Normalization tests
  putStrLn "\n[Normalization]"
  case parseEmail "USER@EXAMPLE.COM" of
    Ok email => do
      assertEq "normalizeEmail lowercases domain" "USER@example.com" (normalizeEmail email)
    Err _ => putStrLn "  ✗ Failed to parse normalization test email"

  -- Validation severity tests
  putStrLn "\n[Validation Severity]"
  assertTrue "isValidEmail \"valid@example.com\" = True" (isValidEmail "valid@example.com")
  assertTrue "isValidEmail \"invalid\" = False" (not $ isValidEmail "invalid")

  -- Edge cases
  putStrLn "\n[Edge Cases]"
  assertOk "parseEmail handles numeric domain" (parseEmail "user@123.456.789.0")
  assertOk "parseEmail handles long local part" (parseEmail "a.very.long.local.part.name@example.com")

  putStrLn "\n✓ SafeEmail unit tests complete"
