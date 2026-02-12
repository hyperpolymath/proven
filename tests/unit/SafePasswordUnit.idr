-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafePasswordUnit

import Proven.Core
import Proven.SafePassword

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
runPasswordUnitTests : IO ()
runPasswordUnitTests = do
  putStrLn "SafePassword Unit Tests"
  putStrLn "======================="

  -- Policy validation tests
  putStrLn "\n[Policy Validation]"
  assertOk "NIST policy accepts strong password" (validatePassword NISTPolicy "MyStr0ng!Pass")
  assertErr "NIST policy rejects short password" (validatePassword NISTPolicy "weak")
  assertErr "NIST policy rejects no digits" (validatePassword NISTPolicy "NoDigitsHere!")

  assertOk "PCI-DSS policy accepts complex password" (validatePassword PCIDSSPolicy "C0mpl3x!Pass#2025")
  assertErr "PCI-DSS policy rejects simple" (validatePassword PCIDSSPolicy "simple123")

  -- Strength analysis tests
  putStrLn "\n[Strength Analysis]"
  assertEq "analyzeStrength \"password\" = VeryWeak" VeryWeak (analyzeStrength "password")
  assertEq "analyzeStrength \"Password1\" = Weak" Weak (analyzeStrength "Password1")
  assertEq "analyzeStrength \"MyP@ssw0rd!\" = Medium" Medium (analyzeStrength "MyP@ssw0rd!")
  assertEq "analyzeStrength \"xK9$mN2@pL5!qR8#\" = Strong" Strong (analyzeStrength "xK9$mN2@pL5!qR8#")

  -- Pattern detection tests
  putStrLn "\n[Pattern Detection]"
  assertTrue "detectPatterns finds keyboard" (KeyboardPattern `elem` detectPatterns "qwerty123")
  assertTrue "detectPatterns finds sequence" (SequentialPattern `elem` detectPatterns "abc123")
  assertTrue "detectPatterns finds repeated" (RepeatedPattern `elem` detectPatterns "aaaaaa")
  assertTrue "detectPatterns finds date" (DatePattern `elem` detectPatterns "19901225")

  -- Common password detection tests
  putStrLn "\n[Common Password Detection]"
  assertTrue "isCommonPassword \"password\"" (isCommonPassword "password")
  assertTrue "isCommonPassword \"123456\"" (isCommonPassword "123456")
  assertTrue "isCommonPassword \"qwerty\"" (isCommonPassword "qwerty")
  assertTrue "isCommonPassword unique" (not $ isCommonPassword "xK9$mN2@pL5!qR8#vB")

  -- Hash algorithm tests
  putStrLn "\n[Hash Algorithms]"
  assertOk "hashPassword Argon2id" (hashPassword Argon2id "password")
  assertOk "hashPassword Bcrypt" (hashPassword Bcrypt "password")
  assertOk "hashPassword Scrypt" (hashPassword Scrypt "password")
  assertOk "hashPassword PBKDF2" (hashPassword PBKDF2 "password")

  -- Verify tests
  putStrLn "\n[Password Verification]"
  case hashPassword Argon2id "secret123" of
    Ok hashed => assertTrue "verifyPassword correct" (verifyPassword "secret123" hashed)
    Err _ => putStrLn "  ✗ Failed to hash"
  case hashPassword Argon2id "secret123" of
    Ok hashed => assertTrue "verifyPassword wrong" (not $ verifyPassword "wrong" hashed)
    Err _ => putStrLn "  ✗ Failed to hash"

  -- Crack time estimation tests
  putStrLn "\n[Crack Time Estimation]"
  let weakTime = estimateCrackTime "password"
  let strongTime = estimateCrackTime "xK9$mN2@pL5!qR8#vB7&"
  assertTrue "weak password cracks quickly" (weakTime < 86400)  -- < 1 day
  assertTrue "strong password takes long" (strongTime > 31536000000)  -- > 1000 years

  -- Entropy calculation tests
  putStrLn "\n[Entropy]"
  assertTrue "entropy(weak) < entropy(strong)"
             (calculateEntropy "password" < calculateEntropy "xK9$mN2@pL5!qR8#")

  putStrLn "\n✓ SafePassword unit tests complete"
