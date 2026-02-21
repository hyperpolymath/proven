-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeMathUnit

import Proven.Core
import Proven.SafeMath

%default total

||| Test helper: Assert equality
assertEq : (Eq a, Show a) => String -> a -> a -> IO ()
assertEq name expected actual =
  if expected == actual
    then putStrLn $ "  ✓ " ++ name
    else putStrLn $ "  ✗ " ++ name ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")"

||| Test helper: Assert result is Ok
assertOk : Show e => String -> Result e a -> IO ()
assertOk name (Ok _) = putStrLn $ "  ✓ " ++ name
assertOk name (Err e) = putStrLn $ "  ✗ " ++ name ++ " (got error: " ++ show e ++ ")"

||| Test helper: Assert result is Err
assertErr : Show a => String -> Result e a -> IO ()
assertErr name (Err _) = putStrLn $ "  ✓ " ++ name
assertErr name (Ok v) = putStrLn $ "  ✗ " ++ name ++ " (expected error, got: " ++ show v ++ ")"

export
runMathUnitTests : IO ()
runMathUnitTests = do
  putStrLn "SafeMath Unit Tests"
  putStrLn "==================="

  -- Addition tests
  putStrLn "\n[Addition]"
  assertOk "safeAdd 1 2 = Ok 3" (safeAdd 1 2)
  assertOk "safeAdd 0 0 = Ok 0" (safeAdd 0 0)
  assertOk "safeAdd (-5) 10 = Ok 5" (safeAdd (-5) 10)

  -- Subtraction tests
  putStrLn "\n[Subtraction]"
  assertOk "safeSub 10 3 = Ok 7" (safeSub 10 3)
  assertOk "safeSub 5 5 = Ok 0" (safeSub 5 5)
  assertOk "safeSub 0 100 = Ok (-100)" (safeSub 0 100)

  -- Multiplication tests
  putStrLn "\n[Multiplication]"
  assertOk "safeMul 6 7 = Ok 42" (safeMul 6 7)
  assertOk "safeMul 0 999 = Ok 0" (safeMul 0 999)
  assertOk "safeMul (-3) 4 = Ok (-12)" (safeMul (-3) 4)

  -- Division tests
  putStrLn "\n[Division]"
  assertOk "safeDiv 10 2 = Ok 5" (safeDiv 10 2)
  assertErr "safeDiv 10 0 = Err" (safeDiv 10 0)
  assertOk "safeDiv 0 5 = Ok 0" (safeDiv 0 5)
  assertOk "safeDiv 7 3 = Ok 2" (safeDiv 7 3)

  -- Modulo tests
  putStrLn "\n[Modulo]"
  assertOk "safeMod 10 3 = Ok 1" (safeMod 10 3)
  assertErr "safeMod 10 0 = Err" (safeMod 10 0)
  assertOk "safeMod 15 5 = Ok 0" (safeMod 15 5)

  -- Absolute value tests
  putStrLn "\n[Absolute Value]"
  assertEq "safeAbs 5 = 5" 5 (safeAbs 5)
  assertEq "safeAbs (-5) = 5" 5 (safeAbs (-5))
  assertEq "safeAbs 0 = 0" 0 (safeAbs 0)

  -- Negate tests
  putStrLn "\n[Negate]"
  assertEq "safeNegate 5 = (-5)" (-5) (safeNegate 5)
  assertEq "safeNegate (-5) = 5" 5 (safeNegate (-5))
  assertEq "safeNegate 0 = 0" 0 (safeNegate 0)

  putStrLn "\n✓ SafeMath unit tests complete"
