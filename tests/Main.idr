-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module Main

import SafeMathProps
import SafeStringProps

||| Main test runner for all property tests
||| In Idris 2, property tests are primarily verified at compile time.
||| If this module type-checks, the proofs are valid.
main : IO ()
main = do
  putStrLn "========================================="
  putStrLn "   PROVEN - Property-Based Test Suite   "
  putStrLn "========================================="
  putStrLn ""

  runMathProperties
  putStrLn ""

  runStringProperties
  putStrLn ""

  putStrLn "========================================="
  putStrLn "   All compile-time proofs verified!    "
  putStrLn "========================================="
  putStrLn ""
  putStrLn "Note: In dependently-typed languages like Idris 2,"
  putStrLn "many properties are proven at compile time."
  putStrLn "If this program compiles, the proofs are valid."
