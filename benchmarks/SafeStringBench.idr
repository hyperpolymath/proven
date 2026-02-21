-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeStringBench

import Proven.Core
import Proven.SafeString
import System.Clock
import Data.List
import Data.String

%default total

iterations : Nat
iterations = 10000

testString : String
testString = "The quick brown fox jumps over the lazy dog"

||| Benchmark string truncation
benchTruncate : IO Double
benchTruncate = do
  start <- clockTime Monotonic
  let _ = map (\_ => truncate 20 testString) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark string sanitization
benchSanitize : IO Double
benchSanitize = do
  start <- clockTime Monotonic
  let _ = map (\_ => sanitize "<script>alert('xss')</script>" ) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark string validation
benchValidate : IO Double
benchValidate = do
  start <- clockTime Monotonic
  let _ = map (\_ => validateLength 10 100 testString) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark UTF-8 validation
benchUTF8Validate : IO Double
benchUTF8Validate = do
  start <- clockTime Monotonic
  let _ = map (\_ => validateUTF8 testString) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark string normalization
benchNormalize : IO Double
benchNormalize = do
  start <- clockTime Monotonic
  let _ = map (\_ => normalize "  hello   world  ") [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Run all SafeString benchmarks
export
runStringBenchmarks : IO ()
runStringBenchmarks = do
  putStrLn "SafeString Benchmarks"
  putStrLn "====================="
  putStrLn $ "Iterations: " ++ show iterations
  putStrLn ""

  truncTime <- benchTruncate
  putStrLn $ "truncate:    " ++ show truncTime ++ " ms"

  sanitizeTime <- benchSanitize
  putStrLn $ "sanitize:    " ++ show sanitizeTime ++ " ms"

  validateTime <- benchValidate
  putStrLn $ "validate:    " ++ show validateTime ++ " ms"

  utf8Time <- benchUTF8Validate
  putStrLn $ "UTF-8:       " ++ show utf8Time ++ " ms"

  normTime <- benchNormalize
  putStrLn $ "normalize:   " ++ show normTime ++ " ms"

  putStrLn ""
