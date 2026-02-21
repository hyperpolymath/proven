-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeCryptoBench

import Proven.Core
import Proven.SafeCrypto
import System.Clock
import Data.List

%default total

iterations : Nat
iterations = 1000

testData : String
testData = "benchmark test data for cryptographic operations"

||| Benchmark SHA-256 hashing
benchSHA256 : IO Double
benchSHA256 = do
  start <- clockTime Monotonic
  let _ = map (\_ => sha256 testData) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark SHA-512 hashing
benchSHA512 : IO Double
benchSHA512 = do
  start <- clockTime Monotonic
  let _ = map (\_ => sha512 testData) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark HMAC-SHA256
benchHMAC : IO Double
benchHMAC = do
  start <- clockTime Monotonic
  let _ = map (\_ => hmacSHA256 "secret-key" testData) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark random bytes generation
benchRandomBytes : IO Double
benchRandomBytes = do
  start <- clockTime Monotonic
  let _ = map (\_ => randomBytes 32) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark constant-time comparison
benchConstantTimeEq : IO Double
benchConstantTimeEq = do
  start <- clockTime Monotonic
  let hash1 = sha256 testData
  let hash2 = sha256 testData
  let _ = map (\_ => constantTimeEq hash1 hash2) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Run all SafeCrypto benchmarks
export
runCryptoBenchmarks : IO ()
runCryptoBenchmarks = do
  putStrLn "SafeCrypto Benchmarks"
  putStrLn "====================="
  putStrLn $ "Iterations: " ++ show iterations
  putStrLn ""

  sha256Time <- benchSHA256
  putStrLn $ "SHA-256:         " ++ show sha256Time ++ " ms"

  sha512Time <- benchSHA512
  putStrLn $ "SHA-512:         " ++ show sha512Time ++ " ms"

  hmacTime <- benchHMAC
  putStrLn $ "HMAC-SHA256:     " ++ show hmacTime ++ " ms"

  randomTime <- benchRandomBytes
  putStrLn $ "randomBytes(32): " ++ show randomTime ++ " ms"

  ctEqTime <- benchConstantTimeEq
  putStrLn $ "constantTimeEq:  " ++ show ctEqTime ++ " ms"

  putStrLn ""
  putStrLn $ "Throughput (SHA-256): " ++ show (cast iterations * 1000.0 / sha256Time) ++ " ops/sec"
