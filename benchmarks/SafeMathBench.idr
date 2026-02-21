-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeMathBench

import Proven.Core
import Proven.SafeMath
import System.Clock
import Data.List

%default total

||| Number of iterations for each benchmark
iterations : Nat
iterations = 100000

||| Benchmark safe addition
benchSafeAdd : IO Double
benchSafeAdd = do
  start <- clockTime Monotonic
  let _ = foldr (\i, acc => case safeAdd i acc of Ok r => r; _ => acc) 0 [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark safe multiplication
benchSafeMul : IO Double
benchSafeMul = do
  start <- clockTime Monotonic
  let _ = map (\i => safeMul i 7) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark safe division
benchSafeDiv : IO Double
benchSafeDiv = do
  start <- clockTime Monotonic
  let _ = map (\i => safeDiv i 7) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark overflow detection
benchOverflowDetection : IO Double
benchOverflowDetection = do
  start <- clockTime Monotonic
  let _ = map (\i => isOk (safeAdd maxInt (cast i))) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Run all SafeMath benchmarks
export
runMathBenchmarks : IO ()
runMathBenchmarks = do
  putStrLn "SafeMath Benchmarks"
  putStrLn "==================="
  putStrLn $ "Iterations: " ++ show iterations
  putStrLn ""

  addTime <- benchSafeAdd
  putStrLn $ "safeAdd:     " ++ show addTime ++ " ms"

  mulTime <- benchSafeMul
  putStrLn $ "safeMul:     " ++ show mulTime ++ " ms"

  divTime <- benchSafeDiv
  putStrLn $ "safeDiv:     " ++ show divTime ++ " ms"

  overflowTime <- benchOverflowDetection
  putStrLn $ "overflow:    " ++ show overflowTime ++ " ms"

  putStrLn ""
  putStrLn $ "Avg per op (add): " ++ show (addTime * 1000.0 / cast iterations) ++ " μs"
