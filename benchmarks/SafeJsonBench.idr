-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeJsonBench

import Proven.Core
import Proven.SafeJson
import System.Clock
import Data.List

%default total

iterations : Nat
iterations = 5000

simpleJson : String
simpleJson = """{"name":"test","value":42}"""

nestedJson : String
nestedJson = """{"user":{"name":"John","age":30},"items":[1,2,3]}"""

largeArrayJson : String
largeArrayJson = "[" ++ foldr (\i, acc => show i ++ if i == 100 then "" else "," ++ acc) "" [1..100] ++ "]"

||| Benchmark simple JSON parsing
benchParseSimple : IO Double
benchParseSimple = do
  start <- clockTime Monotonic
  let _ = map (\_ => parseJson simpleJson) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark nested JSON parsing
benchParseNested : IO Double
benchParseNested = do
  start <- clockTime Monotonic
  let _ = map (\_ => parseJson nestedJson) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark large array JSON parsing
benchParseLargeArray : IO Double
benchParseLargeArray = do
  start <- clockTime Monotonic
  let _ = map (\_ => parseJson largeArrayJson) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark JSON stringification
benchStringify : IO Double
benchStringify = do
  start <- clockTime Monotonic
  let obj = JsonObject [("name", JsonString "test"), ("value", JsonNumber 42)]
  let _ = map (\_ => stringify obj) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark JSON value access
benchValueAccess : IO Double
benchValueAccess = do
  start <- clockTime Monotonic
  case parseJson nestedJson of
    Ok json => do
      let _ = map (\_ => getValue ["user", "name"] json) [1..iterations]
      end <- clockTime Monotonic
      pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0
    Err _ => pure 0.0

||| Run all SafeJson benchmarks
export
runJsonBenchmarks : IO ()
runJsonBenchmarks = do
  putStrLn "SafeJson Benchmarks"
  putStrLn "==================="
  putStrLn $ "Iterations: " ++ show iterations
  putStrLn ""

  simpleTime <- benchParseSimple
  putStrLn $ "parse (simple):    " ++ show simpleTime ++ " ms"

  nestedTime <- benchParseNested
  putStrLn $ "parse (nested):    " ++ show nestedTime ++ " ms"

  largeTime <- benchParseLargeArray
  putStrLn $ "parse (100 items): " ++ show largeTime ++ " ms"

  stringifyTime <- benchStringify
  putStrLn $ "stringify:         " ++ show stringifyTime ++ " ms"

  accessTime <- benchValueAccess
  putStrLn $ "value access:      " ++ show accessTime ++ " ms"

  putStrLn ""
  putStrLn $ "Parse throughput: " ++ show (cast iterations * 1000.0 / simpleTime) ++ " ops/sec"
