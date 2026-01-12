-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module Main

import Data.List
import System.Clock

import Proven.Core
import Proven.SafeMath
import Proven.SafeString
import Proven.SafeJson
import Proven.SafeUrl
import Proven.SafeEmail
import Proven.SafePath
import Proven.SafeRegex
import Proven.SafeSQL
import Proven.SafeBase64
import Proven.SafeUUID

||| Time a computation in nanoseconds
timeIt : IO a -> IO (a, Integer)
timeIt action = do
  start <- clockTime Monotonic
  result <- action
  end <- clockTime Monotonic
  let elapsed = seconds end - seconds start
  let nanos = nanoseconds end - nanoseconds start
  pure (result, elapsed * 1000000000 + nanos)

||| Run a benchmark N times and report statistics
benchmark : String -> Nat -> IO () -> IO ()
benchmark name iterations action = do
  putStrLn $ "Benchmarking: " ++ name
  times <- sequence $ replicate iterations (timeIt action)
  let nanos = map snd times
  let total = sum nanos
  let avg = total `div` cast iterations
  let minTime = foldr min 999999999999 nanos
  let maxTime = foldr max 0 nanos
  putStrLn $ "  Iterations: " ++ show iterations
  putStrLn $ "  Total:      " ++ show total ++ " ns"
  putStrLn $ "  Average:    " ++ show avg ++ " ns"
  putStrLn $ "  Min:        " ++ show minTime ++ " ns"
  putStrLn $ "  Max:        " ++ show maxTime ++ " ns"
  putStrLn ""

||| SafeMath benchmarks
benchSafeMath : IO ()
benchSafeMath = do
  putStrLn "SafeMath Benchmarks"
  benchmark "safeAdd" 10000 $ pure $ ignore $ safeAdd 12345 67890
  benchmark "safeDiv" 10000 $ pure $ ignore $ safeDiv 1000000 12345

||| SafeString benchmarks
benchSafeString : IO ()
benchSafeString = do
  putStrLn "SafeString Benchmarks"
  let str = "Hello <script>alert('xss')</script> World"
  benchmark "escapeHtml" 10000 $ pure $ ignore $ escapeHtml str
  benchmark "urlEncode" 10000 $ pure $ ignore $ urlEncode str

||| SafeJson benchmarks
benchSafeJson : IO ()
benchSafeJson = do
  putStrLn "SafeJson Benchmarks"
  let json = "{\"name\": \"Alice\", \"age\": 30}"
  benchmark "parseJson" 5000 $ pure $ ignore $ parseJson json

||| SafeUrl benchmarks
benchSafeUrl : IO ()
benchSafeUrl = do
  putStrLn "SafeUrl Benchmarks"
  let url = "https://api.example.com:8080/v1/users?page=1"
  benchmark "parseUrl" 10000 $ pure $ ignore $ parseUrl url
  benchmark "isValidUrl" 10000 $ pure $ ignore $ isValidUrl url

||| Main
main : IO ()
main = do
  putStrLn "PROVEN Performance Benchmarks"
  putStrLn "=============================="
  benchSafeMath
  benchSafeString
  benchSafeJson
  benchSafeUrl
  putStrLn "Done!"
