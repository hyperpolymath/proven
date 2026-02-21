-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeNetworkBench

import Proven.Core
import Proven.SafeNetwork
import System.Clock
import Data.List

%default total

iterations : Nat
iterations = 10000

||| Benchmark IPv4 parsing
benchParseIPv4 : IO Double
benchParseIPv4 = do
  start <- clockTime Monotonic
  let _ = map (\_ => parseIPv4 "192.168.1.1") [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark IPv6 parsing
benchParseIPv6 : IO Double
benchParseIPv6 = do
  start <- clockTime Monotonic
  let _ = map (\_ => parseIPv6 "2001:0db8:85a3:0000:0000:8a2e:0370:7334") [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark CIDR parsing
benchParseCIDR : IO Double
benchParseCIDR = do
  start <- clockTime Monotonic
  let _ = map (\_ => parseCIDR "10.0.0.0/8") [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark port validation
benchValidatePort : IO Double
benchValidatePort = do
  start <- clockTime Monotonic
  let _ = map (\i => validatePort (cast (i `mod` 65536))) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Benchmark private IP detection
benchPrivateIPCheck : IO Double
benchPrivateIPCheck = do
  start <- clockTime Monotonic
  let ip = MkIPv4 192 168 1 1
  let _ = map (\_ => isPrivateIP ip) [1..iterations]
  end <- clockTime Monotonic
  pure $ cast (nanoseconds end - nanoseconds start) / 1000000.0

||| Run all SafeNetwork benchmarks
export
runNetworkBenchmarks : IO ()
runNetworkBenchmarks = do
  putStrLn "SafeNetwork Benchmarks"
  putStrLn "======================"
  putStrLn $ "Iterations: " ++ show iterations
  putStrLn ""

  ipv4Time <- benchParseIPv4
  putStrLn $ "parseIPv4:     " ++ show ipv4Time ++ " ms"

  ipv6Time <- benchParseIPv6
  putStrLn $ "parseIPv6:     " ++ show ipv6Time ++ " ms"

  cidrTime <- benchParseCIDR
  putStrLn $ "parseCIDR:     " ++ show cidrTime ++ " ms"

  portTime <- benchValidatePort
  putStrLn $ "validatePort:  " ++ show portTime ++ " ms"

  privateTime <- benchPrivateIPCheck
  putStrLn $ "isPrivateIP:   " ++ show privateTime ++ " ms"

  putStrLn ""
