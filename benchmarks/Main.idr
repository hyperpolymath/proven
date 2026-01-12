-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module Main

import SafeMathBench
import SafeStringBench
import SafeCryptoBench
import SafeJsonBench
import SafeNetworkBench
import System

%default total

||| Print benchmark header
printHeader : IO ()
printHeader = do
  putStrLn "╔════════════════════════════════════════════╗"
  putStrLn "║      Proven Library Benchmark Suite        ║"
  putStrLn "║        Performance Validation              ║"
  putStrLn "╚════════════════════════════════════════════╝"
  putStrLn ""

||| Print benchmark footer
printFooter : IO ()
printFooter = do
  putStrLn "════════════════════════════════════════════"
  putStrLn "Benchmark complete."
  putStrLn ""
  putStrLn "Note: Times include Idris 2 runtime overhead."
  putStrLn "For production metrics, use language-specific"
  putStrLn "benchmarks (Rust criterion, Python pytest-benchmark)."

||| Main entry point
main : IO ()
main = do
  printHeader

  runMathBenchmarks
  putStrLn ""

  runStringBenchmarks
  putStrLn ""

  runCryptoBenchmarks
  putStrLn ""

  runJsonBenchmarks
  putStrLn ""

  runNetworkBenchmarks

  printFooter
