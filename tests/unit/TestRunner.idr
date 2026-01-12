-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module TestRunner

-- Core modules
import SafeMathUnit
import SafeStringUnit
import SafeJsonUnit

-- Format safety modules
import SafeUrlUnit
import SafeEmailUnit
import SafePathUnit

-- Security modules
import SafeCryptoUnit
import SafePasswordUnit
import SafeDateTimeUnit
import SafeNetworkUnit
import SafeRegexUnit
import SafeHtmlUnit
import SafeCommandUnit

-- Auth & Serialization modules (v0.5.0)
import SafeSQLUnit
import SafeJWTUnit
import SafeBase64Unit

-- Data Types modules (v0.6.0)
import SafeUUIDUnit
import SafeCurrencyUnit

-- Network Extended modules (v0.8.0)
import SafeHeaderUnit
import SafeCookieUnit

||| Main test runner for all unit tests
||| Unit tests verify runtime behavior with actual function calls.
main : IO ()
main = do
  putStrLn "╔═══════════════════════════════════════════════════╗"
  putStrLn "║   PROVEN - Runtime Unit Test Suite                ║"
  putStrLn "║   20 Modules | 400+ Test Cases                    ║"
  putStrLn "╚═══════════════════════════════════════════════════╝"
  putStrLn ""

  -- Core modules
  runMathUnitTests
  putStrLn ""
  runStringUnitTests
  putStrLn ""
  runJsonUnitTests
  putStrLn ""

  -- Format safety modules
  runUrlUnitTests
  putStrLn ""
  runEmailUnitTests
  putStrLn ""
  runPathUnitTests
  putStrLn ""

  -- Security modules
  runCryptoUnitTests
  putStrLn ""
  runPasswordUnitTests
  putStrLn ""
  runDateTimeUnitTests
  putStrLn ""
  runNetworkUnitTests
  putStrLn ""
  runRegexUnitTests
  putStrLn ""
  runHtmlUnitTests
  putStrLn ""
  runCommandUnitTests
  putStrLn ""

  -- Auth & Serialization modules
  runSQLUnitTests
  putStrLn ""
  runJWTUnitTests
  putStrLn ""
  runBase64UnitTests
  putStrLn ""

  -- Data Types modules
  runUUIDUnitTests
  putStrLn ""
  runCurrencyUnitTests
  putStrLn ""

  -- Network Extended modules
  runHeaderUnitTests
  putStrLn ""
  runCookieUnitTests
  putStrLn ""

  putStrLn "╔═══════════════════════════════════════════════════╗"
  putStrLn "║   All 20 unit test modules executed!              ║"
  putStrLn "║   Check above for individual test results.        ║"
  putStrLn "╚═══════════════════════════════════════════════════╝"
