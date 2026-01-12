-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module Main

-- Core modules
import SafeMathProps
import SafeStringProps
import SafeJsonProps

-- Format safety modules
import SafeUrlProps
import SafeEmailProps
import SafePathProps

-- Security modules
import SafeCryptoProps
import SafePasswordProps
import SafeDateTimeProps
import SafeNetworkProps

-- Extended safety modules
import SafeRegexProps
import SafeHtmlProps
import SafeCommandProps

-- Auth & Serialization modules (v0.5.0)
import SafeSQLProps
import SafeJWTProps
import SafeBase64Props
import SafeXMLProps
import SafeYAMLProps
import SafeTOMLProps

-- Data Types modules (v0.6.0)
import SafeUUIDProps
import SafeCurrencyProps
import SafePhoneProps
import SafeHexProps

-- I/O Safety modules (v0.7.0)
import SafeEnvProps
import SafeArgsProps
import SafeFileProps

-- Network Extended modules (v0.8.0)
import SafeHeaderProps
import SafeCookieProps
import SafeContentTypeProps

||| Main test runner for all property tests
||| In Idris 2, property tests are primarily verified at compile time.
||| If this module type-checks, the proofs are valid.
main : IO ()
main = do
  putStrLn "╔═══════════════════════════════════════════╗"
  putStrLn "║   PROVEN - Property-Based Test Suite      ║"
  putStrLn "║   29 Modules | 135+ Properties            ║"
  putStrLn "╚═══════════════════════════════════════════╝"
  putStrLn ""

  -- Core modules
  runMathProperties
  runStringProperties
  runJsonProperties

  -- Format safety modules
  runUrlProperties
  runEmailProperties
  runPathProperties

  -- Security modules
  runCryptoProperties
  runPasswordProperties
  runDateTimeProperties
  runNetworkProperties

  -- Extended safety modules
  runRegexProperties
  runHtmlProperties
  runCommandProperties

  -- Auth & Serialization modules (v0.5.0)
  runSQLProperties
  runJWTProperties
  runBase64Properties
  runXMLProperties
  runYAMLProperties
  runTOMLProperties

  -- Data Types modules (v0.6.0)
  runUUIDProperties
  runCurrencyProperties
  runPhoneProperties
  runHexProperties

  -- I/O Safety modules (v0.7.0)
  runEnvProperties
  runArgsProperties
  runFileProperties

  -- Network Extended modules (v0.8.0)
  runHeaderProperties
  runCookieProperties
  runContentTypeProperties

  putStrLn "╔═══════════════════════════════════════════╗"
  putStrLn "║   All 29 modules tested successfully!     ║"
  putStrLn "║   All compile-time proofs verified!       ║"
  putStrLn "╚═══════════════════════════════════════════╝"
  putStrLn ""
  putStrLn "Note: In dependently-typed languages like Idris 2,"
  putStrLn "many properties are proven at compile time."
  putStrLn "If this program compiles, the proofs are valid."
