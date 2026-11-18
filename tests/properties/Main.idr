-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module Main

import KnownDefects

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

||| Main test runner for all property tests.
||| Compiling this suite checks the type-level proofs it contains. The rest are
||| finite execution checks (they do not prove the general property), named
||| `...Obligation` types (owed, not proved), and strict known-defect checks
||| (see KnownDefects). Each module prints which kind each property is.
main : IO ()
main = do
  putStrLn "╔═══════════════════════════════════════════╗"
  putStrLn "║   PROVEN - Property-Based Test Suite      ║"
  putStrLn "║   29 Modules                              ║"
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

  reportKnownDefects
  putStrLn ""
  putStrLn "All 29 modules ran. Type-level proofs typechecked; execution checks passed"
  putStrLn "on their finite inputs; named Obligation types remain owed; listed known"
  putStrLn "defects still reproduce exactly as recorded."
