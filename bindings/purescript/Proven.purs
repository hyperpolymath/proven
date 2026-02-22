-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | Proven Safety Library for PureScript
-- |
-- | FFI bindings to libproven (Idris 2 verified safety library).
-- | All computation is performed in Idris 2 via the Zig FFI layer.
-- | This module re-exports all submodule bindings.

module Proven
  ( module Proven.Result
  , module Proven.SafeMath
  , module Proven.SafeString
  , module Proven.SafeCrypto
  , module Proven.SafeEmail
  , module Proven.SafeFloat
  , module Proven.SafeHex
  , module Proven.SafeJson
  , module Proven.SafeNetwork
  , module Proven.SafePath
  , module Proven.SafePhone
  , module Proven.SafeUrl
  , module Proven.SafeUUID
  , module Proven.SafeCurrency
  , module Proven.SafeDateTime
  , module Proven.SafeAngle
  , module Proven.SafeColor
  , module Proven.SafeUnit
  , module Proven.SafeVersion
  , module Proven.Bounded
  , module Proven.Percentage
  , module Proven.Validation
  , version
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..), ok, err, isOk, isErr, unwrapOr, mapResult, flatMapResult)
import Proven.SafeMath (safeAdd, safeSub, safeMul, safeDiv, safeMod, clamp)
import Proven.SafeString (escapeHtml, escapeSql, escapeJs, isValidUtf8)
import Proven.SafeCrypto (constantTimeCompare, randomBytes)
import Proven.SafeEmail (isValidEmail)
import Proven.SafeFloat (safeDiv, safeSqrt, safeLn, isFinite, isNaN) as SafeFloat
import Proven.SafeHex (hexEncode, hexEncodeUpper, hexDecode)
import Proven.SafeJson (isValidJson, jsonTypeOf)
import Proven.SafeNetwork (parseIpv4, ipv4IsPrivate, ipv4IsLoopback)
import Proven.SafePath (hasTraversal, sanitizeFilename)
import Proven.SafePhone (parsePhone, formatE164)
import Proven.SafeUrl (parseUrl)
import Proven.SafeUUID (uuidV4, uuidParse, uuidIsNil, uuidVersion)
import Proven.SafeCurrency (currencyParse, currencyFormat)
import Proven.SafeDateTime (isLeapYear, daysInMonth)
import Proven.SafeAngle (degToRad, radToDeg, normalizeDegrees, normalizeRadians)
import Proven.SafeColor (parseHexColor, rgbToHsl, rgbToHex)
import Proven.SafeUnit (convertLength, convertTemp)
import Proven.SafeVersion (versionParse, versionCompare)
import Proven.Bounded (BoundedInt(..), mkBoundedInt, getValue, getBounds, isAtMin, isAtMax)
import Proven.Percentage (Percentage(..), mkPercentage, toNormalized)
import Proven.Validation (isValidEmail, isValidPath, requireValidEmail, requireSafePath) as Validation

-- | Library version
version :: String
version = "1.0.0"
