{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Proven: Formally verified safety primitives for Haskell.
--
-- This library is a thin FFI wrapper around @libproven@, which is
-- compiled from Idris 2 source code with dependent types and totality
-- checking. __No safety logic is reimplemented in Haskell__; all
-- computation is delegated to the verified core via the C ABI.
--
-- == Architecture
--
-- @
-- Haskell (this package)
--   |
--   | foreign import ccall
--   v
-- libproven.so (Zig FFI layer)
--   |
--   | Zig -> Idris2 RefC calls
--   v
-- Idris 2 (dependent types, totality checking)
-- @
--
-- == Initialization
--
-- Before calling any function, initialize the runtime:
--
-- @
-- import Proven
-- import Proven.FFI (c_proven_init)
--
-- main :: IO ()
-- main = do
--   _ <- c_proven_init
--   result <- safeDiv 10 3
--   print result
-- @
--
-- == Safety Guarantees
--
-- * All functions return @IO (Maybe a)@ or @IO (Either ProvenError a)@
-- * No 'unsafePerformIO', no 'unsafeCoerce', no 'undefined', no 'error'
-- * Memory allocated by libproven is freed via @proven_free_string@
module Proven
  ( -- * Core Types
    module Proven.Core
    -- * FFI Bindings
  , module Proven.FFI
    -- * Safe Wrappers
  , module Proven.SafeMath
  , module Proven.SafeString
  , module Proven.SafePath
  , module Proven.SafeEmail
  , module Proven.SafeNetwork
  , module Proven.SafeCrypto
  , module Proven.SafeUUID
  , module Proven.SafeCurrency
  , module Proven.SafePhone
  , module Proven.SafeHex
  , module Proven.SafeJson
  , module Proven.SafeDateTime
  , module Proven.SafeFloat
  , module Proven.SafeVersion
  , module Proven.SafeColor
  , module Proven.SafeAngle
  , module Proven.SafeUnit
  , module Proven.SafeHeader
  , module Proven.SafeCookie
  , module Proven.SafeContentType
  , module Proven.SafeGeo
  , module Proven.SafeChecksum
  , module Proven.SafeProbability
  , module Proven.SafeCalculator
  , module Proven.SafePassword
  , module Proven.SafeML
  , module Proven.SafeRetry
  ) where

import Proven.Core
import Proven.FFI
import Proven.SafeMath
import Proven.SafeString
import Proven.SafePath
import Proven.SafeEmail
import Proven.SafeNetwork
import Proven.SafeCrypto
import Proven.SafeUUID
import Proven.SafeCurrency
import Proven.SafePhone
import Proven.SafeHex
import Proven.SafeJson
import Proven.SafeDateTime
import Proven.SafeFloat
import Proven.SafeVersion
import Proven.SafeColor
import Proven.SafeAngle
import Proven.SafeUnit
import Proven.SafeHeader
import Proven.SafeCookie
import Proven.SafeContentType
import Proven.SafeGeo
import Proven.SafeChecksum
import Proven.SafeProbability
import Proven.SafeCalculator
import Proven.SafePassword
import Proven.SafeML
import Proven.SafeRetry
