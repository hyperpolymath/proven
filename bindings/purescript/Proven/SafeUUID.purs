-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafeUUID - FFI bindings to libproven UUID operations
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafeUUID
  ( uuidV4
  , uuidParse
  , uuidIsNil
  , uuidVersion
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))

-- | Generate a UUID v4 (random) (delegates to Idris 2).
foreign import uuidV4Impl :: Unit -> { status :: Int, value :: String }

uuidV4 :: Result String ProvenError
uuidV4 =
  let r = uuidV4Impl unit
  in if r.status == 0 then Ok r.value else Err InvalidUuid

-- | Parse a UUID string (delegates to Idris 2).
foreign import uuidParseImpl :: String -> { status :: Int, value :: String }

uuidParse :: String -> Result String ProvenError
uuidParse s =
  let r = uuidParseImpl s
  in if r.status == 0 then Ok r.value else Err InvalidUuid

-- | Check if a UUID string is the nil UUID (delegates to Idris 2).
foreign import uuidIsNilImpl :: String -> Boolean

uuidIsNil :: String -> Boolean
uuidIsNil = uuidIsNilImpl

-- | Get the version of a UUID string (delegates to Idris 2).
foreign import uuidVersionImpl :: String -> Int

uuidVersion :: String -> Int
uuidVersion = uuidVersionImpl
