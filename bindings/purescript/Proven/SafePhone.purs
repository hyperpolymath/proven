-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafePhone - FFI bindings to libproven phone number validation
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafePhone
  ( parsePhone
  , formatE164
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))

-- | Parse a phone number string (delegates to Idris 2).
-- | Returns country code, national number, and validity flag.
foreign import parsePhoneImpl :: String ->
  { status :: Int
  , countryCode :: Int
  , nationalNumber :: Int
  , isValid :: Boolean
  }

parsePhone :: String -> Result { countryCode :: Int, nationalNumber :: Int } ProvenError
parsePhone s =
  let r = parsePhoneImpl s
  in if r.status == 0 && r.isValid
     then Ok { countryCode: r.countryCode, nationalNumber: r.nationalNumber }
     else Err InvalidPhone

-- | Format a phone number in E.164 format (delegates to Idris 2).
foreign import formatE164Impl :: Int -> Int -> String

formatE164 :: Int -> Int -> String
formatE164 = formatE164Impl
