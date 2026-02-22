-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafeCurrency - FFI bindings to libproven currency operations
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafeCurrency
  ( currencyParse
  , currencyFormat
  , CurrencyValue
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))

-- | Parsed currency value returned by the FFI.
type CurrencyValue =
  { amountMinor :: Int
  , currencyCode :: String
  , decimalPlaces :: Int
  }

-- | Parse a currency string (e.g. "USD 123.45") (delegates to Idris 2).
foreign import currencyParseImpl :: String ->
  { status :: Int
  , amountMinor :: Int
  , currencyCode :: String
  , decimalPlaces :: Int
  }

currencyParse :: String -> Result CurrencyValue ProvenError
currencyParse s =
  let r = currencyParseImpl s
  in if r.status == 0
     then Ok
       { amountMinor: r.amountMinor
       , currencyCode: r.currencyCode
       , decimalPlaces: r.decimalPlaces
       }
     else Err InvalidCurrency

-- | Format a currency value as a string (delegates to Idris 2).
foreign import currencyFormatImpl :: Int -> String -> Int -> String

currencyFormat :: Int -> String -> Int -> String
currencyFormat = currencyFormatImpl
