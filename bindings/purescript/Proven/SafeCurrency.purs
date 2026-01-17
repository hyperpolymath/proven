-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe currency and monetary operations.
-- |
-- | Provides safe handling of monetary values with proper decimal
-- | arithmetic to avoid floating-point precision issues.

module Proven.SafeCurrency
  ( SafeCurrency
  , Money(..)
  , Currency(..)
  , mkMoney
  , add
  , subtract
  , multiply
  , divide
  , isValidCurrencyCode
  , format
  , toMinorUnits
  , fromMinorUnits
  ) where

import Prelude

import Data.Int (floor, toNumber)
import Data.String (length, toUpper)
import Proven.Result (Result(..), ProvenError(..))

-- | SafeCurrency namespace marker (not instantiated).
data SafeCurrency

-- | ISO 4217 currency code.
newtype Currency = Currency String

derive instance eqCurrency :: Eq Currency

instance showCurrency :: Show Currency where
  show (Currency c) = c

-- | Money value with currency and amount in minor units (cents).
-- | Stored as integer minor units to avoid floating-point issues.
newtype Money = Money
  { currency :: Currency
  , minorUnits :: Int  -- Amount in minor units (e.g., cents)
  , decimalPlaces :: Int  -- Number of decimal places for this currency
  }

derive instance eqMoney :: Eq Money

instance showMoney :: Show Money where
  show (Money m) =
    let
      (Currency code) = m.currency
      major = m.minorUnits / (pow 10 m.decimalPlaces)
      minor = mod (abs m.minorUnits) (pow 10 m.decimalPlaces)
    in code <> " " <> show major <> "." <> padLeft m.decimalPlaces '0' (show minor)
    where
      pow _ 0 = 1
      pow base exp = base * pow base (exp - 1)
      abs n = if n < 0 then -n else n

foreign import padLeft :: Int -> Char -> String -> String

-- | Create a money value from major units (e.g., dollars).
mkMoney :: Currency -> Number -> Int -> Money
mkMoney currency amount decimalPlaces =
  let
    multiplier = pow 10 decimalPlaces
    minorUnits = floor (amount * toNumber multiplier)
  in Money { currency, minorUnits, decimalPlaces }
  where
    pow _ 0 = 1
    pow base exp = base * pow base (exp - 1)

-- | Add two money values (must have same currency).
add :: Money -> Money -> Result Money ProvenError
add (Money a) (Money b)
  | a.currency /= b.currency = Err InvalidCurrency
  | otherwise = Ok (Money
      { currency: a.currency
      , minorUnits: a.minorUnits + b.minorUnits
      , decimalPlaces: a.decimalPlaces
      })

-- | Subtract money values (must have same currency).
subtract :: Money -> Money -> Result Money ProvenError
subtract (Money a) (Money b)
  | a.currency /= b.currency = Err InvalidCurrency
  | otherwise = Ok (Money
      { currency: a.currency
      , minorUnits: a.minorUnits - b.minorUnits
      , decimalPlaces: a.decimalPlaces
      })

-- | Multiply money by a factor.
multiply :: Money -> Number -> Money
multiply (Money m) factor =
  let newMinor = floor (toNumber m.minorUnits * factor)
  in Money m { minorUnits = newMinor }

-- | Divide money by a divisor.
divide :: Money -> Number -> Result Money ProvenError
divide _ 0.0 = Err DivisionByZero
divide (Money m) divisor =
  let newMinor = floor (toNumber m.minorUnits / divisor)
  in Ok (Money m { minorUnits = newMinor })

-- | Check if a string is a valid ISO 4217 currency code.
-- | Valid codes are exactly 3 uppercase ASCII letters.
isValidCurrencyCode :: String -> Boolean
isValidCurrencyCode code =
  length code == 3 && isAllUpperAlpha code

foreign import isAllUpperAlpha :: String -> Boolean

-- | Format money as a string with proper decimal places.
format :: Money -> String
format = show

-- | Get the amount in minor units (e.g., cents).
toMinorUnits :: Money -> Int
toMinorUnits (Money m) = m.minorUnits

-- | Create money from minor units (e.g., cents).
fromMinorUnits :: Currency -> Int -> Int -> Money
fromMinorUnits currency minorUnits decimalPlaces =
  Money { currency, minorUnits, decimalPlaces }
