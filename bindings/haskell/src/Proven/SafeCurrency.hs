{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe currency operations via libproven FFI.
--
-- Currency parsing and formatting are performed by the Idris 2 verified core.
module Proven.SafeCurrency
  ( CurrencyAmount(..)
  , parseCurrency
  , formatCurrency
  ) where

import Data.Word (Word8)
import Data.Char (ord)
import Proven.FFI (c_proven_currency_parse, c_proven_currency_format,
                   c_proven_free_string)
import Proven.FFI.Types (CurrencyResult(..))
import Proven.Core (withCStringLen', stringResultToMaybe)

-- | A parsed currency amount.
data CurrencyAmount = CurrencyAmount
  { caAmountMinor   :: !Int      -- ^ Amount in minor units (cents)
  , caCurrencyCode  :: !String   -- ^ ISO 4217 currency code (3 chars)
  , caDecimalPlaces :: !Int      -- ^ Number of decimal places
  } deriving (Eq, Show)

-- | Parse a currency string (e.g., "USD 123.45").
-- Delegates to @proven_currency_parse@ in libproven.
parseCurrency :: String -> IO (Maybe CurrencyAmount)
parseCurrency str = withCStringLen' str $ \ptr len -> do
  result <- c_proven_currency_parse ptr len
  if currStatusRaw result == 0
    then let (c0, c1, c2) = currCode result
             code = [toChar c0, toChar c1, toChar c2]
         in return (Just (CurrencyAmount
              (fromIntegral (currAmountMinor result))
              code
              (fromIntegral (currDecimalPlaces result))))
    else return Nothing
  where
    toChar :: Word8 -> Char
    toChar = toEnum . fromIntegral

-- | Format a currency amount.
-- Delegates to @proven_currency_format@ in libproven.
formatCurrency :: Int -> String -> Int -> IO (Maybe String)
formatCurrency amountMinor code decimalPlaces
  | length code /= 3 = return Nothing
  | otherwise = do
      let [c0, c1, c2] = map (fromIntegral . ord) code :: [Word8]
      sr <- c_proven_currency_format
              (fromIntegral amountMinor)
              c0 c1 c2
              (fromIntegral decimalPlaces)
      stringResultToMaybe c_proven_free_string sr
