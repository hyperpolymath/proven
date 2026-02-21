{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Safe currency operations with type-safe monetary values.
--
-- Provides:
--
-- * ISO 4217 currency codes as an ADT
-- * Money type using integer minor units (cents) to avoid floating-point errors
-- * Safe arithmetic operations
-- * Currency formatting with symbols and codes
module Proven.SafeCurrency
  ( -- * Types
    CurrencyCode(..)
  , Money(..)
  , CurrencyError(..)
    -- * Currency Properties
  , currencyDecimals
  , currencySymbol
  , currencyName
    -- * Currency Parsing
  , parseCurrencyCode
  , isValidCurrencyCode
    -- * Money Construction
  , fromMajorUnits
  , fromMinorUnits
  , zeroMoney
    -- * Money Properties
  , getMinorUnits
  , getMajorUnits
  , getCurrency
  , isZero
  , isPositive
  , isNegative
    -- * Money Arithmetic
  , addMoney
  , subtractMoney
  , multiplyMoney
  , divideMoney
  , negateMoney
  , absMoney
    -- * Money Formatting
  , formatMoney
  , formatMoneyWithCode
  ) where

import Data.Char (toUpper)
import Data.Int (Int64)

-- | ISO 4217 currency codes (common currencies).
data CurrencyCode
  = USD  -- ^ US Dollar
  | EUR  -- ^ Euro
  | GBP  -- ^ British Pound
  | JPY  -- ^ Japanese Yen
  | CHF  -- ^ Swiss Franc
  | CAD  -- ^ Canadian Dollar
  | AUD  -- ^ Australian Dollar
  | NZD  -- ^ New Zealand Dollar
  | CNY  -- ^ Chinese Yuan
  | INR  -- ^ Indian Rupee
  | BRL  -- ^ Brazilian Real
  | MXN  -- ^ Mexican Peso
  | KRW  -- ^ South Korean Won
  | SGD  -- ^ Singapore Dollar
  | HKD  -- ^ Hong Kong Dollar
  | SEK  -- ^ Swedish Krona
  | NOK  -- ^ Norwegian Krone
  | DKK  -- ^ Danish Krone
  | PLN  -- ^ Polish Zloty
  | RUB  -- ^ Russian Ruble
  | ZAR  -- ^ South African Rand
  | TRY  -- ^ Turkish Lira
  | THB  -- ^ Thai Baht
  | MYR  -- ^ Malaysian Ringgit
  | IDR  -- ^ Indonesian Rupiah
  | PHP  -- ^ Philippine Peso
  | VND  -- ^ Vietnamese Dong
  | AED  -- ^ UAE Dirham
  | SAR  -- ^ Saudi Riyal
  | ILS  -- ^ Israeli Shekel
  | CZK  -- ^ Czech Koruna
  | HUF  -- ^ Hungarian Forint
  | RON  -- ^ Romanian Leu
  | BGN  -- ^ Bulgarian Lev
  | HRK  -- ^ Croatian Kuna
  | ISK  -- ^ Icelandic Krona
  | CLP  -- ^ Chilean Peso
  | COP  -- ^ Colombian Peso
  | PEN  -- ^ Peruvian Sol
  | ARS  -- ^ Argentine Peso
  | BTC  -- ^ Bitcoin
  | ETH  -- ^ Ethereum
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Currency operation errors.
data CurrencyError
  = InvalidAmount String
  | CurrencyMismatch CurrencyCode CurrencyCode
  | DivisionByZero
  | Overflow
  | InvalidCurrencyCodeError String
  deriving (Show, Eq)

-- | Monetary value in minor units (cents, satoshis, etc.) with currency.
data Money = Money
  { moneyMinorUnits :: Int64
  , moneyCurrency   :: CurrencyCode
  } deriving (Eq)

instance Show Money where
  show = formatMoneyWithCode

instance Ord Money where
  compare (Money a _) (Money b _) = compare a b

--------------------------------------------------------------------------------
-- Currency Properties
--------------------------------------------------------------------------------

-- | Get the number of decimal places for a currency.
currencyDecimals :: CurrencyCode -> Int
currencyDecimals JPY = 0   -- Yen has no decimals
currencyDecimals KRW = 0   -- Won has no decimals
currencyDecimals VND = 0   -- Dong has no decimals
currencyDecimals BTC = 8   -- Bitcoin uses satoshis
currencyDecimals ETH = 8   -- Ethereum (capped at 8 for practicality)
currencyDecimals _   = 2   -- Most currencies use 2 decimal places

-- | Get the currency symbol.
currencySymbol :: CurrencyCode -> String
currencySymbol USD = "$"
currencySymbol EUR = "\x20AC"  -- Euro sign
currencySymbol GBP = "\x00A3"  -- Pound sign
currencySymbol JPY = "\x00A5"  -- Yen sign
currencySymbol CHF = "Fr"
currencySymbol CAD = "C$"
currencySymbol AUD = "A$"
currencySymbol NZD = "NZ$"
currencySymbol CNY = "\x00A5"  -- Yen sign (shared with JPY)
currencySymbol INR = "\x20B9"  -- Rupee sign
currencySymbol BRL = "R$"
currencySymbol MXN = "Mex$"
currencySymbol KRW = "\x20A9"  -- Won sign
currencySymbol SGD = "S$"
currencySymbol HKD = "HK$"
currencySymbol SEK = "kr"
currencySymbol NOK = "kr"
currencySymbol DKK = "kr"
currencySymbol PLN = "z\x0142"  -- zloty
currencySymbol RUB = "\x20BD"  -- Ruble sign
currencySymbol ZAR = "R"
currencySymbol TRY = "\x20BA"  -- Lira sign
currencySymbol THB = "\x0E3F"  -- Baht sign
currencySymbol MYR = "RM"
currencySymbol IDR = "Rp"
currencySymbol PHP = "\x20B1"  -- Peso sign
currencySymbol VND = "\x20AB"  -- Dong sign
currencySymbol AED = "AED"
currencySymbol SAR = "SAR"
currencySymbol ILS = "\x20AA"  -- Shekel sign
currencySymbol CZK = "K\x010D"  -- Koruna
currencySymbol HUF = "Ft"
currencySymbol RON = "lei"
currencySymbol BGN = "лв"
currencySymbol HRK = "kn"
currencySymbol ISK = "kr"
currencySymbol CLP = "$"
currencySymbol COP = "$"
currencySymbol PEN = "S/"
currencySymbol ARS = "$"
currencySymbol BTC = "\x20BF"  -- Bitcoin sign
currencySymbol ETH = "\x039E"  -- Xi (commonly used for Ethereum)

-- | Get the currency name.
currencyName :: CurrencyCode -> String
currencyName USD = "US Dollar"
currencyName EUR = "Euro"
currencyName GBP = "British Pound Sterling"
currencyName JPY = "Japanese Yen"
currencyName CHF = "Swiss Franc"
currencyName CAD = "Canadian Dollar"
currencyName AUD = "Australian Dollar"
currencyName NZD = "New Zealand Dollar"
currencyName CNY = "Chinese Yuan"
currencyName INR = "Indian Rupee"
currencyName BRL = "Brazilian Real"
currencyName MXN = "Mexican Peso"
currencyName KRW = "South Korean Won"
currencyName SGD = "Singapore Dollar"
currencyName HKD = "Hong Kong Dollar"
currencyName SEK = "Swedish Krona"
currencyName NOK = "Norwegian Krone"
currencyName DKK = "Danish Krone"
currencyName PLN = "Polish Zloty"
currencyName RUB = "Russian Ruble"
currencyName ZAR = "South African Rand"
currencyName TRY = "Turkish Lira"
currencyName THB = "Thai Baht"
currencyName MYR = "Malaysian Ringgit"
currencyName IDR = "Indonesian Rupiah"
currencyName PHP = "Philippine Peso"
currencyName VND = "Vietnamese Dong"
currencyName AED = "UAE Dirham"
currencyName SAR = "Saudi Riyal"
currencyName ILS = "Israeli New Shekel"
currencyName CZK = "Czech Koruna"
currencyName HUF = "Hungarian Forint"
currencyName RON = "Romanian Leu"
currencyName BGN = "Bulgarian Lev"
currencyName HRK = "Croatian Kuna"
currencyName ISK = "Icelandic Krona"
currencyName CLP = "Chilean Peso"
currencyName COP = "Colombian Peso"
currencyName PEN = "Peruvian Sol"
currencyName ARS = "Argentine Peso"
currencyName BTC = "Bitcoin"
currencyName ETH = "Ethereum"

--------------------------------------------------------------------------------
-- Currency Parsing
--------------------------------------------------------------------------------

-- | Parse currency code from string (case-insensitive).
parseCurrencyCode :: String -> Maybe CurrencyCode
parseCurrencyCode s = case map toUpper s of
  "USD" -> Just USD
  "EUR" -> Just EUR
  "GBP" -> Just GBP
  "JPY" -> Just JPY
  "CHF" -> Just CHF
  "CAD" -> Just CAD
  "AUD" -> Just AUD
  "NZD" -> Just NZD
  "CNY" -> Just CNY
  "INR" -> Just INR
  "BRL" -> Just BRL
  "MXN" -> Just MXN
  "KRW" -> Just KRW
  "SGD" -> Just SGD
  "HKD" -> Just HKD
  "SEK" -> Just SEK
  "NOK" -> Just NOK
  "DKK" -> Just DKK
  "PLN" -> Just PLN
  "RUB" -> Just RUB
  "ZAR" -> Just ZAR
  "TRY" -> Just TRY
  "THB" -> Just THB
  "MYR" -> Just MYR
  "IDR" -> Just IDR
  "PHP" -> Just PHP
  "VND" -> Just VND
  "AED" -> Just AED
  "SAR" -> Just SAR
  "ILS" -> Just ILS
  "CZK" -> Just CZK
  "HUF" -> Just HUF
  "RON" -> Just RON
  "BGN" -> Just BGN
  "HRK" -> Just HRK
  "ISK" -> Just ISK
  "CLP" -> Just CLP
  "COP" -> Just COP
  "PEN" -> Just PEN
  "ARS" -> Just ARS
  "BTC" -> Just BTC
  "ETH" -> Just ETH
  _     -> Nothing

-- | Check if string is valid currency code.
isValidCurrencyCode :: String -> Bool
isValidCurrencyCode s = case parseCurrencyCode s of
  Just _  -> True
  Nothing -> False

--------------------------------------------------------------------------------
-- Money Construction
--------------------------------------------------------------------------------

-- | Create money from major units (dollars, euros, etc.).
fromMajorUnits :: CurrencyCode -> Int64 -> Money
fromMajorUnits currency major =
  let decimals = currencyDecimals currency
      multiplier = 10 ^ decimals
  in Money (major * multiplier) currency

-- | Create money from minor units (cents, pence, etc.).
fromMinorUnits :: CurrencyCode -> Int64 -> Money
fromMinorUnits currency minor = Money minor currency

-- | Zero amount for a currency.
zeroMoney :: CurrencyCode -> Money
zeroMoney currency = Money 0 currency

--------------------------------------------------------------------------------
-- Money Properties
--------------------------------------------------------------------------------

-- | Get the minor units (cents) from a Money value.
getMinorUnits :: Money -> Int64
getMinorUnits = moneyMinorUnits

-- | Get the major units (dollars) as an integer (truncated).
getMajorUnits :: Money -> Int64
getMajorUnits (Money units currency) =
  let decimals = currencyDecimals currency
      divisor = 10 ^ decimals
  in units `div` divisor

-- | Get the currency code.
getCurrency :: Money -> CurrencyCode
getCurrency = moneyCurrency

-- | Check if money is zero.
isZero :: Money -> Bool
isZero (Money units _) = units == 0

-- | Check if money is positive.
isPositive :: Money -> Bool
isPositive (Money units _) = units > 0

-- | Check if money is negative.
isNegative :: Money -> Bool
isNegative (Money units _) = units < 0

--------------------------------------------------------------------------------
-- Money Arithmetic
--------------------------------------------------------------------------------

-- | Add two monetary values (must be same currency).
addMoney :: Money -> Money -> Either CurrencyError Money
addMoney (Money a ca) (Money b cb)
  | ca /= cb  = Left (CurrencyMismatch ca cb)
  | otherwise = Right (Money (a + b) ca)

-- | Subtract two monetary values (must be same currency).
subtractMoney :: Money -> Money -> Either CurrencyError Money
subtractMoney (Money a ca) (Money b cb)
  | ca /= cb  = Left (CurrencyMismatch ca cb)
  | otherwise = Right (Money (a - b) ca)

-- | Multiply money by a scalar.
multiplyMoney :: Money -> Int64 -> Money
multiplyMoney (Money units currency) n = Money (units * n) currency

-- | Divide money by a scalar (truncates).
divideMoney :: Money -> Int64 -> Either CurrencyError Money
divideMoney _ 0 = Left DivisionByZero
divideMoney (Money units currency) n = Right (Money (units `div` n) currency)

-- | Negate a monetary value.
negateMoney :: Money -> Money
negateMoney (Money units currency) = Money (negate units) currency

-- | Absolute value.
absMoney :: Money -> Money
absMoney (Money units currency) = Money (abs units) currency

--------------------------------------------------------------------------------
-- Money Formatting
--------------------------------------------------------------------------------

-- | Pad string with character on the left.
padLeft :: Int -> Char -> String -> String
padLeft targetLen c s
  | length s >= targetLen = s
  | otherwise = replicate (targetLen - length s) c ++ s

-- | Format money with symbol (e.g., "$123.45").
formatMoney :: Money -> String
formatMoney (Money units currency) =
  let decimals = currencyDecimals currency
      symbol = currencySymbol currency
      isNeg = units < 0
      absUnits = abs units
      divisor = 10 ^ decimals
      major = absUnits `div` divisor
      minor = absUnits `mod` divisor
      negSign = if isNeg then "-" else ""
  in if decimals == 0
       then negSign ++ symbol ++ show major
       else negSign ++ symbol ++ show major ++ "." ++ padLeft decimals '0' (show minor)

-- | Format money with code (e.g., "123.45 USD").
formatMoneyWithCode :: Money -> String
formatMoneyWithCode (Money units currency) =
  let decimals = currencyDecimals currency
      isNeg = units < 0
      absUnits = abs units
      divisor = 10 ^ decimals
      major = absUnits `div` divisor
      minor = absUnits `mod` divisor
      negSign = if isNeg then "-" else ""
  in if decimals == 0
       then negSign ++ show major ++ " " ++ show currency
       else negSign ++ show major ++ "." ++ padLeft decimals '0' (show minor) ++ " " ++ show currency
