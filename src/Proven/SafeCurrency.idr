-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeCurrency - Type-safe currency operations
|||
||| This module provides:
||| - Type-safe monetary value representation
||| - ISO 4217 currency codes
||| - Safe arithmetic that preserves currency type
||| - Currency formatting
||| - Exchange rate handling
|||
||| All monetary values use integer cents/minor units to avoid floating-point errors
module Proven.SafeCurrency

import public Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Currency Code Types
--------------------------------------------------------------------------------

||| ISO 4217 currency codes (subset of common currencies)
public export
data CurrencyCode
  = USD  -- US Dollar
  | EUR  -- Euro
  | GBP  -- British Pound
  | JPY  -- Japanese Yen
  | CHF  -- Swiss Franc
  | CAD  -- Canadian Dollar
  | AUD  -- Australian Dollar
  | NZD  -- New Zealand Dollar
  | CNY  -- Chinese Yuan
  | INR  -- Indian Rupee
  | BRL  -- Brazilian Real
  | MXN  -- Mexican Peso
  | KRW  -- South Korean Won
  | SGD  -- Singapore Dollar
  | HKD  -- Hong Kong Dollar
  | SEK  -- Swedish Krona
  | NOK  -- Norwegian Krone
  | DKK  -- Danish Krone
  | PLN  -- Polish Zloty
  | RUB  -- Russian Ruble
  | ZAR  -- South African Rand
  | TRY  -- Turkish Lira
  | THB  -- Thai Baht
  | MYR  -- Malaysian Ringgit
  | IDR  -- Indonesian Rupiah
  | PHP  -- Philippine Peso
  | VND  -- Vietnamese Dong
  | AED  -- UAE Dirham
  | SAR  -- Saudi Riyal
  | ILS  -- Israeli Shekel
  | CZK  -- Czech Koruna
  | HUF  -- Hungarian Forint
  | RON  -- Romanian Leu
  | BGN  -- Bulgarian Lev
  | HRK  -- Croatian Kuna
  | ISK  -- Icelandic Krona
  | CLP  -- Chilean Peso
  | COP  -- Colombian Peso
  | PEN  -- Peruvian Sol
  | ARS  -- Argentine Peso
  | BTC  -- Bitcoin (crypto)
  | ETH  -- Ethereum (crypto)

public export
Eq CurrencyCode where
  USD == USD = True
  EUR == EUR = True
  GBP == GBP = True
  JPY == JPY = True
  CHF == CHF = True
  CAD == CAD = True
  AUD == AUD = True
  NZD == NZD = True
  CNY == CNY = True
  INR == INR = True
  BRL == BRL = True
  MXN == MXN = True
  KRW == KRW = True
  SGD == SGD = True
  HKD == HKD = True
  SEK == SEK = True
  NOK == NOK = True
  DKK == DKK = True
  PLN == PLN = True
  RUB == RUB = True
  ZAR == ZAR = True
  TRY == TRY = True
  THB == THB = True
  MYR == MYR = True
  IDR == IDR = True
  PHP == PHP = True
  VND == VND = True
  AED == AED = True
  SAR == SAR = True
  ILS == ILS = True
  CZK == CZK = True
  HUF == HUF = True
  RON == RON = True
  BGN == BGN = True
  HRK == HRK = True
  ISK == ISK = True
  CLP == CLP = True
  COP == COP = True
  PEN == PEN = True
  ARS == ARS = True
  BTC == BTC = True
  ETH == ETH = True
  _ == _ = False

public export
Show CurrencyCode where
  show USD = "USD"
  show EUR = "EUR"
  show GBP = "GBP"
  show JPY = "JPY"
  show CHF = "CHF"
  show CAD = "CAD"
  show AUD = "AUD"
  show NZD = "NZD"
  show CNY = "CNY"
  show INR = "INR"
  show BRL = "BRL"
  show MXN = "MXN"
  show KRW = "KRW"
  show SGD = "SGD"
  show HKD = "HKD"
  show SEK = "SEK"
  show NOK = "NOK"
  show DKK = "DKK"
  show PLN = "PLN"
  show RUB = "RUB"
  show ZAR = "ZAR"
  show TRY = "TRY"
  show THB = "THB"
  show MYR = "MYR"
  show IDR = "IDR"
  show PHP = "PHP"
  show VND = "VND"
  show AED = "AED"
  show SAR = "SAR"
  show ILS = "ILS"
  show CZK = "CZK"
  show HUF = "HUF"
  show RON = "RON"
  show BGN = "BGN"
  show HRK = "HRK"
  show ISK = "ISK"
  show CLP = "CLP"
  show COP = "COP"
  show PEN = "PEN"
  show ARS = "ARS"
  show BTC = "BTC"
  show ETH = "ETH"

--------------------------------------------------------------------------------
-- Currency Properties
--------------------------------------------------------------------------------

||| Get the number of decimal places for a currency
public export
currencyDecimals : CurrencyCode -> Nat
currencyDecimals JPY = 0   -- Yen has no decimals
currencyDecimals KRW = 0   -- Won has no decimals
currencyDecimals VND = 0   -- Dong has no decimals
currencyDecimals BTC = 8   -- Bitcoin uses satoshis
currencyDecimals ETH = 18  -- Ethereum uses wei (but we cap at 8 for practicality)
currencyDecimals _   = 2   -- Most currencies use 2 decimal places

||| Get the currency symbol
public export
currencySymbol : CurrencyCode -> String
currencySymbol USD = "$"
currencySymbol EUR = "€"
currencySymbol GBP = "£"
currencySymbol JPY = "¥"
currencySymbol CHF = "Fr"
currencySymbol CAD = "C$"
currencySymbol AUD = "A$"
currencySymbol NZD = "NZ$"
currencySymbol CNY = "¥"
currencySymbol INR = "₹"
currencySymbol BRL = "R$"
currencySymbol MXN = "Mex$"
currencySymbol KRW = "₩"
currencySymbol SGD = "S$"
currencySymbol HKD = "HK$"
currencySymbol SEK = "kr"
currencySymbol NOK = "kr"
currencySymbol DKK = "kr"
currencySymbol PLN = "zł"
currencySymbol RUB = "₽"
currencySymbol ZAR = "R"
currencySymbol TRY = "₺"
currencySymbol THB = "฿"
currencySymbol MYR = "RM"
currencySymbol IDR = "Rp"
currencySymbol PHP = "₱"
currencySymbol VND = "₫"
currencySymbol AED = "د.إ"
currencySymbol SAR = "﷼"
currencySymbol ILS = "₪"
currencySymbol CZK = "Kč"
currencySymbol HUF = "Ft"
currencySymbol RON = "lei"
currencySymbol BGN = "лв"
currencySymbol HRK = "kn"
currencySymbol ISK = "kr"
currencySymbol CLP = "$"
currencySymbol COP = "$"
currencySymbol PEN = "S/"
currencySymbol ARS = "$"
currencySymbol BTC = "₿"
currencySymbol ETH = "Ξ"

||| Get the currency name
public export
currencyName : CurrencyCode -> String
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
-- Money Type
--------------------------------------------------------------------------------

||| Monetary value in minor units (cents, satoshis, etc.)
||| The currency code is part of the type to prevent mixing currencies
public export
data Money : CurrencyCode -> Type where
  MkMoney : (minorUnits : Integer) -> Money currency

||| Get the minor units (cents) from a Money value
public export
getMinorUnits : Money c -> Integer
getMinorUnits (MkMoney units) = units

||| Get the major units (dollars) as an integer (truncated)
public export
getMajorUnits : Money c -> Integer
getMajorUnits (MkMoney units) {c} =
  let decimals = currencyDecimals c
      divisor = pow 10 decimals
  in units `div` cast divisor

--------------------------------------------------------------------------------
-- Money Construction
--------------------------------------------------------------------------------

||| Create money from major units (dollars, euros, etc.)
public export
fromMajorUnits : (c : CurrencyCode) -> Integer -> Money c
fromMajorUnits c major =
  let decimals = currencyDecimals c
      multiplier = pow 10 decimals
  in MkMoney (major * cast multiplier)

||| Create money from minor units (cents, pence, etc.)
public export
fromMinorUnits : (c : CurrencyCode) -> Integer -> Money c
fromMinorUnits _ minor = MkMoney minor

||| Zero amount for a currency
public export
zero : (c : CurrencyCode) -> Money c
zero _ = MkMoney 0

--------------------------------------------------------------------------------
-- Money Arithmetic (Type-Safe)
--------------------------------------------------------------------------------

||| Add two monetary values (same currency)
public export
add : Money c -> Money c -> Money c
add (MkMoney a) (MkMoney b) = MkMoney (a + b)

||| Subtract two monetary values (same currency)
public export
subtract : Money c -> Money c -> Money c
subtract (MkMoney a) (MkMoney b) = MkMoney (a - b)

||| Multiply money by a scalar
public export
multiply : Money c -> Integer -> Money c
multiply (MkMoney units) n = MkMoney (units * n)

||| Divide money by a scalar (truncates)
public export
divide : Money c -> Integer -> Maybe (Money c)
divide _ 0 = Nothing
divide (MkMoney units) n = Just (MkMoney (units `div` n))

||| Negate a monetary value
public export
negate : Money c -> Money c
negate (MkMoney units) = MkMoney (-units)

||| Absolute value
public export
abs : Money c -> Money c
abs (MkMoney units) = MkMoney (if units < 0 then -units else units)

--------------------------------------------------------------------------------
-- Money Comparison
--------------------------------------------------------------------------------

public export
Eq (Money c) where
  (MkMoney a) == (MkMoney b) = a == b

public export
Ord (Money c) where
  compare (MkMoney a) (MkMoney b) = compare a b

||| Check if money is zero
public export
isZero : Money c -> Bool
isZero (MkMoney units) = units == 0

||| Check if money is positive
public export
isPositive : Money c -> Bool
isPositive (MkMoney units) = units > 0

||| Check if money is negative
public export
isNegative : Money c -> Bool
isNegative (MkMoney units) = units < 0

--------------------------------------------------------------------------------
-- Money Formatting
--------------------------------------------------------------------------------

||| Format money with symbol (e.g., "$123.45")
public export
formatMoney : Money c -> String
formatMoney m {c} =
  let units = getMinorUnits m
      decimals = currencyDecimals c
      symbol = currencySymbol c
      isNeg = units < 0
      absUnits = if isNeg then -units else units
      divisor = pow 10 decimals
      major = absUnits `div` cast divisor
      minor = absUnits `mod` cast divisor
      negSign = if isNeg then "-" else ""
  in if decimals == 0
       then negSign ++ symbol ++ show major
       else negSign ++ symbol ++ show major ++ "." ++ padLeft decimals '0' (show minor)
  where
    padLeft : Nat -> Char -> String -> String
    padLeft n c s =
      let len = length s
      in if len >= n then s else pack (replicate (minus n len) c) ++ s

||| Format money with code (e.g., "123.45 USD")
public export
formatMoneyWithCode : Money c -> String
formatMoneyWithCode m {c} =
  let units = getMinorUnits m
      decimals = currencyDecimals c
      isNeg = units < 0
      absUnits = if isNeg then -units else units
      divisor = pow 10 decimals
      major = absUnits `div` cast divisor
      minor = absUnits `mod` cast divisor
      negSign = if isNeg then "-" else ""
  in if decimals == 0
       then negSign ++ show major ++ " " ++ show c
       else negSign ++ show major ++ "." ++ padLeft decimals '0' (show minor) ++ " " ++ show c
  where
    padLeft : Nat -> Char -> String -> String
    padLeft n c s =
      let len = length s
      in if len >= n then s else pack (replicate (minus n len) c) ++ s

public export
Show (Money c) where
  show = formatMoneyWithCode

--------------------------------------------------------------------------------
-- Currency Errors
--------------------------------------------------------------------------------

||| Currency operation errors
public export
data CurrencyError
  = InvalidAmount String
  | CurrencyMismatch CurrencyCode CurrencyCode
  | DivisionByZero
  | Overflow
  | InvalidCurrencyCode String

public export
Show CurrencyError where
  show (InvalidAmount s) = "Invalid amount: " ++ s
  show (CurrencyMismatch a b) = "Currency mismatch: " ++ show a ++ " vs " ++ show b
  show DivisionByZero = "Division by zero"
  show Overflow = "Arithmetic overflow"
  show (InvalidCurrencyCode s) = "Invalid currency code: " ++ s

--------------------------------------------------------------------------------
-- Currency Code Parsing
--------------------------------------------------------------------------------

||| Parse currency code from string
public export
parseCurrencyCode : String -> Maybe CurrencyCode
parseCurrencyCode s = case toUpper s of
  "USD" => Just USD
  "EUR" => Just EUR
  "GBP" => Just GBP
  "JPY" => Just JPY
  "CHF" => Just CHF
  "CAD" => Just CAD
  "AUD" => Just AUD
  "NZD" => Just NZD
  "CNY" => Just CNY
  "INR" => Just INR
  "BRL" => Just BRL
  "MXN" => Just MXN
  "KRW" => Just KRW
  "SGD" => Just SGD
  "HKD" => Just HKD
  "SEK" => Just SEK
  "NOK" => Just NOK
  "DKK" => Just DKK
  "PLN" => Just PLN
  "RUB" => Just RUB
  "ZAR" => Just ZAR
  "TRY" => Just TRY
  "THB" => Just THB
  "MYR" => Just MYR
  "IDR" => Just IDR
  "PHP" => Just PHP
  "VND" => Just VND
  "AED" => Just AED
  "SAR" => Just SAR
  "ILS" => Just ILS
  "CZK" => Just CZK
  "HUF" => Just HUF
  "RON" => Just RON
  "BGN" => Just BGN
  "HRK" => Just HRK
  "ISK" => Just ISK
  "CLP" => Just CLP
  "COP" => Just COP
  "PEN" => Just PEN
  "ARS" => Just ARS
  "BTC" => Just BTC
  "ETH" => Just ETH
  _ => Nothing

||| Check if string is valid currency code
public export
isValidCurrencyCode : String -> Bool
isValidCurrencyCode s = isJust (parseCurrencyCode s)

--------------------------------------------------------------------------------
-- Exchange Rate Types
--------------------------------------------------------------------------------

||| Exchange rate between two currencies
||| Rate is expressed as: 1 unit of 'from' = rate units of 'to'
||| Rate is stored as integer with 6 decimal places (1000000 = 1.0)
public export
data ExchangeRate : CurrencyCode -> CurrencyCode -> Type where
  MkRate : (rateX1M : Integer) -> ExchangeRate from to

||| Create exchange rate from decimal (e.g., 1.25 = 1250000)
public export
mkExchangeRate : (from : CurrencyCode) -> (to : CurrencyCode) ->
                 Integer -> Integer -> ExchangeRate from to
mkExchangeRate _ _ whole decimal =
  MkRate (whole * 1000000 + decimal)

||| Convert money using exchange rate
public export
convert : ExchangeRate from to -> Money from -> Money to
convert (MkRate rateX1M) (MkMoney units) =
  MkMoney ((units * rateX1M) `div` 1000000)
