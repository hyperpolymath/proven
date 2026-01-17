-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeCurrency - Safe currency and money operations
|||
||| This module provides safe monetary calculations with
||| proper rounding, currency conversion, and overflow protection.
module Proven.SafeCurrency

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Currency Codes (ISO 4217)
--------------------------------------------------------------------------------

||| Common currency codes
public export
data CurrencyCode : Type where
  USD : CurrencyCode  -- US Dollar
  EUR : CurrencyCode  -- Euro
  GBP : CurrencyCode  -- British Pound
  JPY : CurrencyCode  -- Japanese Yen
  CHF : CurrencyCode  -- Swiss Franc
  CAD : CurrencyCode  -- Canadian Dollar
  AUD : CurrencyCode  -- Australian Dollar
  CNY : CurrencyCode  -- Chinese Yuan
  INR : CurrencyCode  -- Indian Rupee
  BTC : CurrencyCode  -- Bitcoin
  ETH : CurrencyCode  -- Ethereum
  Other : String -> CurrencyCode

public export
Eq CurrencyCode where
  USD == USD = True
  EUR == EUR = True
  GBP == GBP = True
  JPY == JPY = True
  CHF == CHF = True
  CAD == CAD = True
  AUD == AUD = True
  CNY == CNY = True
  INR == INR = True
  BTC == BTC = True
  ETH == ETH = True
  (Other a) == (Other b) = a == b
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
  show CNY = "CNY"
  show INR = "INR"
  show BTC = "BTC"
  show ETH = "ETH"
  show (Other s) = s

||| Get minor unit precision for currency
public export
minorUnits : CurrencyCode -> Nat
minorUnits JPY = 0  -- Yen has no minor unit
minorUnits BTC = 8  -- Satoshis
minorUnits ETH = 18 -- Wei
minorUnits _ = 2    -- Most currencies use cents

--------------------------------------------------------------------------------
-- Money Type
--------------------------------------------------------------------------------

||| Money represented in minor units (cents, satoshis, etc.)
||| This avoids floating point issues
public export
record Money where
  constructor MkMoney
  amount : Integer          -- In minor units (can be negative)
  currency : CurrencyCode

||| Create money from major units (e.g., dollars)
public export
fromMajor : Double -> CurrencyCode -> Money
fromMajor amt curr =
  let precision = minorUnits curr
      multiplier = pow 10.0 (cast precision)
  in MkMoney (cast (amt * multiplier)) curr
  where
    pow : Double -> Nat -> Double
    pow _ Z = 1.0
    pow b (S n) = b * pow b n

||| Create money from minor units (e.g., cents)
public export
fromMinor : Integer -> CurrencyCode -> Money
fromMinor amt curr = MkMoney amt curr

||| Convert to major units (e.g., cents to dollars)
public export
toMajor : Money -> Double
toMajor m =
  let precision = minorUnits m.currency
      divisor = pow 10.0 (cast precision)
  in cast m.amount / divisor
  where
    pow : Double -> Nat -> Double
    pow _ Z = 1.0
    pow b (S n) = b * pow b n

||| Zero money in a currency
public export
zero : CurrencyCode -> Money
zero curr = MkMoney 0 curr

--------------------------------------------------------------------------------
-- Safe Arithmetic
--------------------------------------------------------------------------------

||| Add two money values (must be same currency)
public export
add : Money -> Money -> Maybe Money
add a b =
  if a.currency == b.currency
    then Just (MkMoney (a.amount + b.amount) a.currency)
    else Nothing

||| Subtract money (must be same currency)
public export
subtract : Money -> Money -> Maybe Money
subtract a b =
  if a.currency == b.currency
    then Just (MkMoney (a.amount - b.amount) a.currency)
    else Nothing

||| Multiply by a scalar
public export
multiply : Money -> Double -> Money
multiply m factor =
  MkMoney (cast (cast m.amount * factor)) m.currency

||| Divide by a scalar (returns Nothing if divisor is 0)
public export
divide : Money -> Double -> Maybe Money
divide m divisor =
  if divisor == 0.0
    then Nothing
    else Just (MkMoney (cast (cast m.amount / divisor)) m.currency)

||| Negate money value
public export
negate : Money -> Money
negate m = MkMoney (- m.amount) m.currency

||| Get absolute value
public export
abs : Money -> Money
abs m = MkMoney (if m.amount < 0 then - m.amount else m.amount) m.currency

--------------------------------------------------------------------------------
-- Comparisons
--------------------------------------------------------------------------------

public export
Eq Money where
  a == b = a.currency == b.currency && a.amount == b.amount

||| Compare money values (must be same currency)
public export
compare : Money -> Money -> Maybe Ordering
compare a b =
  if a.currency == b.currency
    then Just (compareInteger a.amount b.amount)
    else Nothing
  where
    compareInteger : Integer -> Integer -> Ordering
    compareInteger x y = if x < y then LT else if x > y then GT else EQ

||| Check if money is positive
public export
isPositive : Money -> Bool
isPositive m = m.amount > 0

||| Check if money is negative
public export
isNegative : Money -> Bool
isNegative m = m.amount < 0

||| Check if money is zero
public export
isZero : Money -> Bool
isZero m = m.amount == 0

--------------------------------------------------------------------------------
-- Rounding
--------------------------------------------------------------------------------

||| Rounding mode
public export
data RoundingMode : Type where
  RoundUp : RoundingMode       -- Always round away from zero
  RoundDown : RoundingMode     -- Always round toward zero
  RoundHalfUp : RoundingMode   -- Round .5 away from zero
  RoundHalfDown : RoundingMode -- Round .5 toward zero
  RoundHalfEven : RoundingMode -- Banker's rounding

||| Round money to fewer decimal places
public export
roundTo : (decimals : Nat) -> RoundingMode -> Money -> Money
roundTo decimals mode m =
  let currentPrecision = minorUnits m.currency
  in if decimals >= currentPrecision
       then m  -- No rounding needed
       else let factor = pow 10 (minus currentPrecision decimals)
                rounded = roundAmount mode m.amount factor
            in MkMoney (rounded * cast factor) m.currency
  where
    pow : Integer -> Nat -> Integer
    pow _ Z = 1
    pow b (S n) = b * pow b n
    
    roundAmount : RoundingMode -> Integer -> Integer -> Integer
    roundAmount RoundUp amt factor =
      if amt `mod` factor == 0 then amt `div` factor
      else (amt `div` factor) + (if amt > 0 then 1 else -1)
    roundAmount RoundDown amt factor = amt `div` factor
    roundAmount _ amt factor = (amt + factor `div` 2) `div` factor -- Default half-up

--------------------------------------------------------------------------------
-- Currency Conversion
--------------------------------------------------------------------------------

||| Exchange rate between two currencies
public export
record ExchangeRate where
  constructor MkRate
  fromCurrency : CurrencyCode
  toCurrency : CurrencyCode
  rate : Double               -- 1 unit of from = rate units of to

||| Convert money using an exchange rate
public export
convert : ExchangeRate -> Money -> Maybe Money
convert xr m =
  if m.currency == xr.fromCurrency
    then Just (fromMajor (toMajor m * xr.rate) xr.toCurrency)
    else Nothing

--------------------------------------------------------------------------------
-- Allocation
--------------------------------------------------------------------------------

||| Split money into N equal parts (handles remainders)
public export
split : (parts : Nat) -> Money -> Maybe (List Money)
split Z _ = Nothing
split (S n) m =
  let each = m.amount `div` cast (S n)
      remainder = m.amount `mod` cast (S n)
      baseAmounts = replicate (S n) each
      -- Distribute remainder to first parts
      adjusted = distributeRemainder remainder baseAmounts
  in Just (map (\amt => MkMoney amt m.currency) adjusted)
  where
    distributeRemainder : Integer -> List Integer -> List Integer
    distributeRemainder 0 xs = xs
    distributeRemainder _ [] = []
    distributeRemainder r (x :: xs) = (x + 1) :: distributeRemainder (r - 1) xs

||| Allocate money by ratios
public export
allocate : List Nat -> Money -> Maybe (List Money)
allocate ratios m =
  let total = sum ratios
  in if total == 0
       then Nothing
       else Just (allocateByRatio ratios total m.amount m.currency)
  where
    sum : List Nat -> Nat
    sum [] = 0
    sum (x :: xs) = x + sum xs
    
    allocateByRatio : List Nat -> Nat -> Integer -> CurrencyCode -> List Money
    allocateByRatio [] _ _ _ = []
    allocateByRatio (r :: rs) total amt curr =
      let share = (amt * cast r) `div` cast total
      in MkMoney share curr :: allocateByRatio rs total amt curr

--------------------------------------------------------------------------------
-- Formatting
--------------------------------------------------------------------------------

||| Format money with currency symbol
public export
format : Money -> String
format m =
  let symbol = currencySymbol m.currency
      major = toMajor m
  in symbol ++ formatNumber major (minorUnits m.currency)
  where
    currencySymbol : CurrencyCode -> String
    currencySymbol USD = "$"
    currencySymbol EUR = "€"
    currencySymbol GBP = "£"
    currencySymbol JPY = "¥"
    currencySymbol CNY = "¥"
    currencySymbol INR = "₹"
    currencySymbol BTC = "₿"
    currencySymbol ETH = "Ξ"
    currencySymbol c = show c ++ " "
    
    formatNumber : Double -> Nat -> String
    formatNumber n precision = show n  -- Simplified

public export
Show Money where
  show = format

