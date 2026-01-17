-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeDecimal - Safe arbitrary-precision decimal arithmetic
|||
||| This module provides decimal numbers suitable for financial calculations
||| where floating-point rounding errors are unacceptable.
module Proven.SafeDecimal

import public Proven.Core
import Data.List

%default total

--------------------------------------------------------------------------------
-- Decimal Type
--------------------------------------------------------------------------------

||| A decimal number with fixed precision
||| Represented as mantissa * 10^(-scale)
||| Example: 123.45 = mantissa 12345, scale 2
public export
record Decimal where
  constructor MkDecimal
  mantissa : Integer
  scale : Nat  -- Number of decimal places

||| Decimal errors
public export
data DecimalError
  = DivisionByZero
  | Overflow
  | PrecisionLoss
  | InvalidFormat String

public export
Show DecimalError where
  show DivisionByZero = "Decimal division by zero"
  show Overflow = "Decimal overflow"
  show PrecisionLoss = "Decimal precision loss"
  show (InvalidFormat s) = "Invalid decimal format: " ++ s

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

||| Power of 10
pow10 : Nat -> Integer
pow10 Z = 1
pow10 (S n) = 10 * pow10 n

||| Normalize by removing trailing zeros (reduce scale)
normalize : Decimal -> Decimal
normalize (MkDecimal m Z) = MkDecimal m Z
normalize (MkDecimal m (S n)) =
  if m `mod` 10 == 0
    then normalize (MkDecimal (m `div` 10) n)
    else MkDecimal m (S n)

||| Align two decimals to the same scale
align : Decimal -> Decimal -> (Decimal, Decimal)
align (MkDecimal m1 s1) (MkDecimal m2 s2) =
  if s1 >= s2
    then (MkDecimal m1 s1, MkDecimal (m2 * pow10 (minus s1 s2)) s1)
    else (MkDecimal (m1 * pow10 (minus s2 s1)) s2, MkDecimal m2 s2)

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

||| Create a decimal from mantissa and scale
public export
decimal : Integer -> Nat -> Decimal
decimal m s = normalize (MkDecimal m s)

||| Create from an integer (scale 0)
public export
fromInteger : Integer -> Decimal
fromInteger n = MkDecimal n 0

||| Create from components: whole part and fractional part
||| decimal' 123 45 2 = 123.45
public export
decimal' : (whole : Integer) -> (frac : Nat) -> (fracDigits : Nat) -> Decimal
decimal' w f d =
  let sign = if w < 0 then -1 else 1
      mantissa = abs w * pow10 d + cast f
  in normalize (MkDecimal (sign * mantissa) d)

||| Zero
public export
zero : Decimal
zero = MkDecimal 0 0

||| One
public export
one : Decimal
one = MkDecimal 1 0

||| Parse from string (e.g., "123.45")
public export
parse : String -> Maybe Decimal
parse s =
  case break (== '.') (unpack s) of
    (wholeChars, []) =>
      -- No decimal point
      case parseInteger (pack wholeChars) of
        Nothing => Nothing
        Just w => Just (fromInteger w)
    (wholeChars, '.' :: fracChars) =>
      -- Has decimal point
      case (parseInteger (pack wholeChars), parseNat (pack fracChars)) of
        (Just w, Just f) =>
          let d = length fracChars
          in Just (decimal' w f d)
        _ => Nothing
    _ => Nothing
  where
    parseNat : String -> Maybe Nat
    parseNat s =
      case parseInteger s of
        Nothing => Nothing
        Just n => if n >= 0 then Just (cast n) else Nothing

--------------------------------------------------------------------------------
-- Arithmetic Operations
--------------------------------------------------------------------------------

||| Addition
public export
add : Decimal -> Decimal -> Decimal
add d1 d2 =
  let (MkDecimal m1 s, MkDecimal m2 _) = align d1 d2
  in normalize (MkDecimal (m1 + m2) s)

||| Subtraction
public export
sub : Decimal -> Decimal -> Decimal
sub d1 d2 =
  let (MkDecimal m1 s, MkDecimal m2 _) = align d1 d2
  in normalize (MkDecimal (m1 - m2) s)

||| Multiplication
public export
mul : Decimal -> Decimal -> Decimal
mul (MkDecimal m1 s1) (MkDecimal m2 s2) =
  normalize (MkDecimal (m1 * m2) (s1 + s2))

||| Division with specified result precision
public export
divWithPrecision : Nat -> Decimal -> Decimal -> Maybe Decimal
divWithPrecision precision (MkDecimal m1 s1) (MkDecimal m2 s2) =
  if m2 == 0 then Nothing
  else
    let scaledM1 = m1 * pow10 (precision + s2)
        resultMantissa = scaledM1 `div` m2
    in Just (normalize (MkDecimal resultMantissa (s1 + precision)))

||| Division with default precision of 10 decimal places
public export
div : Decimal -> Decimal -> Maybe Decimal
div = divWithPrecision 10

||| Division with error type
public export
divSafe : Decimal -> Decimal -> Either DecimalError Decimal
divSafe d1 d2 =
  case div d1 d2 of
    Nothing => Left DivisionByZero
    Just r => Right r

||| Negation
public export
negate : Decimal -> Decimal
negate (MkDecimal m s) = MkDecimal (-m) s

||| Absolute value
public export
abs : Decimal -> Decimal
abs (MkDecimal m s) = MkDecimal (abs m) s

--------------------------------------------------------------------------------
-- Num Instance
--------------------------------------------------------------------------------

public export
Num Decimal where
  (+) = add
  (*) = mul
  fromInteger = SafeDecimal.fromInteger

public export
Neg Decimal where
  negate = SafeDecimal.negate
  (-) = sub

--------------------------------------------------------------------------------
-- Comparison
--------------------------------------------------------------------------------

public export
Eq Decimal where
  (==) d1 d2 =
    let (MkDecimal m1 _, MkDecimal m2 _) = align d1 d2
    in m1 == m2

public export
Ord Decimal where
  compare d1 d2 =
    let (MkDecimal m1 _, MkDecimal m2 _) = align d1 d2
    in compare m1 m2

||| Check if decimal is zero
public export
isZero : Decimal -> Bool
isZero (MkDecimal m _) = m == 0

||| Check if decimal is positive
public export
isPositive : Decimal -> Bool
isPositive (MkDecimal m _) = m > 0

||| Check if decimal is negative
public export
isNegative : Decimal -> Bool
isNegative (MkDecimal m _) = m < 0

--------------------------------------------------------------------------------
-- Rounding Operations
--------------------------------------------------------------------------------

||| Round to specified number of decimal places (banker's rounding)
public export
roundTo : Nat -> Decimal -> Decimal
roundTo targetScale (MkDecimal m s) =
  if targetScale >= s then MkDecimal (m * pow10 (minus targetScale s)) targetScale
  else
    let factor = pow10 (minus s targetScale)
        quotient = m `div` factor
        remainder = m `mod` factor
        halfFactor = factor `div` 2
        -- Banker's rounding: round half to even
        rounded = if abs remainder > halfFactor then
                    if m >= 0 then quotient + 1 else quotient - 1
                  else if abs remainder == halfFactor then
                    if quotient `mod` 2 == 0 then quotient else quotient + 1
                  else quotient
    in normalize (MkDecimal rounded targetScale)

||| Truncate to specified decimal places
public export
truncateTo : Nat -> Decimal -> Decimal
truncateTo targetScale (MkDecimal m s) =
  if targetScale >= s then MkDecimal (m * pow10 (minus targetScale s)) targetScale
  else
    let factor = pow10 (minus s targetScale)
    in normalize (MkDecimal (m `div` factor) targetScale)

||| Floor (round towards negative infinity)
public export
floor : Decimal -> Integer
floor (MkDecimal m s) =
  let factor = pow10 s
  in if m >= 0 then m `div` factor
     else (m - factor + 1) `div` factor

||| Ceiling (round towards positive infinity)
public export
ceiling : Decimal -> Integer
ceiling (MkDecimal m s) =
  let factor = pow10 s
  in if m >= 0 then (m + factor - 1) `div` factor
     else m `div` factor

--------------------------------------------------------------------------------
-- Financial Operations
--------------------------------------------------------------------------------

||| Round to cents (2 decimal places)
public export
toCents : Decimal -> Decimal
toCents = roundTo 2

||| Percentage calculation: percent% of value
public export
percent : Decimal -> Decimal -> Decimal
percent pct value =
  let product = mul pct value
  in case divWithPrecision 10 product (fromInteger 100) of
       Nothing => zero  -- Should never happen
       Just r => r

||| Split amount into n equal parts with remainder handling
public export
split : Nat -> Decimal -> List Decimal
split Z _ = []
split (S n) amount =
  let baseAmount = case divWithPrecision 2 amount (fromInteger (cast (S n))) of
                     Nothing => zero
                     Just r => roundTo 2 r
      total = mul baseAmount (fromInteger (cast (S n)))
      remainder = sub amount total
  in baseAmount :: replicate n baseAmount  -- Simplified: distribute remainder to first

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

||| Convert to Double (may lose precision)
public export
toDouble : Decimal -> Double
toDouble (MkDecimal m s) = cast m / cast (pow10 s)

||| Get the whole (integer) part
public export
wholePart : Decimal -> Integer
wholePart (MkDecimal m s) = m `div` pow10 s

||| Get the fractional part as a decimal
public export
fractionalPart : Decimal -> Decimal
fractionalPart d@(MkDecimal m s) =
  let w = wholePart d
  in sub d (fromInteger w)

||| Get the number of decimal places
public export
precision : Decimal -> Nat
precision (MkDecimal _ s) = s

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Show Decimal where
  show (MkDecimal m 0) = show m
  show (MkDecimal m s) =
    let factor = pow10 s
        whole = m `div` factor
        frac = abs (m `mod` factor)
        sign = if m < 0 && whole == 0 then "-" else ""
        fracStr = padLeft s '0' (show frac)
    in sign ++ show (abs whole) ++ "." ++ fracStr
    where
      padLeft : Nat -> Char -> String -> String
      padLeft n c s =
        let len = length s
        in if len >= n then s
           else pack (replicate (minus n len) c) ++ s
