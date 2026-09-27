-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeRational - Rational number arithmetic with exact precision
|||
||| This module provides rational numbers (fractions) with automatic
||| normalization. Non-crash invariants and gcd-correctness theorems are
||| OWED — see `PROOF-NEEDS.md`.
module Proven.SafeRational
import Data.String
import Data.List

import public Proven.Core
import Data.List

%default total

--------------------------------------------------------------------------------
-- Rational Number Type
--------------------------------------------------------------------------------

||| A rational number represented as numerator/denominator
||| Invariant: denominator > 0, gcd(numerator, denominator) = 1
public export
record Rational where
  constructor MkRational
  numerator : Integer
  denominator : Integer  -- Always positive, never zero

||| Rational number errors
public export
data RationalError
  = DivisionByZero
  | Overflow

public export
Show RationalError where
  show DivisionByZero = "Rational division by zero"
  show Overflow = "Rational number overflow"

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

||| Greatest common divisor.  The subtraction algorithm carries an explicit
||| `a + b + 1` budget, so totality does not depend on the covering Integer
||| remainder operation.
gcd : Integer -> Integer -> Integer
gcd a b =
  let left : Nat = cast (abs a)
      right : Nat = cast (abs b)
  in cast (gcdNatFuel (S (left + right)) left right)
  where
    gcdNatFuel : (fuel : Nat) -> Nat -> Nat -> Nat
    gcdNatFuel Z left right = if left == 0 then right else left
    gcdNatFuel (S fuel) Z right = right
    gcdNatFuel (S fuel) left Z = left
    gcdNatFuel (S fuel) left right =
      if left == right then left
      else if left > right
        then gcdNatFuel fuel (minus left right) right
        else gcdNatFuel fuel left (minus right left)

||| Normalize a rational to lowest terms with positive denominator
normalize : Integer -> Integer -> Rational
normalize _ 0 = MkRational 0 1  -- Invalid, but total
normalize n d =
  let g = gcd n d
      sign = if d < 0 then -1 else 1
  in MkRational (sign * (n `div` g)) (abs (d `div` g))

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

||| Create a rational from numerator and denominator
||| Returns Nothing if denominator is zero
public export
rational : Integer -> Integer -> Maybe Rational
rational _ 0 = Nothing
rational n d = Just (normalize n d)

||| Unsafe creation - use only when denominator is known to be non-zero
public export
unsafeRational : Integer -> Integer -> Rational
unsafeRational n d = normalize n d

||| Create from an integer
public export
fromInteger : Integer -> Rational
fromInteger n = MkRational n 1

||| Zero
public export
zero : Rational
zero = MkRational 0 1

||| One
public export
one : Rational
one = MkRational 1 1

||| Negative one
public export
negOne : Rational
negOne = MkRational (-1) 1

||| One half
public export
half : Rational
half = MkRational 1 2

||| One third
public export
third : Rational
third = MkRational 1 3

||| One quarter
public export
quarter : Rational
quarter = MkRational 1 4

--------------------------------------------------------------------------------
-- Arithmetic Operations
--------------------------------------------------------------------------------

||| Addition
public export
add : Rational -> Rational -> Rational
add (MkRational n1 d1) (MkRational n2 d2) =
  normalize (n1 * d2 + n2 * d1) (d1 * d2)

||| Subtraction
public export
sub : Rational -> Rational -> Rational
sub (MkRational n1 d1) (MkRational n2 d2) =
  normalize (n1 * d2 - n2 * d1) (d1 * d2)

||| Multiplication
public export
mul : Rational -> Rational -> Rational
mul (MkRational n1 d1) (MkRational n2 d2) =
  normalize (n1 * n2) (d1 * d2)

||| Division (safe, returns Maybe)
public export
div : Rational -> Rational -> Maybe Rational
div _ (MkRational 0 _) = Nothing
div (MkRational n1 d1) (MkRational n2 d2) =
  Just (normalize (n1 * d2) (d1 * n2))

||| Division with error type
public export
divSafe : Rational -> Rational -> Either RationalError Rational
divSafe _ (MkRational 0 _) = Left DivisionByZero
divSafe r1 r2 =
  case div r1 r2 of
    Nothing => Left DivisionByZero
    Just r => Right r

||| Negation
public export
negate : Rational -> Rational
negate (MkRational n d) = MkRational (-n) d

||| Absolute value
public export
abs : Rational -> Rational
abs (MkRational n d) = MkRational (abs n) d

||| Reciprocal (safe)
public export
recip : Rational -> Maybe Rational
recip (MkRational 0 _) = Nothing
recip (MkRational n d) = Just (normalize d n)

||| Power with natural exponent
public export
pow : Rational -> Nat -> Rational
pow _ Z = one
pow r (S n) = mul r (pow r n)

--------------------------------------------------------------------------------
-- Num Instance
--------------------------------------------------------------------------------

public export
Num Rational where
  (+) = add
  (*) = mul
  fromInteger = SafeRational.fromInteger

public export
Neg Rational where
  negate = SafeRational.negate
  (-) = sub

--------------------------------------------------------------------------------
-- Comparison
--------------------------------------------------------------------------------

public export
Eq Rational where
  (==) (MkRational n1 d1) (MkRational n2 d2) = n1 * d2 == n2 * d1

public export
Ord Rational where
  compare (MkRational n1 d1) (MkRational n2 d2) = compare (n1 * d2) (n2 * d1)

||| Check if rational is zero
public export
isZero : Rational -> Bool
isZero (MkRational n _) = n == 0

||| Check if rational is positive
public export
isPositive : Rational -> Bool
isPositive (MkRational n _) = n > 0

||| Check if rational is negative
public export
isNegative : Rational -> Bool
isNegative (MkRational n _) = n < 0

||| Check if rational is an integer
public export
isInteger : Rational -> Bool
isInteger (MkRational _ d) = d == 1

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

||| Convert to Double (may lose precision)
public export
toDouble : Rational -> Double
toDouble (MkRational n d) = cast n / cast d

||| Approximate a Double as a rational with given max denominator
public export
fromDouble : (maxDenom : Integer) -> Double -> Rational
fromDouble maxDenom x =
  if x == 0 then zero
  else
    let target = if x < 0 then -x else x
        budget : Nat = S (cast (abs maxDenom))
        result = findBest budget target 0 1 1 0
    in if x < 0 then negate result else result
  where
    closer : Double -> Integer -> Integer -> Integer -> Integer -> Rational
    closer target a b c d =
      if b == 0 then MkRational c d
      else if d == 0 then MkRational a b
      else if abs (toDouble (MkRational a b) - target) <
              abs (toDouble (MkRational c d) - target)
        then MkRational a b
        else MkRational c d

    findBest : (fuel : Nat) -> Double ->
               Integer -> Integer -> Integer -> Integer -> Rational
    findBest Z target a b c d = closer target a b c d
    findBest (S fuel) target a b c d =
      let mediantN = a + c
          mediantD = b + d
      in if mediantD > maxDenom
           then closer target a b c d
           else if toDouble (MkRational mediantN mediantD) < target
                  then findBest fuel target mediantN mediantD c d
                  else findBest fuel target a b mediantN mediantD

||| Floor division (towards negative infinity)
public export
floor : Rational -> Integer
floor (MkRational n d) =
  if n >= 0 then n `div` d
  else (n - d + 1) `div` d

||| Ceiling division (towards positive infinity)
public export
ceiling : Rational -> Integer
ceiling (MkRational n d) =
  if n >= 0 then (n + d - 1) `div` d
  else n `div` d

||| Truncate towards zero
public export
truncate : Rational -> Integer
truncate (MkRational n d) = n `div` d

||| Round to nearest integer
public export
round : Rational -> Integer
round r =
  let f = floor r
      diff = sub r (fromInteger f)
  in if diff >= half then f + 1 else f

||| Get fractional part (always non-negative, less than 1)
public export
frac : Rational -> Rational
frac r = sub r (fromInteger (floor r))

--------------------------------------------------------------------------------
-- Mediant and Continued Fractions
--------------------------------------------------------------------------------

||| Mediant of two rationals (useful for Stern-Brocot tree)
public export
mediant : Rational -> Rational -> Rational
mediant (MkRational n1 d1) (MkRational n2 d2) =
  normalize (n1 + n2) (d1 + d2)

||| Get the sign: -1, 0, or 1
public export
signum : Rational -> Integer
signum (MkRational n _) =
  if n > 0 then 1
  else if n < 0 then -1
  else 0

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Show Rational where
  show (MkRational n 1) = show n
  show (MkRational n d) = show n ++ "/" ++ show d
