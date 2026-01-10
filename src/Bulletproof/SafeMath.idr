-- SPDX-License-Identifier: Palimpsest-MPL
||| SafeMath - Arithmetic operations that cannot crash
|||
||| This module provides safe arithmetic operations that handle edge cases
||| like division by zero, overflow, and underflow without throwing exceptions.
||| All operations are total and return Option/Result types for failure cases.
module Bulletproof.SafeMath

import public Bulletproof.Core
import public Bulletproof.SafeMath.Nat
import public Bulletproof.SafeMath.Int
import public Bulletproof.SafeMath.Bounded
import public Bulletproof.SafeMath.Proofs

%default total

--------------------------------------------------------------------------------
-- Safe Division
--------------------------------------------------------------------------------

||| Safe division that returns Nothing on division by zero
||| @ numerator   The dividend
||| @ denominator The divisor
||| @ returns     Just (numerator / denominator) if denominator /= 0, Nothing otherwise
public export
div : (numerator : Integer) -> (denominator : Integer) -> Maybe Integer
div _ 0 = Nothing
div n d = Just (n `div` d)

||| Safe division with a default value for division by zero
public export
divOr : (default : Integer) -> (numerator : Integer) -> (denominator : Integer) -> Integer
divOr def n d = withDefault def (div n d)

||| Safe division for natural numbers
public export
divNat : (numerator : Nat) -> (denominator : Nat) -> Maybe Nat
divNat _ Z = Nothing
divNat n d = Just (n `div` d)

--------------------------------------------------------------------------------
-- Safe Modulo
--------------------------------------------------------------------------------

||| Safe modulo that returns Nothing on division by zero
public export
mod : (numerator : Integer) -> (denominator : Integer) -> Maybe Integer
mod _ 0 = Nothing
mod n d = Just (n `mod` d)

||| Safe modulo for natural numbers
public export
modNat : (numerator : Nat) -> (denominator : Nat) -> Maybe Nat
modNat _ Z = Nothing
modNat n d = Just (n `mod` d)

--------------------------------------------------------------------------------
-- Checked Arithmetic (Overflow Detection)
--------------------------------------------------------------------------------

||| Maximum value for signed 64-bit integer
public export
maxInt64 : Integer
maxInt64 = 9223372036854775807

||| Minimum value for signed 64-bit integer
public export
minInt64 : Integer
minInt64 = -9223372036854775808

||| Check if an integer fits in 64 bits
public export
fitsInt64 : Integer -> Bool
fitsInt64 n = n >= minInt64 && n <= maxInt64

||| Safe addition with overflow detection for 64-bit integers
||| Returns Nothing if the result would overflow
public export
addChecked : Integer -> Integer -> Maybe Integer
addChecked a b =
  let result = a + b
  in if fitsInt64 result then Just result else Nothing

||| Safe subtraction with underflow detection for 64-bit integers
public export
subChecked : Integer -> Integer -> Maybe Integer
subChecked a b =
  let result = a - b
  in if fitsInt64 result then Just result else Nothing

||| Safe multiplication with overflow detection for 64-bit integers
public export
mulChecked : Integer -> Integer -> Maybe Integer
mulChecked a b =
  let result = a * b
  in if fitsInt64 result then Just result else Nothing

--------------------------------------------------------------------------------
-- Safe Absolute Value
--------------------------------------------------------------------------------

||| Safe absolute value that handles MIN_INT correctly
||| Regular abs(MIN_INT) overflows because -MIN_INT > MAX_INT
||| This version returns Nothing in that case
public export
absSafe : Integer -> Maybe Integer
absSafe n =
  if n == minInt64
    then Nothing  -- Cannot represent |MIN_INT| in signed 64-bit
    else Just (abs n)

||| Absolute value that clamps to MAX_INT instead of overflowing
public export
absClamped : Integer -> Integer
absClamped n =
  if n == minInt64
    then maxInt64
    else abs n

--------------------------------------------------------------------------------
-- Safe Power
--------------------------------------------------------------------------------

||| Safe exponentiation for natural numbers
||| Returns Nothing if the result would be too large
public export
powChecked : (base : Integer) -> (exp : Nat) -> Maybe Integer
powChecked base exp =
  let result = power base exp
  in if fitsInt64 result then Just result else Nothing
  where
    power : Integer -> Nat -> Integer
    power _ Z = 1
    power b (S n) = b * power b n

--------------------------------------------------------------------------------
-- Clamping Operations
--------------------------------------------------------------------------------

||| Clamp a value to a range [lo, hi]
public export
clamp : (lo : Integer) -> (hi : Integer) -> (value : Integer) -> Integer
clamp lo hi value =
  if value < lo then lo
  else if value > hi then hi
  else value

||| Clamp to non-negative (>= 0)
public export
clampNonNegative : Integer -> Integer
clampNonNegative n = if n < 0 then 0 else n

||| Convert Integer to Nat safely, clamping negative values to 0
public export
toNatClamped : Integer -> Nat
toNatClamped n =
  if n < 0 then Z
  else fromInteger n

--------------------------------------------------------------------------------
-- Safe Comparison Utilities
--------------------------------------------------------------------------------

||| Compare two integers, returning an ordering
public export
compareInt : Integer -> Integer -> Ordering
compareInt a b =
  if a < b then LT
  else if a > b then GT
  else EQ

||| Minimum of two integers
public export
minInt : Integer -> Integer -> Integer
minInt a b = if a <= b then a else b

||| Maximum of two integers
public export
maxInt : Integer -> Integer -> Integer
maxInt a b = if a >= b then a else b

--------------------------------------------------------------------------------
-- Percentage and Ratio Operations
--------------------------------------------------------------------------------

||| Calculate percentage safely (returns Nothing on division by zero)
||| percentOf 50 200 = Just 100 (50% of 200)
public export
percentOf : (percent : Integer) -> (total : Integer) -> Maybe Integer
percentOf percent total = do
  result <- mulChecked percent total
  div result 100

||| Calculate what percentage a is of b
||| asPercent 50 200 = Just 25 (50 is 25% of 200)
public export
asPercent : (part : Integer) -> (whole : Integer) -> Maybe Integer
asPercent part whole = do
  scaled <- mulChecked part 100
  div scaled whole
