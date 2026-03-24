-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeMath - Arithmetic operations that cannot crash
|||
||| This module provides safe arithmetic operations that handle edge cases
||| like division by zero, overflow, and underflow without throwing exceptions.
||| All operations are total and return Option/Result types for failure cases.
module Proven.SafeMath
import Data.String
import Data.List

import public Proven.Core
import public Proven.SafeMath.Nat
import public Proven.SafeMath.Int
import public Proven.SafeMath.Bounded
import public Proven.SafeMath.Proofs

%default total

--------------------------------------------------------------------------------
-- Safe Division
--------------------------------------------------------------------------------

||| Safe division that returns Nothing on division by zero
||| @ numerator   The dividend
||| @ denominator The divisor
||| @ returns     Just (numerator / denominator) if denominator /= 0, Nothing otherwise
public export
safeDiv : (numerator : Integer) -> (denominator : Integer) -> Maybe Integer
safeDiv _ 0 = Nothing
safeDiv n d = Just (Prelude.div n d)

||| Safe division with a default value for division by zero
public export
divOr : (defVal : Integer) -> (numerator : Integer) -> (denominator : Integer) -> Integer
divOr defVal n d = withDefault defVal (safeDiv n d)

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
safeMod : (numerator : Integer) -> (denominator : Integer) -> Maybe Integer
safeMod _ 0 = Nothing
safeMod n d = Just (Prelude.mod n d)

||| Safe modulo for natural numbers
public export
modNat : (numerator : Nat) -> (denominator : Nat) -> Maybe Nat
modNat _ Z = Nothing
modNat n d = Just (n `mod` d)

--------------------------------------------------------------------------------
-- Checked Arithmetic (Overflow Detection)
-- Note: maxInt64, minInt64, fitsInt64, addInt64, subInt64, mulInt64
-- are provided by Proven.SafeMath.Int (re-exported via import public).
--------------------------------------------------------------------------------

||| Safe addition with overflow detection for 64-bit integers
||| Returns Nothing if the result would overflow
public export
addChecked : Integer -> Integer -> Maybe Integer
addChecked = addInt64

||| Safe subtraction with underflow detection for 64-bit integers
public export
subChecked : Integer -> Integer -> Maybe Integer
subChecked = subInt64

||| Safe multiplication with overflow detection for 64-bit integers
public export
mulChecked : Integer -> Integer -> Maybe Integer
mulChecked = mulInt64

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
percentOf : (percent : Integer) -> (whole : Integer) -> Maybe Integer
percentOf percent whole = do
  result <- mulChecked percent whole
  safeDiv result 100

||| Calculate what percentage a is of b
||| asPercent 50 200 = Just 25 (50 is 25% of 200)
public export
asPercent : (part : Integer) -> (whole : Integer) -> Maybe Integer
asPercent part whole = do
  scaled <- mulChecked part 100
  safeDiv scaled whole
