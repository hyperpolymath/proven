-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Safe operations on Natural numbers
|||
||| Natural numbers (Nat) are non-negative by construction, eliminating
||| an entire class of bugs related to negative values.
module Proven.SafeMath.Nat

import Proven.Core
import Data.Nat

%default total

--------------------------------------------------------------------------------
-- Safe Subtraction for Nat
--------------------------------------------------------------------------------

||| Safe subtraction that returns Nothing if result would be negative
||| Unlike the built-in minus which truncates to 0, this tells you when
||| subtraction isn't possible.
public export
subSafe : (a : Nat) -> (b : Nat) -> Maybe Nat
subSafe a b = if b <= a then Just (minus a b) else Nothing

||| Subtract with explicit underflow handling
public export
data SubResult = Underflow Nat | Exact Nat

||| Subtraction that tells you exactly what happened
public export
subExact : (a : Nat) -> (b : Nat) -> SubResult
subExact a b =
  if b <= a
    then Exact (minus a b)
    else Underflow (minus b a)  -- How much we're under by

--------------------------------------------------------------------------------
-- Safe Division
--------------------------------------------------------------------------------

||| Safe division for natural numbers
public export
divSafe : (n : Nat) -> (d : Nat) -> Maybe Nat
divSafe _ Z = Nothing
divSafe n d = Just (div n d)

||| Division with remainder, safe from division by zero
public export
divMod : (n : Nat) -> (d : Nat) -> Maybe (Nat, Nat)
divMod _ Z = Nothing
divMod n d = Just (div n d, mod n d)

--------------------------------------------------------------------------------
-- Predecessor
--------------------------------------------------------------------------------

||| Safe predecessor that returns Nothing for 0
public export
predSafe : Nat -> Maybe Nat
predSafe Z = Nothing
predSafe (S n) = Just n

--------------------------------------------------------------------------------
-- Bounded Operations
--------------------------------------------------------------------------------

||| Add with upper bound - caps at maximum
public export
addCapped : (max : Nat) -> Nat -> Nat -> Nat
addCapped max a b =
  let sum = a + b
  in if sum > max then max else sum

||| Multiply with upper bound
public export
mulCapped : (max : Nat) -> Nat -> Nat -> Nat
mulCapped max a b =
  let prod = a * b
  in if prod > max then max else prod

--------------------------------------------------------------------------------
-- Range Operations
--------------------------------------------------------------------------------

||| Check if a natural number is in range [lo, hi]
public export
inRange : (lo : Nat) -> (hi : Nat) -> (n : Nat) -> Bool
inRange lo hi n = n >= lo && n <= hi

||| Clamp a natural number to a range
public export
clampNat : (lo : Nat) -> (hi : Nat) -> (n : Nat) -> Nat
clampNat lo hi n =
  if n < lo then lo
  else if n > hi then hi
  else n

--------------------------------------------------------------------------------
-- GCD and LCM
--------------------------------------------------------------------------------

||| Greatest Common Divisor (Euclidean algorithm)
public export
gcd : Nat -> Nat -> Nat
gcd a Z = a
gcd a b = gcd b (mod a b)

||| Least Common Multiple (safe from overflow in result, not in computation)
public export
lcm : Nat -> Nat -> Maybe Nat
lcm Z _ = Just Z
lcm _ Z = Just Z
lcm a b = Just (div (a * b) (gcd a b))

--------------------------------------------------------------------------------
-- Factorial and Combinatorics
--------------------------------------------------------------------------------

||| Factorial (note: grows very fast!)
public export
factorial : Nat -> Nat
factorial Z = 1
factorial (S n) = (S n) * factorial n

||| Safe factorial with overflow detection
public export
factorialSafe : Nat -> Maybe Nat
factorialSafe n =
  if n > 20  -- 21! overflows 64-bit
    then Nothing
    else Just (factorial n)

||| Binomial coefficient (n choose k)
public export
choose : (n : Nat) -> (k : Nat) -> Nat
choose n k =
  if k > n then 0
  else div (factorial n) (factorial k * factorial (minus n k))

--------------------------------------------------------------------------------
-- Digit Operations
--------------------------------------------------------------------------------

||| Count digits in base 10
public export
digitCount : Nat -> Nat
digitCount Z = 1
digitCount n = go n 0
  where
    go : Nat -> Nat -> Nat
    go Z acc = acc
    go m acc = go (div m 10) (S acc)

||| Extract digits in base 10 (least significant first)
public export
digits : Nat -> List Nat
digits Z = [0]
digits n = go n []
  where
    go : Nat -> List Nat -> List Nat
    go Z acc = acc
    go m acc = go (div m 10) (mod m 10 :: acc)

||| Sum of digits
public export
digitSum : Nat -> Nat
digitSum = sum . digits
