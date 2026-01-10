-- SPDX-License-Identifier: Palimpsest-MPL
||| Bounded arithmetic - integers with compile-time and runtime bounds
module Bulletproof.SafeMath.Bounded

import Bulletproof.Core
import Data.Fin

%default total

--------------------------------------------------------------------------------
-- Bounded Natural Numbers using Fin
--------------------------------------------------------------------------------

||| A natural number strictly less than n
||| This uses the standard library's Fin type which provides
||| compile-time guarantees about bounds.
public export
BoundedNat : Nat -> Type
BoundedNat n = Fin n

||| Create a bounded natural from a regular Nat, checking bounds
public export
toBoundedNat : (bound : Nat) -> (value : Nat) -> Maybe (Fin bound)
toBoundedNat bound value = natToFin value bound

||| Convert back to Nat
public export
fromBoundedNat : Fin n -> Nat
fromBoundedNat = finToNat

||| Safe increment that wraps at boundary
public export
incWrap : Fin (S n) -> Fin (S n)
incWrap {n = Z} FZ = FZ
incWrap {n = S _} FZ = FS FZ
incWrap {n = S n} (FS k) with (strengthen (FS k))
  incWrap {n = S n} (FS k) | Just stronger = FS (incWrap stronger)
  incWrap {n = S n} (FS k) | Nothing = FZ

||| Safe decrement that wraps at zero
public export
decWrap : Fin (S n) -> Fin (S n)
decWrap {n} FZ = last
decWrap (FS k) = weaken k

--------------------------------------------------------------------------------
-- Percentage Type (0-100)
--------------------------------------------------------------------------------

||| A percentage value guaranteed to be between 0 and 100
public export
record Percentage where
  constructor MkPercentage
  value : Nat
  {auto inBounds : LTE value 100}

||| Create a percentage from a Nat, returning Nothing if > 100
public export
toPercentage : Nat -> Maybe Percentage
toPercentage n with (isLTE n 100)
  toPercentage n | Yes prf = Just (MkPercentage n)
  toPercentage n | No _ = Nothing

||| Get the percentage value
public export
percentageValue : Percentage -> Nat
percentageValue = value

||| Apply percentage to a value: (p% of n)
public export
applyPercentage : Percentage -> Nat -> Nat
applyPercentage p n = div (value p * n) 100

||| Complement of a percentage (100 - p)
public export
complement : Percentage -> Percentage
complement p = MkPercentage (minus 100 (value p))

--------------------------------------------------------------------------------
-- Unit Interval (0.0 to 1.0 represented as fraction)
--------------------------------------------------------------------------------

||| A fraction representing a value in [0, 1]
||| Represented as numerator/denominator where numerator <= denominator
public export
record UnitInterval where
  constructor MkUnit
  numerator : Nat
  denominator : Nat
  {auto nonZeroDenom : IsSucc denominator}
  {auto inRange : LTE numerator denominator}

||| Create a unit interval from two naturals
public export
toUnitInterval : (num : Nat) -> (denom : Nat) -> Maybe UnitInterval
toUnitInterval _ Z = Nothing  -- Division by zero
toUnitInterval num (S denom) with (isLTE num (S denom))
  toUnitInterval num (S denom) | Yes prf = Just (MkUnit num (S denom))
  toUnitInterval num (S denom) | No _ = Nothing

||| Zero in unit interval
public export
unitZero : UnitInterval
unitZero = MkUnit 0 1

||| One in unit interval
public export
unitOne : UnitInterval
unitOne = MkUnit 1 1

||| Half in unit interval
public export
unitHalf : UnitInterval
unitHalf = MkUnit 1 2

||| Apply unit interval to a natural: floor(u * n)
public export
applyUnit : UnitInterval -> Nat -> Nat
applyUnit u n = div (numerator u * n) (denominator u)

--------------------------------------------------------------------------------
-- Saturating Arithmetic
--------------------------------------------------------------------------------

||| Saturating addition - caps at max instead of overflowing
public export
addSat : (max : Nat) -> Nat -> Nat -> Nat
addSat max a b =
  let sum = a + b
  in if sum > max then max else sum

||| Saturating subtraction - floors at 0 instead of underflowing
public export
subSat : Nat -> Nat -> Nat
subSat a b = if b >= a then 0 else minus a b

||| Saturating multiplication
public export
mulSat : (max : Nat) -> Nat -> Nat -> Nat
mulSat max a b =
  let prod = a * b
  in if prod > max then max else prod

--------------------------------------------------------------------------------
-- Wrapping Arithmetic
--------------------------------------------------------------------------------

||| Wrapping addition - wraps around at modulus
public export
addWrap : (modulus : Nat) -> Nat -> Nat -> Nat
addWrap Z _ _ = 0
addWrap (S m) a b = mod (a + b) (S m)

||| Wrapping subtraction
public export
subWrap : (modulus : Nat) -> Nat -> Nat -> Nat
subWrap Z _ _ = 0
subWrap (S m) a b =
  if b > a
    then minus (S m) (mod (minus b a) (S m))
    else mod (minus a b) (S m)

||| Wrapping multiplication
public export
mulWrap : (modulus : Nat) -> Nat -> Nat -> Nat
mulWrap Z _ _ = 0
mulWrap (S m) a b = mod (a * b) (S m)
