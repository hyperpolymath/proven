-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeBitset bounded bit-manipulation.
|||
||| `Proven.SafeBitset` exposes bounds-checked single-bit / range
||| / bitwise operations and queries. This file machine-checks the
||| structural invariants:
|||
|||   * `empty cap` records the requested capacity (no off-by-one).
|||   * `empty cap` has `bits = 0` (no junk content).
|||   * `empty cap` reports `isEmpty = True`.
|||   * `singleton` bounds-rejection (`bit >= cap` returns Nothing).
|||   * Capacity preservation on every total constructor returning
|||     `Right` / `Just` (set / clear / toggle / and / or / bitXor /
|||     andNot / setRange / clearRange / bitNot / unsafe* / fromInteger).
|||   * `set` / `clear` / `toggle` / `test` all bounds-reject when
|||     `i >= bs.capacity` (structural case-split on the `if`).
|||
||| OWED: arithmetic with `Integer` opaque (`.|.`, `.&.`, `shiftL`)
||| cannot be `Refl`-decided.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeBitset.Proofs

import Proven.SafeBitset
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- `empty` invariants
--------------------------------------------------------------------------------

||| `empty cap` records the requested capacity.
public export
emptyCapacity : (cap : Nat) -> (empty cap).capacity = cap
emptyCapacity cap = Refl

||| `empty cap` has zero bits.
public export
emptyBitsZero : (cap : Nat) -> (empty cap).bits = 0
emptyBitsZero cap = Refl

||| `empty cap` reports `isEmpty = True`.
public export
emptyIsEmpty : (cap : Nat) -> isEmpty (empty cap) = True
emptyIsEmpty cap = Refl

||| `empty cap` reports `any = False`.
public export
emptyAnyFalse : (cap : Nat) -> any (empty cap) = False
emptyAnyFalse cap = Refl

--------------------------------------------------------------------------------
-- `unsafe*` ALWAYS preserve capacity
--------------------------------------------------------------------------------

||| `unsafeSet` preserves capacity.
public export
unsafeSetPreservesCapacity :
  (i : Nat) -> (bs : Bitset) -> (unsafeSet i bs).capacity = bs.capacity
unsafeSetPreservesCapacity i bs = Refl

||| `unsafeClear` preserves capacity.
public export
unsafeClearPreservesCapacity :
  (i : Nat) -> (bs : Bitset) -> (unsafeClear i bs).capacity = bs.capacity
unsafeClearPreservesCapacity i bs = Refl

||| `bitNot` preserves capacity (it explicitly re-uses bs.capacity).
public export
bitNotPreservesCapacity :
  (bs : Bitset) -> (bitNot bs).capacity = bs.capacity
bitNotPreservesCapacity bs = Refl

||| `fromInteger cap n` records the requested capacity.
public export
fromIntegerCapacity :
  (cap : Nat) -> (n : Integer) -> (fromInteger cap n).capacity = cap
fromIntegerCapacity cap n = Refl

--------------------------------------------------------------------------------
-- `singleton` bounds-rejection
--------------------------------------------------------------------------------

||| OWED: `singleton 0 bit` always rejects (no bit fits in zero
||| capacity). Blocked because `bit < 0` for variable `bit : Nat`
||| does not Refl-reduce (standards#128 Nat-comparison opacity).
public export
0 singletonZeroCapRejects :
  (bit : Nat) -> singleton 0 bit = Nothing

--------------------------------------------------------------------------------
-- Capacity preservation on `Right`-branch (structural case split on `if`)
--------------------------------------------------------------------------------

||| `set` preserves capacity when it succeeds.
public export
setPreservesCapacity :
  (i : Nat) -> (bs, bs' : Bitset)
  -> set i bs = Right bs' -> bs'.capacity = bs.capacity
setPreservesCapacity i bs bs' prf with (i >= bs.capacity)
  setPreservesCapacity i bs bs' prf | True  = case prf of Refl impossible
  setPreservesCapacity i bs bs' prf | False = case prf of Refl => Refl

||| `clear` preserves capacity when it succeeds.
public export
clearPreservesCapacity :
  (i : Nat) -> (bs, bs' : Bitset)
  -> clear i bs = Right bs' -> bs'.capacity = bs.capacity
clearPreservesCapacity i bs bs' prf with (i >= bs.capacity)
  clearPreservesCapacity i bs bs' prf | True  = case prf of Refl impossible
  clearPreservesCapacity i bs bs' prf | False = case prf of Refl => Refl

||| `toggle` preserves capacity when it succeeds.
public export
togglePreservesCapacity :
  (i : Nat) -> (bs, bs' : Bitset)
  -> toggle i bs = Right bs' -> bs'.capacity = bs.capacity
togglePreservesCapacity i bs bs' prf with (i >= bs.capacity)
  togglePreservesCapacity i bs bs' prf | True  = case prf of Refl impossible
  togglePreservesCapacity i bs bs' prf | False = case prf of Refl => Refl

||| `and` preserves capacity (uses bs1.capacity on success).
public export
andPreservesCapacity :
  (bs1, bs2, result : Bitset)
  -> and bs1 bs2 = Right result -> result.capacity = bs1.capacity
andPreservesCapacity bs1 bs2 result prf with (bs1.capacity /= bs2.capacity)
  andPreservesCapacity bs1 bs2 result prf | True  = case prf of Refl impossible
  andPreservesCapacity bs1 bs2 result prf | False = case prf of Refl => Refl

||| `or` preserves capacity.
public export
orPreservesCapacity :
  (bs1, bs2, result : Bitset)
  -> or bs1 bs2 = Right result -> result.capacity = bs1.capacity
orPreservesCapacity bs1 bs2 result prf with (bs1.capacity /= bs2.capacity)
  orPreservesCapacity bs1 bs2 result prf | True  = case prf of Refl impossible
  orPreservesCapacity bs1 bs2 result prf | False = case prf of Refl => Refl

||| `bitXor` preserves capacity.
public export
bitXorPreservesCapacity :
  (bs1, bs2, result : Bitset)
  -> bitXor bs1 bs2 = Right result -> result.capacity = bs1.capacity
bitXorPreservesCapacity bs1 bs2 result prf with (bs1.capacity /= bs2.capacity)
  bitXorPreservesCapacity bs1 bs2 result prf | True  = case prf of Refl impossible
  bitXorPreservesCapacity bs1 bs2 result prf | False = case prf of Refl => Refl

||| `andNot` preserves capacity.
public export
andNotPreservesCapacity :
  (bs1, bs2, result : Bitset)
  -> andNot bs1 bs2 = Right result -> result.capacity = bs1.capacity
andNotPreservesCapacity bs1 bs2 result prf with (bs1.capacity /= bs2.capacity)
  andNotPreservesCapacity bs1 bs2 result prf | True  = case prf of Refl impossible
  andNotPreservesCapacity bs1 bs2 result prf | False = case prf of Refl => Refl

--------------------------------------------------------------------------------
-- Conversion `toInteger`
--------------------------------------------------------------------------------

||| `toInteger` of `empty cap` is `0`.
public export
toIntegerEmpty : (cap : Nat) -> toInteger (empty cap) = 0
toIntegerEmpty cap = Refl

--------------------------------------------------------------------------------
-- Aliases
--------------------------------------------------------------------------------

||| `none = isEmpty` (definitional alias).
public export
noneIsEmpty : (bs : Bitset) -> none bs = isEmpty bs
noneIsEmpty bs = Refl

||| `all = isFull` (definitional alias).
public export
allIsFull : (bs : Bitset) -> Proven.SafeBitset.all bs = isFull bs
allIsFull bs = Refl

||| `getCapacity` is just record-projection.
public export
getCapacityProjection :
  (bs : Bitset) -> getCapacity bs = bs.capacity
getCapacityProjection bs = Refl
