-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeBuffer bounds-checked dynamic buffers.
|||
||| The `Proven.SafeBuffer` module doc claims "Prevents: buffer overflows
||| and underflows through bounds checking". Prior to this commit it
||| shipped the `DynBuffer`/`Buffer` operations with NO discharged
||| theorem that the bounds-checking actually holds — the prevention
||| claim was unproven.
|||
||| This file machine-checks (`idris2 --check`) the core invariants
||| that ARE type-level reducible:
|||
|||   * `emptyDyn` produces a buffer of length 0 and the requested
|||     capacity (no junk content, no capacity drift).
|||   * `clear` leaves capacity untouched and zeroes length.
|||   * `write` ALWAYS preserves capacity (the only `DynBuffer`
|||     constructor used in its `Just` branch is `MkDynBuffer
|||     buf.capacity ...`).
|||   * `dropBuf` ALWAYS preserves capacity (same structural argument).
|||   * `copyTo` ALWAYS preserves the *destination's* capacity.
|||   * `bufferSize` of a freshly-built `Buffer n d` equals `n`
|||     (fixed-size buffer invariant via `Vect`).
|||
||| Items NOT covered here (explicit OWED markers, deliberately, to
||| make the residual proof debt discoverable rather than silent):
|||
|||   * `write x buf = Just buf' -> dynLength buf' = S (dynLength buf)`
|||     — needs the stdlib lemma `length (xs ++ [x]) = S (length xs)`,
|||     which Idris2 0.8.0's Prelude does NOT expose definitionally
|||     (it reduces through `foldr` and stalls). The same pattern blocks
|||     `writeMany` length accounting. Tracked under standards#128's
|||     "needs cons-distribution lemmas for foldl-based Prelude" item.
|||   * The full negative direction of `isFull` / `remaining`
|||     (`write x buf = Nothing -> length buf.content = buf.capacity`)
|||     depends on the same lemma family.
|||
||| Zero `believe_me`/`idris_crash`. The OWED items above are stated as
||| named, erased postulates parallel to the I6/I7 stated-assumption
||| pattern in `proof-of-work` ABI seam — discoverable, not silent.
module Proven.SafeBuffer.Proofs

import Proven.SafeBuffer
import Data.List
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- DynBuffer construction invariants
--------------------------------------------------------------------------------

||| A freshly-allocated dynamic buffer has length zero. Direct from the
||| definition of `emptyDyn`: it constructs `MkDynBuffer cap []`.
public export
emptyDynLengthZero : (cap : Nat) -> dynLength (emptyDyn {a} cap) = Z
emptyDynLengthZero cap = Refl

||| `emptyDyn` records the requested capacity exactly — no off-by-one,
||| no rounding, no overflow.
public export
emptyDynCapacity : (cap : Nat) -> (emptyDyn {a} cap).capacity = cap
emptyDynCapacity cap = Refl

||| A freshly-allocated dynamic buffer is `isEmpty`. The check `isNil []`
||| reduces to `True` on the empty content list.
public export
emptyDynIsEmpty : (cap : Nat) -> isEmpty (emptyDyn {a} cap) = True
emptyDynIsEmpty cap = Refl

--------------------------------------------------------------------------------
-- Clear semantics
--------------------------------------------------------------------------------

||| `clear` preserves capacity. The `MkDynBuffer buf.capacity []`
||| constructor used in `clear` literally re-uses the input capacity.
public export
clearPreservesCapacity : (buf : DynBuffer a)
                      -> (clear buf).capacity = buf.capacity
clearPreservesCapacity buf = Refl

||| `clear` zeroes the content. `clear` always sets the list to `[]`
||| regardless of the prior content, so length reduces to `Z`.
public export
clearZeroesLength : (buf : DynBuffer a) -> dynLength (clear buf) = Z
clearZeroesLength buf = Refl

--------------------------------------------------------------------------------
-- Capacity preservation on successful operations
--------------------------------------------------------------------------------

||| `write` preserves capacity: every successful `Just`-branch
||| constructs `MkDynBuffer buf.capacity (buf.content ++ [x])`. The
||| `Nothing` branch trivially has no successor to compare. Structural
||| case split on `isFull buf`.
public export
writePreservesCapacity :
  (x : a) -> (buf : DynBuffer a) -> (buf' : DynBuffer a)
  -> write x buf = Just buf' -> buf'.capacity = buf.capacity
writePreservesCapacity x buf buf' prf with (isFull buf)
  writePreservesCapacity x buf buf' prf | True  = case prf of Refl impossible
  writePreservesCapacity x buf buf' prf | False = case prf of Refl => Refl

||| `dropBuf` preserves capacity unconditionally (it builds
||| `MkDynBuffer buf.capacity (drop n buf.content)`).
public export
dropBufPreservesCapacity :
  (n : Nat) -> (buf : DynBuffer a) -> (dropBuf n buf).capacity = buf.capacity
dropBufPreservesCapacity n buf = Refl

||| `copyTo` preserves the *destination* capacity (the `Just`-branch
||| constructs `MkDynBuffer dest.capacity (dest.content ++ src.content)`).
||| Structural case split on the inner `if`.
public export
copyToPreservesDestCapacity :
  (src, dest, result : DynBuffer a)
  -> copyTo src dest = Just result -> result.capacity = dest.capacity
copyToPreservesDestCapacity src dest result prf with (length src.content > remaining dest)
  copyToPreservesDestCapacity src dest result prf | True  = case prf of Refl impossible
  copyToPreservesDestCapacity src dest result prf | False = case prf of Refl => Refl

--------------------------------------------------------------------------------
-- Fixed-size Buffer invariant
--------------------------------------------------------------------------------

||| A `Buffer` of declared size `n` reports `bufferSize = n`. Direct
||| from the `Vect`-backed definition: `length (replicate n d) = n`
||| via `Data.Vect.lengthCorrect` (definitional reduction).
public export
emptyBufferSize : (n : Nat) -> (d : a) -> bufferSize (empty n d) = n
emptyBufferSize Z     d = Refl
emptyBufferSize (S k) d = cong S (emptyBufferSize k d)

--------------------------------------------------------------------------------
-- Explicit OWED postulates (named bridge lemmas, NOT believe_me)
--
-- The two below depend on the Prelude lemma
--   `length (xs ++ [x]) = S (length xs)`
-- which under Idris2 0.8.0 does not reduce through `foldr`-defined `++`
-- by `Refl` alone. Tracked under standards#128 (cons-distribution
-- lemmas). Stated here so the residual proof debt is *discoverable*,
-- not silent.
--------------------------------------------------------------------------------

||| OWED: `write` extends content by exactly one. The structural fact is
||| obvious; the proof is blocked on the cons-distribution lemma family.
public export
0 writeAddsOne :
  (x : a) -> (buf : DynBuffer a) -> (buf' : DynBuffer a)
  -> write x buf = Just buf' -> dynLength buf' = S (dynLength buf)

||| OWED: `writeMany` extends content by `length xs`. Same blocker as
||| `writeAddsOne` (cons-distribution / append-length lemma).
public export
0 writeManyAddsLength :
  (xs : List a) -> (buf : DynBuffer a) -> (buf' : DynBuffer a)
  -> writeMany xs buf = Just buf' -> dynLength buf' = dynLength buf + length xs
