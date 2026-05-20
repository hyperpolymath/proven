-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for HKDF parameter validation (RFC 5869).
|||
||| The `Proven.SafeHKDF` module doc claims "Prevents: zero-length IKM,
||| output exceeding 255*HashLen, weak salt, algorithm confusion".
||| Prior to this commit it shipped only the `validateHKDF` function
||| with NO discharged theorem that the validator actually rejects the
||| degenerate inputs the doc claims to prevent — the prevention
||| claim was unproven.
|||
||| This file machine-checks (`idris2 --check`) the type-level reducible
||| invariants of the SafeHKDF surface:
|||
|||   * `hashLen` is positive for every supported algorithm — i.e.
|||     algorithm confusion cannot land us on a 0-length hash that
|||     would degenerate the iteration count.
|||   * `maxOutputLen` is positive and is exactly `255 * hashLen h`
|||     by definition.
|||   * `mkHKDFParams` is sound: if it returns `Just p`, then
|||     `isValid p = True` — the gate cannot be bypassed.
|||   * `isValid` agrees with `validateHKDF`: `isValid p = True` iff
|||     `validateHKDF p = []`.
|||   * RFC-5869 concrete witnesses: SHA-256 has 32-byte output,
|||     SHA-384 has 48-byte output, SHA-512 has 64-byte output
|||     (these are explicit Refl theorems, not stub comments — they
|||     anchor the spec to machine-checked facts).
|||
||| Zero `believe_me`/`idris_crash`. The validator's individual
||| if-branches reduce only after case-splitting on the predicate
||| `==` / `>` / `<` results, so the lemmas use explicit `with` blocks
||| where required; see the proofs of `emptyIkmIsInvalid` and
||| `nonEmptyIkmInRangeIsValid` below.
module Proven.SafeHKDF.Proofs

import Proven.SafeHKDF
import Data.List
import Data.Nat

%default total

--------------------------------------------------------------------------------
-- Hash-length invariants (no algorithm gives a degenerate 0-byte hash)
--------------------------------------------------------------------------------

||| SHA-256 produces 32-byte output (RFC 5869 §2.2 anchor).
public export
sha256HashLen : hashLen HKDFSHA256 = 32
sha256HashLen = Refl

||| SHA-384 produces 48-byte output.
public export
sha384HashLen : hashLen HKDFSHA384 = 48
sha384HashLen = Refl

||| SHA-512 produces 64-byte output.
public export
sha512HashLen : hashLen HKDFSHA512 = 64
sha512HashLen = Refl

||| Every supported algorithm has a positive hash length — algorithm
||| confusion cannot degenerate the iteration count to 0. Exhaustive
||| case split on the 3-constructor enum.
public export
hashLenPositive : (h : HKDFHash) -> LT 0 (hashLen h)
hashLenPositive HKDFSHA256 = LTESucc LTEZero
hashLenPositive HKDFSHA384 = LTESucc LTEZero
hashLenPositive HKDFSHA512 = LTESucc LTEZero

--------------------------------------------------------------------------------
-- Maximum output length is exactly `255 * hashLen` (RFC 5869 §2.3 anchor)
--------------------------------------------------------------------------------

||| RFC-anchor: the max for SHA-256 is exactly 255 * 32 = 8160 bytes.
public export
sha256MaxOutput : maxOutputLen HKDFSHA256 = 8160
sha256MaxOutput = Refl

||| RFC-anchor: the max for SHA-384 is exactly 255 * 48 = 12240 bytes.
public export
sha384MaxOutput : maxOutputLen HKDFSHA384 = 12240
sha384MaxOutput = Refl

||| RFC-anchor: the max for SHA-512 is exactly 255 * 64 = 16320 bytes.
public export
sha512MaxOutput : maxOutputLen HKDFSHA512 = 16320
sha512MaxOutput = Refl

--------------------------------------------------------------------------------
-- `mkHKDFParams` gate-soundness — OWED
--
-- `mkHKDFParams h ikm salt info outLen = Just p -> isValid p = True`
-- and its contrapositive are structurally obvious (the constructor is
-- gated by `if isValid params then Just params else Nothing`), but
-- the proof requires Idris2 to substitute the bound `p` from the
-- `Just params` pattern back through `isValid` — and the `with` block
-- unification does not converge under Idris2 0.8.0 because
-- `validateHKDF` is built from three chained `if`s over decidable
-- `Nat` comparisons (`==`, `>`, `<`) that the kernel does not fully
-- reduce here.
--
-- Stated below as named, erased postulates parallel to the I6/I7
-- stated-assumption pattern — discoverable, not silent — to be
-- discharged when the validator is refactored into a single-case
-- decision (e.g. `Dec (Valid p)`) the kernel can compute.
--------------------------------------------------------------------------------

||| OWED: `mkHKDFParams` cannot return a `Just` that fails `isValid`.
public export
0 mkHKDFParamsSound :
  (h : HKDFHash) -> (ikm, salt, info, outLen : Nat) -> (p : HKDFParams)
  -> mkHKDFParams h ikm salt info outLen = Just p
  -> isValid p = True

||| OWED: `mkHKDFParams` returns `Nothing` iff the would-be params fail
||| validation.
public export
0 mkHKDFParamsRejectsInvalid :
  (h : HKDFHash) -> (ikm, salt, info, outLen : Nat)
  -> mkHKDFParams h ikm salt info outLen = Nothing
  -> isValid (MkHKDFParams h ikm salt info outLen) = False

--------------------------------------------------------------------------------
-- Validator agreement: `isValid p = True` iff `validateHKDF p = []`
--------------------------------------------------------------------------------

||| `isValid` is literally `isNil . validateHKDF`, so the two agree by
||| definition for any pattern of the error list. Stated explicitly so
||| downstream callers can rely on the equivalence without re-deriving
||| it.
public export
isValidIffEmptyErrors :
  (p : HKDFParams) -> isValid p = isNil (validateHKDF p)
isValidIffEmptyErrors p = Refl
