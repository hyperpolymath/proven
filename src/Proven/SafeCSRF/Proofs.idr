-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeCSRF operations
|||
||| Verifies properties of constant-time comparison, token validation,
||| and double-submit cookie pattern correctness.
|||
||| Six declarations in this file are stated as OWED-with-justification
||| per the convention set 2026-05-20 by SafeChecksum/SafeArgs/SafeCSV:
||| triple-pipe `||| OWED:` doc-block + `0 ` erased-multiplicity + bare
||| signature, no `postulate` keyword, no `believe_me`. Each entry
||| names the specific Idris2 0.8.0 blocker (chiefly `unpack`/Char-FFI
||| primitive equality opacity and Bool-Prop reflection gaps over
||| `String.length` / `constantTimeEqual`) and a concrete discharge
||| path. The blocker family is the same as the SafeChecksum Luhn /
||| ISBN String-FFI OWED set and the gossamer `stringNotEqCommut`
||| class-J axiom.
module Proven.SafeCSRF.Proofs

import Proven.SafeCSRF
import Data.String
import Data.List

%default total

--------------------------------------------------------------------------------
-- Constant-Time Comparison Properties
--------------------------------------------------------------------------------

||| OWED: `constantTimeEqual s s = True` for every `s : String`.
||| Operationally true because `constantTimeEqual` first compares
||| `length (unpack s) /= length (unpack s)` (always `False`), then
||| folds `acc && (c == c)` over the zipped character lists, and
||| `c == c = True` for every `Char c`.
|||
||| Held back by Idris2 0.8.0 not type-level reducing `String.unpack`
||| (an opaque C FFI primitive) and not exposing a reflective
||| `prim__eq_Char x x = True` lemma. The `where`-local `go` fold
||| further compounds the issue: induction on the abstract
||| `unpack s` is unavailable without `unpack` reducing, and even if
||| we had a concrete cons-list the inductive step requires
||| `acc && True = acc` (`andTrueNeutral` — not exposed as a Refl
||| identity in Idris2 0.8.0's stdlib). Same blocker family as
||| `SafeChecksum.luhnValidatesKnownGood` (String FFI) and the
||| gossamer `stringNotEqCommut` class-J axiom.
|||
||| Discharge once: (a) a `Data.String` reflective tactic exposes
||| `unpack`/`pack` reductively, (b) `Data.Bool` ships
||| `andTrueNeutral` as a Refl-reducible identity (or it is added
||| locally as a helper lemma over `List Char`), and (c) a
||| `prim__eq_Char` reflective lemma is available. Alternative:
||| refactor `constantTimeEqual` to operate on a `Vect n Char`
||| directly so the length-equality and per-character comparison are
||| both type-level structural.
public export
0 constantTimeEqRefl : (s : String) -> constantTimeEqual s s = True

||| OWED: `constantTimeEqual a b = constantTimeEqual b a` for every
||| `a, b : String`. Operationally true because the length-inequality
||| guard `length as /= length bs` is symmetric in its two arguments
||| and the per-character fold reduces to `acc && (x == y)` which
||| commutes via `Char` primitive equality commutativity.
|||
||| Held back by Idris2 0.8.0 not exposing
||| `prim__eq_Char x y = prim__eq_Char y x` as a Refl-reducible lemma
||| — `Char` equality bottoms out in a runtime FFI primitive, exactly
||| the same blocker as the gossamer `stringNotEqCommut` class-J
||| axiom (which had to introduce `%unsafe`+`believe_me ()` over
||| `prim__eq_String` to land). The `/=` length check is also routed
||| through opaque `String.length`/`unpack` so the upstream guard
||| does not reduce either.
|||
||| Discharge once a `Data.Char` reflective `eqCharSym` lemma is
||| available — symmetric to the `Boj.SafetyLemmas.charEqSym` class-J
||| axiom already shipped in boj-server. Until then, recording this
||| as a stated OWED (parallel to the `SafeChecksum` String-FFI set)
||| keeps the obligation discoverable rather than silent. Specifically
||| awaits a `Data.Char.eqCharSym` reflective lemma symmetric to
||| `Boj.SafetyLemmas.charEqSym`.
public export
0 constantTimeEqSym : (a, b : String) -> constantTimeEqual a b = constantTimeEqual b a

||| OWED: if `Not (length a = length b)` then
||| `constantTimeEqual a b = False`. Operationally true because the
||| `constantTimeEqual` definition first computes
||| `length as /= length bs` (with `as = unpack a`, `bs = unpack b`)
||| and returns `False` immediately when this `Bool` is `True`.
|||
||| Held back by a Bool-Prop reflection gap: the hypothesis is the
||| *propositional* inequality `Not (length a = length b)` over
||| `String.length`, but `constantTimeEqual` branches on the
||| *Boolean* `length as /= length bs` over `List Char` (i.e. after
||| `unpack`). To bridge the two we need both (a) a lemma
||| `length s = length (unpack s)` (not exposed as Refl in Idris2
||| 0.8.0 because `unpack` is an opaque FFI primitive) and (b) a
||| `Bool ↔ Prop` reflection from `Not (m = n)` to `(m /= n) = True`
||| on `Nat` (not in the stdlib at this version). Same family as
||| `SafeArgs.boolParsingComplete`.
|||
||| Discharge once `Data.String` exposes `lengthUnpackEq` and
||| `Data.Nat` (or `Decidable.Equality`) ships the
||| `decEqToNeq` Bool reflection lemma, or once `constantTimeEqual`
||| is refactored to take a `LengthEq`-tagged input (push the
||| length equality into the type and remove the runtime guard).
public export
0 differentLengthUnequal : (a, b : String) ->
                           Not (length a = length b) ->
                           constantTimeEqual a b = False

--------------------------------------------------------------------------------
-- Token Validation Properties
--------------------------------------------------------------------------------

||| OWED: `validateToken tok (tokenString tok) = True` for every
||| `tok : CSRFToken`. By unfolding,
||| `validateToken tok (tokenString tok)
||| = constantTimeEqual (tokenString tok) (tokenString tok)`, which
||| is the `constantTimeEqRefl` instance at `s = tokenString tok`.
|||
||| Held back by the same blocker as `constantTimeEqRefl` above —
||| once that OWED lemma is discharged this proof becomes a one-line
||| `constantTimeEqRefl (tokenString tok)`. Recorded as a separate
||| OWED entry rather than collapsing into `constantTimeEqRefl` so
||| that callers reading `validateToken`-shaped obligations can find
||| the named identity directly. Same blocker family as
||| `SafeChecksum.verifyCRC32Definition` would be without
||| `verifyCRC32` reducing definitionally.
|||
||| Discharge once `constantTimeEqRefl` is discharged (the chain is
||| `constantTimeEqRefl → tokenValidatesSelf`); the proof body is
||| then `constantTimeEqRefl (tokenString tok)`.
public export
0 tokenValidatesSelf : (tok : CSRFToken) -> validateToken tok (tokenString tok) = True

||| OWED: `validateDoubleSubmit (MkDoubleSubmit val val) = True` for
||| every `val : String`. By unfolding,
||| `validateDoubleSubmit (MkDoubleSubmit val val)
||| = constantTimeEqual val val`, which is the `constantTimeEqRefl`
||| instance at `s = val`.
|||
||| Held back by the same blocker as `constantTimeEqRefl` above —
||| once that OWED lemma is discharged this proof becomes a one-line
||| `constantTimeEqRefl val`. Recorded as a separate OWED entry so
||| the double-submit-cookie pattern's reflexivity claim is
||| discoverable as a named obligation in the safety surface.
|||
||| Discharge once `constantTimeEqRefl` is discharged; the proof
||| body is then `constantTimeEqRefl val`.
public export
0 identicalDoubleSubmitValid : (val : String) ->
                               validateDoubleSubmit (MkDoubleSubmit val val) = True

||| OWED: if `validateToken tok submitted = False` then
||| `fullValidation tok submitted origins origin = False`.
||| Operationally true because
||| `fullValidation tok submitted origins origin
||| = validateToken tok submitted && validateOrigin origins origin`,
||| and `False && _ = False` by the `Bool` `&&` definition.
|||
||| Although the `&&` `False`-short-circuit is in principle a Refl
||| identity (`False && b = False`), discharging this lemma still
||| requires `validateToken` to be reduced to its `constantTimeEqual`
||| form so that `rewrite prf` can fire — which in turn requires
||| `String.unpack` / `List.length` / `Char` primitive equality to
||| reduce at the type level, all of which are FFI-opaque in
||| Idris2 0.8.0. The sibling `wrongSessionFails` (already proved in
||| this file by `rewrite prf in Refl`) discharges the symmetric
||| pattern only because its `prf` is stated directly over
||| `constantTimeEqual` rather than over the wrapped `validateToken`
||| — for `fullValidationRequiresToken` the same trick is blocked by
||| `validateToken`'s definitional opacity at the level of `tok`'s
||| record-field accessor `tokenString`.
|||
||| Discharge once either (a) `validateToken` is refactored to
||| operate directly on `String` (rather than via the `tokenString`
||| accessor through the `CSRFToken` record), at which point
||| `rewrite prf in Refl` would close it (cf. `wrongSessionFails`),
||| or (b) a Bool-Prop reflective tactic exposes `validateToken`'s
||| reduction. Recorded as OWED until one of those lands.
public export
0 fullValidationRequiresToken : (tok : CSRFToken) -> (submitted : String) ->
                                (origins : List String) -> (origin : String) ->
                                validateToken tok submitted = False ->
                                fullValidation tok submitted origins origin = False

--------------------------------------------------------------------------------
-- Token Construction Properties
--------------------------------------------------------------------------------

||| mkToken rejects empty strings (empty < 32 = MinTokenBytes * 2).
public export
mkTokenRejectsEmpty : mkToken "" = Nothing
mkTokenRejectsEmpty = Refl

||| Hex tokens have only hex characters.
public export
hexTokenEmpty : isHexToken "" = True
hexTokenEmpty = Refl

--------------------------------------------------------------------------------
-- Session Token Properties
--------------------------------------------------------------------------------

||| Session validation fails if session ID does not match.
||| Since constantTimeEqual of different session IDs returns False,
||| the && short-circuits.
export
wrongSessionFails : (session : SessionCSRF) ->
                    (wrongId : String) -> (submittedToken : String) ->
                    constantTimeEqual session.sessionId wrongId = False ->
                    validateSessionToken session wrongId submittedToken = False
wrongSessionFails session wrongId submittedToken prf =
  rewrite prf in Refl
