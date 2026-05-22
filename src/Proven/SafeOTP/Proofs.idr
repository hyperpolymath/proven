-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeOTP operations
|||
||| Verifies properties of OTP token validation: constant-time comparison
||| reflexivity, divisor correctness, and time counter computation safety.
module Proven.SafeOTP.Proofs

import Proven.SafeOTP
import Data.Nat
import Data.String
import Data.List

%default total

--------------------------------------------------------------------------------
-- Digit Divisor Properties
--------------------------------------------------------------------------------

||| 6-digit divisor is 10^6.
public export
digits6Divisor : digitsDivisor Digits6 = 1000000
digits6Divisor = Refl

||| 8-digit divisor is 10^8.
public export
digits8Divisor : digitsDivisor Digits8 = 100000000
digits8Divisor = Refl

--------------------------------------------------------------------------------
-- Constant-Time Comparison Properties
--------------------------------------------------------------------------------

||| OWED: `constantTimeCompare` is reflexive — `constantTimeCompare s s
||| = True` for every `String s`. Witnessed operationally by the
||| implementation: the `length as /= length bs` guard collapses to
||| `False`, then the inner `go` helper folds `(acc && x == y)` over
||| the unpacked list with `x == y` true at every position.
|||
||| Held back by Idris2 0.8.0 not reducing two stacked String/Char
||| primitives at the type level: (1) `unpack` is FFI-bound, so
||| `unpack s = unpack s` does not type-level normalise for abstract
||| `s` (same blocker family as SafeChecksum's String-FFI OWED set);
||| (2) `(==) : Char -> Char -> Bool` is a primitive whose `c == c =
||| True` lemma is not exposed as a definitional Refl in Idris2 0.8.0
||| (same blocker family as `Boj.SafetyLemmas.charEqSym` in boj-server,
||| where it is discharged as a class-(J) axiom `%unsafe believe_me`).
||| Discharge once a `Data.String` / `Data.Char` reflective tactic is
||| available, or by introducing a class-(J) `charEqRefl` axiom and a
||| per-list induction lemma over `go`.
export
0 constantTimeCompareRefl : (s : String) -> constantTimeCompare s s = True

||| OWED: `constantTimeCompare` is symmetric —
||| `constantTimeCompare a b = constantTimeCompare b a` for all
||| Strings. Witnessed operationally by the implementation: both the
||| `length as /= length bs` guard and the per-position `x == y`
||| comparison in `go` are themselves symmetric (length equality is
||| symmetric; `Char` equality is symmetric).
|||
||| Held back by Idris2 0.8.0 not reducing `unpack` at the type level
||| (FFI-bound primitive), and by `prim__eq_Char` not exposing a
||| `charEqSym : (x, y : Char) -> (x == y) = (y == x)` Refl lemma.
||| Same blocker family as `constantTimeCompareRefl`. Discharge once a
||| `Data.String` reflective tactic is available, or via a class-(J)
||| `charEqSym` axiom paired with a per-list induction over `go`.
export
0 constantTimeCompareSym : (a, b : String) -> constantTimeCompare a b = constantTimeCompare b a

||| Empty strings compare equal.
public export
emptyStringsEqual : constantTimeCompare "" "" = True
emptyStringsEqual = Refl

--------------------------------------------------------------------------------
-- TOTP Validation Properties
--------------------------------------------------------------------------------

||| OWED: an OTP code validates against a singleton list containing
||| itself — `validateTOTPCode code [code] = True`. By definitional
||| unfolding, `validateTOTPCode code [code]` reduces to
||| `any (\e => constantTimeCompare code.code e.code) [code]` and
||| then to `constantTimeCompare code.code code.code || False`, which
||| is `True` iff `constantTimeCompare` is reflexive.
|||
||| Held back by the same blockers as `constantTimeCompareRefl`
||| (String/Char primitive opacity in Idris2 0.8.0): this claim is a
||| direct downstream corollary and cannot be discharged until
||| `constantTimeCompareRefl` is. Discharge follows immediately once
||| `constantTimeCompareRefl` is in scope, by `rewrite` on the head of
||| the `||`.
export
0 codeValidatesAgainstSelf : (code : OTPCode) -> validateTOTPCode code [code] = True

||| OWED: TOTP validation accepts a code if it matches anywhere in
||| the candidate list. By definition `validateTOTPCode code codes =
||| any (\e => constantTimeCompare code.code e.code) codes`, so this
||| is literally the hypothesis — a statement of the function's
||| extensional equality with its body.
|||
||| Held back by Idris2 0.8.0 not eta-reducing the lambda inside `any`
||| at the type level for an abstract `codes : List OTPCode` —
||| `any p xs = True` does not normalise to its body without case
||| analysis on `xs`. Compounded by the same String-FFI opacity in
||| `constantTimeCompare` underneath. Discharge by induction on
||| `codes` (a one-line `Refl` for the `[]` impossibility plus a
||| `rewrite` step for `_ :: _`), once a `Data.List.any` extensionality
||| lemma is in `contrib` — or inline the induction here.
export
0 codeInListValidates : (code : OTPCode) -> (codes : List OTPCode) ->
                        any (\e => constantTimeCompare code.code e.code) codes = True ->
                        validateTOTPCode code codes = True

||| OWED: HOTP validation of identical codes succeeds —
||| `validateHOTPCode code code = True`. By definition
||| `validateHOTPCode submitted expected = constantTimeCompare
||| submitted.code expected.code`, so identical inputs reduce to
||| `constantTimeCompare code.code code.code`, which is `True` by
||| `constantTimeCompareRefl`.
|||
||| Held back by the same blockers as `constantTimeCompareRefl`
||| (String/Char primitive opacity in Idris2 0.8.0). Discharge
||| follows immediately once `constantTimeCompareRefl` is in scope.
export
0 identicalHOTPValid : (code : OTPCode) -> validateHOTPCode code code = True

||| Validation against empty list always fails.
public export
validateEmptyListFails : (code : OTPCode) -> validateTOTPCode code [] = False
validateEmptyListFails _ = Refl

--------------------------------------------------------------------------------
-- Time Counter Properties
--------------------------------------------------------------------------------

||| Time counter with zero period returns 0 (division by zero guard).
public export
timeCounterZeroPeriod : (t : Integer) -> timeCounter t 0 = 0
timeCounterZeroPeriod _ = Refl

||| Default TOTP period is 30 seconds.
public export
defaultPeriodIs30 : defaultTOTP.period = 30
defaultPeriodIs30 = Refl

||| Default TOTP skew is 1 step.
public export
defaultSkewIs1 : defaultTOTP.skew = 1
defaultSkewIs1 = Refl

||| Recommended TOTP uses SHA256 (stronger than default SHA1).
public export
recommendedUsesSHA256 : recommendedTOTP.algorithm = HMACSHA256
recommendedUsesSHA256 = Refl

--------------------------------------------------------------------------------
-- Secret Construction Properties
--------------------------------------------------------------------------------

||| Empty byte list is rejected as a secret (too short).
public export
emptySecretRejected : mkSecret [] = Nothing
emptySecretRejected = Refl

||| Minimum secret length is 16 bytes (128 bits per RFC 4226).
public export
minSecretIs16 : MinSecretBytes = 16
minSecretIs16 = Refl
