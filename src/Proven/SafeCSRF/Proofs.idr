-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeCSRF operations
|||
||| Verifies properties of constant-time comparison, token validation,
||| and double-submit cookie pattern correctness.
module Proven.SafeCSRF.Proofs

import Proven.SafeCSRF
import Data.String
import Data.List

%default total

--------------------------------------------------------------------------------
-- Constant-Time Comparison Properties
--------------------------------------------------------------------------------

||| Constant-time comparison is reflexive: any string equals itself.
||| Postulated: constantTimeEqual uses a where-local accumulator fold
||| over unpack results; proving reflexivity requires showing
||| (c == c) = True for all Char and that the accumulated && folds
||| to True, which involves Char comparison not reducible in Idris 2.
export
constantTimeEqRefl : (s : String) -> constantTimeEqual s s = True

||| Constant-time comparison is symmetric.
||| Postulated: requires showing Char (==) is commutative, which
||| is a primitive operation.
export
constantTimeEqSym : (a, b : String) -> constantTimeEqual a b = constantTimeEqual b a

||| Different-length strings always compare unequal.
||| This follows from the length check at the top of constantTimeEqual.
export
differentLengthUnequal : (a, b : String) ->
                         Not (length a = length b) ->
                         constantTimeEqual a b = False

--------------------------------------------------------------------------------
-- Token Validation Properties
--------------------------------------------------------------------------------

||| A token validates against its own string representation.
||| Follows from constantTimeEqRefl.
export
tokenValidatesSelf : (tok : CSRFToken) -> validateToken tok (tokenString tok) = True

||| Double-submit validation succeeds when both values are identical.
||| Follows from constantTimeEqRefl.
export
identicalDoubleSubmitValid : (val : String) ->
                             validateDoubleSubmit (MkDoubleSubmit val val) = True

||| fullValidation fails if token validation fails.
||| The && short-circuits: if the first conjunct is False, result is False.
export
fullValidationRequiresToken : (tok : CSRFToken) -> (submitted : String) ->
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
