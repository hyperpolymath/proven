-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module Proven.ECHIDNA.Soundness

import public Proven.ECHIDNA.Validator

%default total

||| Recover the certificate from an accepted result of the actual checker.
||| A caller-constructed IsValid value is insufficient: the equality premise
||| binds it to this execution, proof term, and goal.
public export
acceptedCertificate : (prf : ProofTerm) ->
                      (result : Maybe (Certificate prf goal)) ->
                      validationResult prf result = IsValid prf ->
                      Certificate prf goal
acceptedCertificate prf (Just certificate) accepted = certificate
acceptedCertificate prf Nothing Refl impossible

||| Soundness of the production checker for its closed Nat/Bool fragment.
||| No external prover, model prediction, OWED premise or consensus is trusted.
public export
echidnaSoundness : (prf, goal : ProofTerm) ->
                   validateProof prf goal = IsValid prf -> GoalHolds goal
echidnaSoundness prf goal accepted =
  certificateSound (acceptedCertificate prf (certify prf goal) accepted)

public export
comprehensiveSoundness : (prf, goal : ProofTerm) ->
                         validateComprehensive prf goal = IsValid prf -> GoalHolds goal
comprehensiveSoundness = echidnaSoundness

||| No proof term can make the checker accept the audited false equality.
public export
rejectZeroEqualsOne : (prf : ProofTerm) ->
                      validateProof prf (Eq Zero (Succ Zero)) = IsValid prf -> Void
rejectZeroEqualsOne prf accepted =
  zeroNotSuccessor (echidnaSoundness prf (Eq Zero (Succ Zero)) accepted)