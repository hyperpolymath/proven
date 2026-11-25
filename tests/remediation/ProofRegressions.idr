-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module ProofRegressions

import Proven.ECHIDNA.Soundness
import Proven.SafePassword.Hash
import Proven.SafeWebhook
import Proven.FFI.SafeWebhook

%default total

-- These aliases force the generic statements to remain part of the test gate.
checkerSound : (p, g : ProofTerm) -> validateProof p g = IsValid p -> GoalHolds g
checkerSound = echidnaSoundness

falseEqualityImpossible : (p : ProofTerm) ->
  validateProof p (Eq Zero (Succ Zero)) = IsValid p -> Void
falseEqualityImpossible = rejectZeroEqualsOne

-- Positive semantic witnesses prevent "reject everything" from being a fix.
zeroHolds : GoalHolds (Eq Zero Zero)
zeroHolds = echidnaSoundness ProofTerm.Refl (Eq Zero Zero) Refl

trueHolds : GoalHolds (Eq ProofTerm.True ProofTerm.True)
trueHolds = echidnaSoundness ProofTerm.Refl (Eq ProofTerm.True ProofTerm.True) Refl

sameCharacters : (a, b : String) -> constantTimeStringCompare a b = Prelude.True ->
                 unpack a = unpack b
sameCharacters = stringCompareSound

sameStringAccepted : (s : String) -> constantTimeStringCompare s s = Prelude.True
sameStringAccepted = stringCompareReflexive

noWebhookAccepted : (key : String) -> (event : WebhookEvent) ->
  (now : Nat) -> (tracker : IdempotencyTracker) ->
  fst (verifyWebhook key event now tracker) = Verified -> Void
noWebhookAccepted = webhookNeverVerified

trackerUnchanged : (key : String) -> (event : WebhookEvent) ->
  (now : Nat) -> (tracker : IdempotencyTracker) ->
  snd (verifyWebhook key event now tracker) = tracker
trackerUnchanged = webhookPreservesTracker

exportRejectsEveryHeader : (secret, payload, header : String) ->
  proven_idris_webhook_verify_signature secret payload header = 0
exportRejectsEveryHeader = webhookExportRejects
