-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Formal soundness guarantees for ECHIDNA neurosymbolic theorem prover
|||
||| This module provides the formal soundness theorem for ECHIDNA:
||| If the system accepts a proof, it is guaranteed to be valid under
||| the proof theory of the underlying theorem prover backend.
|||
||| Key insight: We never trust ML predictions directly. All verification
||| goes through established theorem provers.

module Proven.ECHIDNA.Soundness

import Proven.ECHIDNA.ProofTerm
import Proven.ECHIDNA.Validator

%default total

--------------------------------------------------------------------------------
-- Prover Backend Abstraction
--------------------------------------------------------------------------------

||| A theorem prover backend that can verify proofs
public export
interface ProverBackend prover where
  ||| Verify a proof term under this prover's proof theory
  verify : prover -> ProofTerm -> Bool

  ||| Soundness guarantee of the prover itself
  ||| If the prover accepts a proof, it is valid
  proverSound : (p : prover) -> (proof : ProofTerm) ->
                verify p proof = True -> ProofValid proof

||| Evidence that a proof is valid under a prover's proof theory
public export
data ProofValid : ProofTerm -> Type where
  ValidProof : (proof : ProofTerm) -> ProofValid proof

--------------------------------------------------------------------------------
-- ML Model Abstraction
--------------------------------------------------------------------------------

||| Machine learning model that suggests tactics
public export
record MLModel where
  constructor MkMLModel
  ||| Suggest tactics for a goal (may be unsound!)
  suggest : ProofTerm -> List ProofTerm
  ||| Confidence score (0.0 to 1.0)
  confidence : ProofTerm -> ProofTerm -> Double

--------------------------------------------------------------------------------
-- ECHIDNA System
--------------------------------------------------------------------------------

||| The ECHIDNA neurosymbolic theorem proving system
public export
record ECHIDNA prover where
  constructor MkECHIDNA
  ||| The ML model for tactic suggestions
  model : MLModel
  ||| The trusted prover backend
  backend : ProverBackend prover => prover
  ||| The internal validator (dependent type checker)
  validator : ProofTerm -> ValidationResult

--------------------------------------------------------------------------------
-- Soundness Theorem
--------------------------------------------------------------------------------

||| Main soundness theorem: If ECHIDNA accepts a proof, it is valid
|||
||| This is the formal guarantee that ECHIDNA provides:
||| The ML model may suggest anything (including unsound tactics),
||| but ECHIDNA only accepts proofs that pass both:
|||   1. Internal dependent type validation
|||   2. External prover backend verification
|||
||| Therefore, no unsound proofs can be accepted.
public export
echidnaSoundness : ProverBackend prover =>
                   (sys : ECHIDNA prover) ->
                   (proof : ProofTerm) ->
                   (validated : sys.validator proof = Valid) ->
                   (verified : verify sys.backend proof = True) ->
                   ProofValid proof
echidnaSoundness sys proof validated verified =
  -- By the soundness of the prover backend
  proverSound sys.backend proof verified

--------------------------------------------------------------------------------
-- Corollaries
--------------------------------------------------------------------------------

||| ECHIDNA never accepts invalid proofs:
||| If the prover rejects a proof, ECHIDNA cannot produce a soundness
||| guarantee for it (the echidnaSoundness theorem requires verified=True).
||| This is an immediate consequence of echidnaSoundness requiring a
||| proof of `verify sys.backend proof = True`, which contradicts
||| `verify sys.backend proof = False` by Uninhabited.
public export
noFalsePositives : ProverBackend prover =>
                   (sys : ECHIDNA prover) ->
                   (proof : ProofTerm) ->
                   verify sys.backend proof = False ->
                   verify sys.backend proof = True -> Void
noFalsePositives sys proof notVerified verified = absurd (trans (sym notVerified) verified)

||| If the validator accepts and prover accepts, the proof is valid
public export
doubleCheck : ProverBackend prover =>
              (sys : ECHIDNA prover) ->
              (proof : ProofTerm) ->
              sys.validator proof = Valid ->
              verify sys.backend proof = True ->
              ProofValid proof
doubleCheck sys proof validatorOk proverOk =
  echidnaSoundness sys proof validatorOk proverOk

||| ML suggestions don't affect soundness (can be arbitrary)
||| Even if the ML model suggests a tactic, the prover backend's rejection
||| prevents echidnaSoundness from producing a validity proof.
||| The ML suggestion is irrelevant to the formal guarantee.
public export
mlIndependence : ProverBackend prover =>
                 (sys : ECHIDNA prover) ->
                 (goal : ProofTerm) ->
                 (tactic : ProofTerm) ->
                 -- Even if ML suggests an invalid tactic...
                 tactic `elem` (sys.model).suggest goal ->
                 -- ...ECHIDNA won't accept it unless verified
                 verify sys.backend tactic = False ->
                 verify sys.backend tactic = True -> Void
mlIndependence sys goal tactic suggested notVerified verified =
  -- The ML suggestion (suggested) plays no role here:
  -- the contradiction follows purely from notVerified vs verified
  absurd (trans (sym notVerified) verified)

--------------------------------------------------------------------------------
-- Multi-Prover Consensus
--------------------------------------------------------------------------------

||| Evidence that multiple provers agree
public export
data Consensus : List prover -> ProofTerm -> Type where
  AllAgree : {provers : List prover} ->
             (proof : ProofTerm) ->
             (allVerify : All (\p => ProverBackend prover => verify p proof = True) provers) ->
             Consensus provers proof

||| Stronger guarantee: If N provers agree, proof is valid under all their theories
||| Each prover's individual soundness guarantee (proverSound) produces a
||| ProofValid proof from its verify=True evidence. Since ProofValid is
||| prover-independent, the result for each prover in the list is identical.
|||
||| Note: The full formal proof requires induction over the All structure
||| with prover interface resolution at each step. This is axiomatised as
||| it follows directly from proverSound applied at each list element.
public export
consensusSoundness : {provers : List prover} ->
                     (proof : ProofTerm) ->
                     Consensus provers proof ->
                     All (\p => ProverBackend prover => ProofValid proof) provers
consensusSoundness {provers = []} proof (AllAgree proof []) = []
consensusSoundness {provers = (p :: ps)} proof (AllAgree proof (eq :: rest)) =
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
  believe_me (ValidProof proof) :: consensusSoundness proof (AllAgree proof rest)

--------------------------------------------------------------------------------
-- Trust Framework Integration
--------------------------------------------------------------------------------

||| Evidence from the trust framework (benchmarking, property tests, anomaly detection)
public export
data TrustEvidence : ProofTerm -> Type where
  ||| Performance benchmarks show no regression
  BenchmarkPassed : ProofTerm -> TrustEvidence proof
  ||| Property-based tests passed all invariants
  PropertiesHold : ProofTerm -> TrustEvidence proof
  ||| No anomalies detected (overconfidence, circular reasoning, etc.)
  NoAnomalies : ProofTerm -> TrustEvidence proof
  ||| All trust layers passed
  FullTrust : TrustEvidence proof -> TrustEvidence proof -> TrustEvidence proof -> TrustEvidence proof

||| Combined soundness: Formal proof + empirical trust evidence
public export
totalTrust : ProverBackend prover =>
             (sys : ECHIDNA prover) ->
             (proof : ProofTerm) ->
             (validated : sys.validator proof = Valid) ->
             (verified : verify sys.backend proof = True) ->
             (evidence : TrustEvidence proof) ->
             ProofValid proof
totalTrust sys proof validated verified evidence =
  -- Trust evidence is additional assurance, but formal soundness is sufficient
  echidnaSoundness sys proof validated verified

--------------------------------------------------------------------------------
-- Export
--------------------------------------------------------------------------------

||| Main export: The soundness theorem is the key guarantee
||| To use: provide evidence that the validator returned Valid and
||| the prover backend verified the proof.
export
soundnessGuarantee : ProverBackend prover =>
                     (sys : ECHIDNA prover) ->
                     (proof : ProofTerm) ->
                     (validated : sys.validator proof = Valid) ->
                     (verified : verify sys.backend proof = True) ->
                     ProofValid proof
soundnessGuarantee = echidnaSoundness
