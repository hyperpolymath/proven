-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeAttestation a2ml attestation envelopes.
|||
||| `Proven.SafeAttestation` already ships `parseAttestTypeRoundTrip`
||| and `parseHashAlgoRoundTrip` inline. This file makes the remaining
||| structural invariants machine-checkable:
|||
|||   * `AttestationType` enum self-equality (4 constructors).
|||   * `HashAlgorithm` enum self-equality (2 constructors).
|||   * `attestTypeToString` exhaustive on the 4-constructor enum
|||     (every constructor maps to its wire-format string).
|||   * `hashAlgoPrefix` exhaustive on the 2-constructor enum.
|||   * `parseAttestationType` rejects unknown strings (`""`).
|||   * `parseHashAlgorithm` rejects weaker algorithms ("md5", "sha1").
|||   * `routingAttestation` / `accessAttestation` / `policyAttestation` /
|||     `trustAttestation` set the declared attestType.
|||
||| Zero `believe_me` / `idris_crash`, zero OWED.
module Proven.SafeAttestation.Proofs

import Proven.SafeAttestation

%default total

--------------------------------------------------------------------------------
-- AttestationType enum self-equality
--------------------------------------------------------------------------------

public export
routingSelfEq : RoutingAttestation == RoutingAttestation = True
routingSelfEq = Refl

public export
accessSelfEq : AccessAttestation == AccessAttestation = True
accessSelfEq = Refl

public export
policySelfEq : PolicyAttestation == PolicyAttestation = True
policySelfEq = Refl

public export
trustSelfEq : TrustAttestation == TrustAttestation = True
trustSelfEq = Refl

||| Distinct types are unequal — anchors fall-through.
public export
routingNotAccess : RoutingAttestation == AccessAttestation = False
routingNotAccess = Refl

public export
policyNotTrust : PolicyAttestation == TrustAttestation = False
policyNotTrust = Refl

--------------------------------------------------------------------------------
-- HashAlgorithm enum self-equality
--------------------------------------------------------------------------------

public export
sha256SelfEq : SHA256 == SHA256 = True
sha256SelfEq = Refl

public export
sha512SelfEq : SHA512 == SHA512 = True
sha512SelfEq = Refl

public export
sha256NotSha512 : SHA256 == SHA512 = False
sha256NotSha512 = Refl

--------------------------------------------------------------------------------
-- `attestTypeToString` wire-format anchors
--------------------------------------------------------------------------------

public export
routingWireFormat : attestTypeToString RoutingAttestation = "routing-attestation"
routingWireFormat = Refl

public export
accessWireFormat : attestTypeToString AccessAttestation = "access-attestation"
accessWireFormat = Refl

public export
policyWireFormat : attestTypeToString PolicyAttestation = "policy-attestation"
policyWireFormat = Refl

public export
trustWireFormat : attestTypeToString TrustAttestation = "trust-attestation"
trustWireFormat = Refl

--------------------------------------------------------------------------------
-- `hashAlgoPrefix` wire-format anchors
--------------------------------------------------------------------------------

public export
sha256Prefix : hashAlgoPrefix SHA256 = "sha256"
sha256Prefix = Refl

public export
sha512Prefix : hashAlgoPrefix SHA512 = "sha512"
sha512Prefix = Refl

--------------------------------------------------------------------------------
-- `parseAttestationType` rejection of unknown inputs
--------------------------------------------------------------------------------

||| Empty string rejected — there is no "default" attestation type.
public export
parseAttestEmptyRejected : parseAttestationType "" = Nothing
parseAttestEmptyRejected = Refl

||| Unknown string rejected (no silent fallback).
public export
parseAttestUnknownRejected : parseAttestationType "bogus" = Nothing
parseAttestUnknownRejected = Refl

--------------------------------------------------------------------------------
-- `parseHashAlgorithm` rejection of weak algorithms
--------------------------------------------------------------------------------

||| Weak algorithm "md5" is rejected — only SHA-256 / SHA-512 accepted.
public export
md5Rejected : parseHashAlgorithm "md5" = Nothing
md5Rejected = Refl

||| Weak algorithm "sha1" is rejected.
public export
sha1Rejected : parseHashAlgorithm "sha1" = Nothing
sha1Rejected = Refl

||| Empty string rejected.
public export
parseHashAlgoEmptyRejected : parseHashAlgorithm "" = Nothing
parseHashAlgoEmptyRejected = Refl

--------------------------------------------------------------------------------
-- Smart constructors record the declared attestation type
--------------------------------------------------------------------------------

||| `routingAttestation` constructs an attestation of type
||| `RoutingAttestation`.
public export
routingAttestationIsRouting :
  (issuer : String) -> (hash : VerifiedHash)
  -> (routingAttestation issuer hash).attestType = RoutingAttestation
routingAttestationIsRouting issuer hash = Refl

public export
accessAttestationIsAccess :
  (issuer : String) -> (hash : VerifiedHash)
  -> (accessAttestation issuer hash).attestType = AccessAttestation
accessAttestationIsAccess issuer hash = Refl

public export
policyAttestationIsPolicy :
  (issuer : String) -> (hash : VerifiedHash)
  -> (policyAttestation issuer hash).attestType = PolicyAttestation
policyAttestationIsPolicy issuer hash = Refl

public export
trustAttestationIsTrust :
  (issuer : String) -> (hash : VerifiedHash)
  -> (trustAttestation issuer hash).attestType = TrustAttestation
trustAttestationIsTrust issuer hash = Refl

||| Smart constructors set the standard version "1.0".
public export
routingAttestationVersion :
  (issuer : String) -> (hash : VerifiedHash)
  -> (routingAttestation issuer hash).version = "1.0"
routingAttestationVersion issuer hash = Refl

public export
accessAttestationVersion :
  (issuer : String) -> (hash : VerifiedHash)
  -> (accessAttestation issuer hash).version = "1.0"
accessAttestationVersion issuer hash = Refl

public export
policyAttestationVersion :
  (issuer : String) -> (hash : VerifiedHash)
  -> (policyAttestation issuer hash).version = "1.0"
policyAttestationVersion issuer hash = Refl

public export
trustAttestationVersion :
  (issuer : String) -> (hash : VerifiedHash)
  -> (trustAttestation issuer hash).version = "1.0"
trustAttestationVersion issuer hash = Refl
