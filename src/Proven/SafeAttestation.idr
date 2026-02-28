-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- SafeAttestation — Verified a2ml attestation types.
--
-- Defines the structure and invariants for a2ml attestation records
-- used across the hyperpolymath ecosystem. Attestations provide
-- content-addressable audit records for decisions (routing, access,
-- policy evaluation).
--
-- Key invariant: an attestation's hash is computed from its payload,
-- making it self-verifying. This module defines the types that
-- ensure attestation records are well-formed.
--
-- Extracted from: cadre-router, hybrid-automation-router,
--                 http-capability-gateway optimisation work.
--
-- Design rationale:
--   The a2ml attestation format was designed to be:
--     1. Content-addressable: the hash uniquely identifies the decision
--     2. Self-describing: the attestation type tells you what was decided
--     3. Auditable: every field needed for post-hoc review is present
--     4. Verifiable: the hash can be recomputed from the payload
--
--   This module provides the TYPE-LEVEL guarantees:
--     - Hash strings are non-empty (via So proof)
--     - Attestation records contain all required fields
--     - Serialisation is total and complete
--
--   The actual hash computation happens in the FFI layer (via
--   Proven.SafeCrypto), but the type-level constraints here
--   ensure that malformed attestations cannot be constructed.

module Proven.SafeAttestation

import public Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- Attestation Type
--------------------------------------------------------------------------------

||| Attestation type identifier.
|||
||| Each attestation records a specific kind of decision made by
||| a component in the hyperpolymath infrastructure. The type
||| discriminator allows audit log consumers to route attestations
||| to the appropriate analysis pipeline.
|||
||| Adding a new attestation type requires:
|||   1. Adding a constructor here
|||   2. Adding a case to attestTypeToString
|||   3. Adding a case to parseAttestationType
|||   4. Updating the round-trip proof
public export
data AttestationType : Type where
  ||| A routing decision made by hybrid-automation-router.
  ||| Records which backend was selected for a request, the
  ||| selection criteria, and the confidence score.
  RoutingAttestation : AttestationType
  ||| An access control decision made by http-capability-gateway.
  ||| Records the trust level, exposure level, and verdict (allow/deny).
  AccessAttestation : AttestationType
  ||| A policy evaluation result from the policy compiler.
  ||| Records which policies were evaluated, in what order,
  ||| and the final composed verdict.
  PolicyAttestation : AttestationType
  ||| A trust verification event (mTLS handshake, API key validation,
  ||| JWT signature check, etc.). Records the verification method,
  ||| the resulting trust level, and any verification errors.
  TrustAttestation : AttestationType

||| Decidable equality for AttestationType.
||| Used for filtering attestation streams by type.
public export
Eq AttestationType where
  RoutingAttestation == RoutingAttestation = True
  AccessAttestation == AccessAttestation = True
  PolicyAttestation == PolicyAttestation = True
  TrustAttestation == TrustAttestation = True
  _ == _ = False

||| Show instance for AttestationType.
||| Produces the wire-format string used in a2ml documents.
public export
Show AttestationType where
  show RoutingAttestation = "routing-attestation"
  show AccessAttestation = "access-attestation"
  show PolicyAttestation = "policy-attestation"
  show TrustAttestation = "trust-attestation"

--------------------------------------------------------------------------------
-- Hash Algorithm
--------------------------------------------------------------------------------

||| Hash algorithm used for attestation verification.
|||
||| Only cryptographically secure hash algorithms are permitted.
||| MD5 and SHA-1 are deliberately excluded — they are broken
||| for content-addressable use cases.
|||
||| SHA-256 is the default and minimum acceptable algorithm.
||| SHA-512 is available for higher-security contexts where
||| collision resistance beyond 128 bits is required.
public export
data HashAlgorithm : Type where
  ||| SHA-256 — default and minimum acceptable.
  ||| Provides 128-bit collision resistance and 256-bit
  ||| preimage resistance. Sufficient for all standard
  ||| attestation use cases.
  SHA256 : HashAlgorithm
  ||| SHA-512 — for higher security contexts.
  ||| Provides 256-bit collision resistance and 512-bit
  ||| preimage resistance. Use when attestations may be
  ||| subject to targeted collision attacks (rare in practice,
  ||| but required by some compliance frameworks).
  SHA512 : HashAlgorithm

||| Decidable equality for HashAlgorithm.
public export
Eq HashAlgorithm where
  SHA256 == SHA256 = True
  SHA512 == SHA512 = True
  _ == _ = False

||| Show instance for HashAlgorithm.
||| Produces the prefix string used in hash URIs (e.g., "sha256:...").
public export
Show HashAlgorithm where
  show SHA256 = "sha256"
  show SHA512 = "sha512"

--------------------------------------------------------------------------------
-- Verified Hash
--------------------------------------------------------------------------------

||| A verified hash string.
|||
||| The key invariant is that the hex digest is non-empty, enforced
||| at the type level via a `So` proof. This prevents construction
||| of attestation records with empty hashes, which would be
||| meaningless for verification purposes.
|||
||| The algorithm field records which hash function was used,
||| enabling the verifier to select the correct algorithm when
||| recomputing the hash from the payload.
|||
||| @ algorithm  Which hash function produced this digest
||| @ hexDigest  The hex-encoded hash value (must be non-empty)
||| @ nonEmpty   Type-level proof that hexDigest has length > 0
public export
record VerifiedHash where
  constructor MkVerifiedHash
  ||| The hash algorithm used to produce this digest.
  algorithm : HashAlgorithm
  ||| The hex-encoded digest string.
  ||| Must be non-empty (enforced by the nonEmpty proof).
  ||| Expected format: lowercase hex characters (0-9, a-f).
  hexDigest : String
  ||| Type-level proof that the hex digest is non-empty.
  ||| This is an erased proof (runtime cost: zero) that prevents
  ||| construction of verified hashes with empty strings.
  0 nonEmpty : So (length hexDigest > 0)

||| Smart constructor for verified hashes.
|||
||| Validates that the hash hex digest is non-empty. Returns
||| Nothing if the digest string is empty, preventing construction
||| of invalid attestation records.
|||
||| This is the ONLY way to construct a VerifiedHash from
||| untrusted input. The MkVerifiedHash constructor requires
||| a proof, which callers generally cannot provide for runtime
||| strings. Use this function instead.
|||
||| @ algo  The hash algorithm that produced the digest
||| @ hex   The hex-encoded digest string (must be non-empty)
public export
mkVerifiedHash : (algo : HashAlgorithm) -> (hex : String) -> Maybe VerifiedHash
mkVerifiedHash algo hex =
  case choose (length hex > 0) of
    Left prf => Just (MkVerifiedHash algo hex prf)
    Right _ => Nothing

||| Decidable equality for VerifiedHash.
||| Two hashes are equal if they use the same algorithm and
||| have the same hex digest. This enables deduplication of
||| identical attestation records.
public export
Eq VerifiedHash where
  h1 == h2 = h1.algorithm == h2.algorithm && h1.hexDigest == h2.hexDigest

--------------------------------------------------------------------------------
-- Attestation Envelope
--------------------------------------------------------------------------------

||| An a2ml attestation envelope.
|||
||| Contains the metadata about the attestation (what was decided,
||| by whom, using which format version) and the decision hash
||| that makes the record content-addressable.
|||
||| The envelope does NOT contain the decision payload itself —
||| only its hash. The payload is stored separately (e.g., in
||| VeriSimDB) and can be retrieved using the hash. This design:
|||   1. Keeps attestation records small (suitable for headers)
|||   2. Enables content-addressable lookup
|||   3. Prevents tampering (hash must match payload)
|||
||| @ version      Format version string (e.g., "1.0")
||| @ attestType   What kind of decision this attests
||| @ issuer       Which service issued the attestation
||| @ decisionHash SHA hash of the decision payload
public export
record Attestation where
  constructor MkAttestation
  ||| Version of the a2ml attestation format.
  ||| Currently "1.0". Future versions may add fields;
  ||| the version allows consumers to select the correct parser.
  version : String
  ||| What kind of decision this attestation records.
  ||| Determines how the decision payload should be interpreted.
  attestType : AttestationType
  ||| The service that issued this attestation.
  ||| Must be a registered service name in the infrastructure
  ||| registry (e.g., "hybrid-automation-router", "http-capability-gateway").
  issuer : String
  ||| Content hash of the decision payload.
  ||| Computed from the canonical serialisation of the decision.
  ||| The hash algorithm is embedded in the VerifiedHash record.
  decisionHash : VerifiedHash

||| Decidable equality for Attestation.
||| Two attestations are equal if all four fields match.
public export
Eq Attestation where
  a1 == a2 = a1.version == a2.version
          && a1.attestType == a2.attestType
          && a1.issuer == a2.issuer
          && a1.decisionHash == a2.decisionHash

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

||| Convert attestation type to wire format string.
|||
||| The wire format uses lowercase-with-hyphens, matching the
||| convention used in HTTP headers and JSON keys throughout
||| the hyperpolymath infrastructure.
|||
||| @ at  The attestation type to serialise
public export
attestTypeToString : (at : AttestationType) -> String
attestTypeToString RoutingAttestation = "routing-attestation"
attestTypeToString AccessAttestation = "access-attestation"
attestTypeToString PolicyAttestation = "policy-attestation"
attestTypeToString TrustAttestation = "trust-attestation"

||| Parse attestation type from wire format string.
|||
||| Returns Nothing for unrecognised strings. Unlike the
||| fail-safe defaults in SafeTrust (which default to the
||| least-privileged level), attestation types have no safe
||| default — an unrecognised type should be rejected, not
||| silently mapped to something else.
|||
||| @ s  The string to parse
public export
parseAttestationType : (s : String) -> Maybe AttestationType
parseAttestationType "routing-attestation" = Just RoutingAttestation
parseAttestationType "access-attestation" = Just AccessAttestation
parseAttestationType "policy-attestation" = Just PolicyAttestation
parseAttestationType "trust-attestation" = Just TrustAttestation
parseAttestationType _ = Nothing

||| Proof that parseAttestationType round-trips with attestTypeToString.
|||
||| Ensures that serialisation followed by deserialisation recovers
||| the original attestation type (wrapped in Just).
|||
||| @ at  The attestation type to prove round-trip for
public export
parseAttestTypeRoundTrip : (at : AttestationType)
                        -> parseAttestationType (attestTypeToString at) = Just at
parseAttestTypeRoundTrip RoutingAttestation = Refl
parseAttestTypeRoundTrip AccessAttestation = Refl
parseAttestTypeRoundTrip PolicyAttestation = Refl
parseAttestTypeRoundTrip TrustAttestation = Refl

||| Convert hash algorithm to its prefix string.
|||
||| This prefix is used in hash URIs: "sha256:abcdef1234..."
||| The prefix format follows the convention used by Docker/OCI
||| content-addressable storage and Subresource Integrity (SRI).
|||
||| @ algo  The hash algorithm to get the prefix for
public export
hashAlgoPrefix : (algo : HashAlgorithm) -> String
hashAlgoPrefix SHA256 = "sha256"
hashAlgoPrefix SHA512 = "sha512"

||| Parse hash algorithm from prefix string.
|||
||| Returns Nothing for unrecognised algorithms.
||| Only SHA-256 and SHA-512 are accepted — weaker algorithms
||| (MD5, SHA-1) are deliberately rejected.
|||
||| @ s  The prefix string to parse
public export
parseHashAlgorithm : (s : String) -> Maybe HashAlgorithm
parseHashAlgorithm "sha256" = Just SHA256
parseHashAlgorithm "sha512" = Just SHA512
parseHashAlgorithm _ = Nothing

||| Proof that parseHashAlgorithm round-trips with hashAlgoPrefix.
|||
||| @ algo  The hash algorithm to prove round-trip for
public export
parseHashAlgoRoundTrip : (algo : HashAlgorithm)
                      -> parseHashAlgorithm (hashAlgoPrefix algo) = Just algo
parseHashAlgoRoundTrip SHA256 = Refl
parseHashAlgoRoundTrip SHA512 = Refl

||| Format a verified hash as its prefixed string representation.
|||
||| Produces strings like "sha256:abcdef1234567890..." following
||| the OCI/SRI convention. This is the canonical serialisation
||| format used in a2ml documents and HTTP headers.
|||
||| @ h  The verified hash to format
public export
formatHash : (h : VerifiedHash) -> String
formatHash h = hashAlgoPrefix h.algorithm ++ ":" ++ h.hexDigest

--------------------------------------------------------------------------------
-- Attestation Construction Helpers
--------------------------------------------------------------------------------

||| Create a routing attestation with the standard version.
|||
||| Convenience constructor for the most common case: a routing
||| decision by hybrid-automation-router using SHA-256.
|||
||| @ issuer  The service name issuing the attestation
||| @ hash    A verified hash of the routing decision payload
public export
routingAttestation : (issuer : String) -> (hash : VerifiedHash) -> Attestation
routingAttestation issuer hash = MkAttestation "1.0" RoutingAttestation issuer hash

||| Create an access attestation with the standard version.
|||
||| Convenience constructor for access control decisions by
||| http-capability-gateway.
|||
||| @ issuer  The service name issuing the attestation
||| @ hash    A verified hash of the access decision payload
public export
accessAttestation : (issuer : String) -> (hash : VerifiedHash) -> Attestation
accessAttestation issuer hash = MkAttestation "1.0" AccessAttestation issuer hash

||| Create a policy attestation with the standard version.
|||
||| Convenience constructor for policy evaluation results.
|||
||| @ issuer  The service name issuing the attestation
||| @ hash    A verified hash of the policy evaluation payload
public export
policyAttestation : (issuer : String) -> (hash : VerifiedHash) -> Attestation
policyAttestation issuer hash = MkAttestation "1.0" PolicyAttestation issuer hash

||| Create a trust attestation with the standard version.
|||
||| Convenience constructor for trust verification events.
|||
||| @ issuer  The service name issuing the attestation
||| @ hash    A verified hash of the trust verification payload
public export
trustAttestation : (issuer : String) -> (hash : VerifiedHash) -> Attestation
trustAttestation issuer hash = MkAttestation "1.0" TrustAttestation issuer hash

--------------------------------------------------------------------------------
-- Attestation Validation
--------------------------------------------------------------------------------

||| Validate that an attestation has all required fields populated.
|||
||| Checks that the version, issuer, and hash are all non-empty.
||| The hash non-emptiness is already guaranteed by the VerifiedHash
||| type, but the version and issuer are plain Strings that could
||| be empty if constructed carelessly.
|||
||| Returns a Result with a descriptive error message on failure.
|||
||| @ att  The attestation to validate
public export
validateAttestation : (att : Attestation) -> Result SafeError Attestation
validateAttestation att =
  if length att.version == 0
    then Err (ValidationError "version" "Attestation version must not be empty")
    else if length att.issuer == 0
      then Err (ValidationError "issuer" "Attestation issuer must not be empty")
      else Ok att
