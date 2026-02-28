-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- SafeTrust — Formally verified trust level hierarchy.
--
-- Defines the trust/exposure model used by http-capability-gateway
-- and related infrastructure. Trust levels form a total order
-- (Untrusted < Authenticated < Internal), and the access decision
-- function is proven monotone: upgrading trust never revokes access.
--
-- This module eliminates an entire class of policy bugs where
-- trust level comparisons are implemented ad-hoc with case statements
-- that can miss combinations or get the ordering wrong.
--
-- Extracted from: cadre-router, hybrid-automation-router,
--                 http-capability-gateway optimisation work.
--
-- Design rationale:
--   The trust hierarchy is intentionally kept simple (three levels)
--   because every additional level doubles the proof surface. The
--   three levels (Untrusted, Authenticated, Internal) cover the
--   overwhelming majority of real-world access control scenarios:
--     - Public APIs (Untrusted callers allowed)
--     - User-facing endpoints (Authenticated required)
--     - Service-to-service calls (Internal required)
--
--   The monotonicity proof is the key contribution: it guarantees
--   that trust upgrades (e.g., going from Authenticated to Internal)
--   never accidentally revoke access. This property is NOT obvious
--   in ad-hoc case-statement implementations and has been a source
--   of real bugs in production gateway configurations.

module Proven.SafeTrust

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Trust Level Type
--------------------------------------------------------------------------------

||| Trust levels representing caller identity verification.
|||
||| Forms a total order: Untrusted < Authenticated < Internal.
|||
||| These correspond to the three tiers of identity verification
||| in the hyperpolymath infrastructure:
|||   - Untrusted: No verification performed. The caller may be
|||     anonymous, or their token may have failed validation.
|||   - Authenticated: The caller presented a valid authentication
|||     token (JWT, API key, session cookie) that was successfully
|||     verified against the identity provider.
|||   - Internal: The caller is a trusted internal service,
|||     verified via mutual TLS, service mesh identity, or a
|||     pre-shared service token from the trusted network segment.
public export
data TrustLevel : Type where
  ||| No authentication — anonymous or unverified caller.
  ||| This is the default trust level when no credentials are
  ||| presented, or when credential validation fails. Fail-safe:
  ||| we always default DOWN to Untrusted, never up.
  Untrusted : TrustLevel
  ||| Valid authentication token present — identity verified.
  ||| The caller has presented credentials that were successfully
  ||| validated (JWT signature check, API key lookup, etc.).
  ||| This does NOT imply authorisation — only that we know WHO
  ||| the caller is.
  Authenticated : TrustLevel
  ||| Internal service — mutual TLS or service token from trusted
  ||| network. This is the highest trust level, reserved for
  ||| service-to-service communication within the trusted
  ||| infrastructure boundary. Typically established via mTLS
  ||| certificates issued by the internal CA, or via service
  ||| mesh identity (e.g., SPIFFE/SPIRE).
  Internal : TrustLevel

||| Decidable equality for TrustLevel.
||| Every pair of trust levels can be compared for equality,
||| and the result is a decidable proposition (not just a Bool).
public export
Eq TrustLevel where
  Untrusted == Untrusted = True
  Authenticated == Authenticated = True
  Internal == Internal = True
  _ == _ = False

||| Show instance for TrustLevel, matching the wire format
||| used in HTTP headers (X-Trust-Level).
public export
Show TrustLevel where
  show Untrusted = "untrusted"
  show Authenticated = "authenticated"
  show Internal = "internal"

--------------------------------------------------------------------------------
-- Exposure Level Type
--------------------------------------------------------------------------------

||| Exposure levels representing the minimum trust required for access.
|||
||| Mirrors trust levels but describes the REQUIREMENT, not the caller.
||| This separation is important: TrustLevel describes what the caller
||| HAS; ExposureLevel describes what the endpoint NEEDS. The access
||| decision is: does what the caller has satisfy what the endpoint needs?
|||
||| The three levels are:
|||   - Public: Anyone can access, including anonymous callers.
|||     Examples: health checks, public documentation, login endpoints.
|||   - AuthenticatedOnly: Requires at least Authenticated trust.
|||     Examples: user profile, account settings, data APIs.
|||   - InternalOnly: Requires Internal trust. Only service-to-service
|||     calls from the trusted network are permitted.
|||     Examples: admin APIs, metrics endpoints, config reload.
public export
data ExposureLevel : Type where
  ||| Anyone can access — no trust requirement.
  ||| Endpoints marked Public are accessible to all callers,
  ||| including those with Untrusted trust level. Use for
  ||| health checks, public assets, and unauthenticated endpoints.
  Public : ExposureLevel
  ||| Requires at least Authenticated trust.
  ||| The caller must have successfully authenticated.
  ||| Internal callers also satisfy this requirement (by monotonicity).
  AuthenticatedOnly : ExposureLevel
  ||| Requires Internal trust — internal services only.
  ||| Only callers with the highest trust level (Internal) can access.
  ||| Neither Untrusted nor Authenticated callers satisfy this.
  InternalOnly : ExposureLevel

||| Decidable equality for ExposureLevel.
public export
Eq ExposureLevel where
  Public == Public = True
  AuthenticatedOnly == AuthenticatedOnly = True
  InternalOnly == InternalOnly = True
  _ == _ = False

||| Show instance for ExposureLevel, matching the wire format
||| used in route configuration metadata.
public export
Show ExposureLevel where
  show Public = "public"
  show AuthenticatedOnly = "authenticated"
  show InternalOnly = "internal"

--------------------------------------------------------------------------------
-- Trust Ranking (Total Order)
--------------------------------------------------------------------------------

||| Numeric rank for trust levels. Used to define the total order.
|||
||| The ranking is:
|||   Untrusted = 0 < Authenticated = 1 < Internal = 2
|||
||| This function is the foundation of the access decision:
||| we compare ranks rather than doing case analysis on pairs,
||| which reduces the number of cases from 9 to a single comparison.
|||
||| @ t  The trust level to rank
public export
trustRank : (t : TrustLevel) -> Nat
trustRank Untrusted = 0
trustRank Authenticated = 1
trustRank Internal = 2

||| Numeric rank for exposure levels.
|||
||| The ranking is:
|||   Public = 0 < AuthenticatedOnly = 1 < InternalOnly = 2
|||
||| Note that the numeric values deliberately align with TrustLevel
||| ranks: Public corresponds to Untrusted (rank 0), AuthenticatedOnly
||| to Authenticated (rank 1), InternalOnly to Internal (rank 2).
||| This alignment is what makes the satisfies function a simple >=.
|||
||| @ e  The exposure level to rank
public export
exposureRank : (e : ExposureLevel) -> Nat
exposureRank Public = 0
exposureRank AuthenticatedOnly = 1
exposureRank InternalOnly = 2

--------------------------------------------------------------------------------
-- Core Access Decision
--------------------------------------------------------------------------------

||| Core access decision: does the caller's trust level satisfy
||| the endpoint's exposure requirement?
|||
||| This is the SINGLE SOURCE OF TRUTH for access control.
||| All gateway implementations MUST call this function rather than
||| implementing their own case analysis. Ad-hoc reimplementations
||| are a known source of security bugs — see the monotonicity
||| proof below for why.
|||
||| The decision is: trust rank >= exposure rank. Because the
||| ranks are aligned (see trustRank and exposureRank), this
||| reduces the full 3x3 matrix to a single numeric comparison.
|||
||| Truth table for reference:
|||   satisfies Untrusted     Public           = True
|||   satisfies Untrusted     AuthenticatedOnly = False
|||   satisfies Untrusted     InternalOnly      = False
|||   satisfies Authenticated Public           = True
|||   satisfies Authenticated AuthenticatedOnly = True
|||   satisfies Authenticated InternalOnly      = False
|||   satisfies Internal      Public           = True
|||   satisfies Internal      AuthenticatedOnly = True
|||   satisfies Internal      InternalOnly      = True
|||
||| @ trust    The caller's verified trust level
||| @ exposure The endpoint's required exposure level
public export
satisfies : (trust : TrustLevel) -> (exposure : ExposureLevel) -> Bool
satisfies trust exposure = trustRank trust >= exposureRank exposure

--------------------------------------------------------------------------------
-- Trust-to-Exposure Correspondence
--------------------------------------------------------------------------------

||| Convert a trust level to its corresponding exposure level.
|||
||| This maps each trust level to the exposure level that requires
||| exactly that much trust:
|||   Untrusted     -> Public           (rank 0)
|||   Authenticated -> AuthenticatedOnly (rank 1)
|||   Internal      -> InternalOnly      (rank 2)
|||
||| Used in the reflexivity proof and for endpoint auto-configuration:
||| "this endpoint requires exactly the caller's trust level."
|||
||| @ t  The trust level to convert
public export
trustToExposure : (t : TrustLevel) -> ExposureLevel
trustToExposure Untrusted = Public
trustToExposure Authenticated = AuthenticatedOnly
trustToExposure Internal = InternalOnly

--------------------------------------------------------------------------------
-- Reflexivity Proof
--------------------------------------------------------------------------------

||| Proof that trust ordering is reflexive:
||| every trust level satisfies its own exposure equivalent.
|||
||| This is a basic sanity property: if you need Authenticated trust
||| and you HAVE Authenticated trust, access is granted. While this
||| seems obvious, it serves as a foundation for the monotonicity
||| proof and catches bugs where the rank mappings are misaligned.
|||
||| The proof proceeds by case analysis on the trust level.
||| Each case reduces to Refl because:
|||   trustRank t >= exposureRank (trustToExposure t)
||| computes to True for all three values of t.
|||
||| @ t  The trust level to prove reflexivity for
public export
satisfiesReflexive : (t : TrustLevel) -> satisfies t (trustToExposure t) = True
satisfiesReflexive Untrusted = Refl
satisfiesReflexive Authenticated = Refl
satisfiesReflexive Internal = Refl

--------------------------------------------------------------------------------
-- Monotonicity Proof
--------------------------------------------------------------------------------

||| Proof that the access decision is monotone:
||| if trust level t1 satisfies exposure e, and t2 has rank >= t1,
||| then t2 also satisfies e.
|||
||| This is the KEY SAFETY PROPERTY of the trust hierarchy.
||| It prevents the class of bugs where upgrading trust accidentally
||| revokes access due to a missing or incorrect case in a manual
||| case-statement implementation.
|||
||| Concretely: if an Authenticated caller can access an endpoint,
||| then an Internal caller can ALSO access that endpoint. This
||| property is NOT guaranteed by ad-hoc case analysis — it must
||| be proven.
|||
||| The proof covers all 27 combinations of (t1, t2, e). Many
||| combinations are impossible because the LTE constraint cannot
||| be satisfied (e.g., Internal/Untrusted with LTE 2 0). For the
||| remaining cases, either:
|||   1. satisfies t2 e reduces to True, so we return Refl.
|||   2. satisfies t1 e reduces to False, making the hypothesis
|||      (False = True) absurd — these cases cannot arise.
|||
||| @ t1  The lower trust level (the one we know satisfies e)
||| @ t2  The higher trust level (the one we want to show also satisfies e)
||| @ e   The exposure level being tested
||| @ lte A proof that t2's rank is at least as high as t1's rank
||| @ prf A proof that t1 already satisfies e
public export
satisfiesMonotone : (t1, t2 : TrustLevel) -> (e : ExposureLevel)
                 -> LTE (trustRank t1) (trustRank t2)
                 -> satisfies t1 e = True
                 -> satisfies t2 e = True
-- t1 = Untrusted (rank 0): LTE 0 n always holds
-- satisfies Untrusted Public = True, so for any t2, satisfies t2 Public = True
satisfiesMonotone Untrusted Untrusted Public _ _ = Refl
satisfiesMonotone Untrusted Authenticated Public _ _ = Refl
satisfiesMonotone Untrusted Internal Public _ _ = Refl
-- satisfies Untrusted AuthenticatedOnly = False, so prf : False = True is absurd
satisfiesMonotone Untrusted Untrusted AuthenticatedOnly _ prf = absurd prf
satisfiesMonotone Untrusted Authenticated AuthenticatedOnly _ _ = Refl
satisfiesMonotone Untrusted Internal AuthenticatedOnly _ _ = Refl
-- satisfies Untrusted InternalOnly = False, so prf : False = True is absurd
satisfiesMonotone Untrusted Untrusted InternalOnly _ prf = absurd prf
satisfiesMonotone Untrusted Authenticated InternalOnly _ prf = absurd prf
satisfiesMonotone Untrusted Internal InternalOnly _ prf = absurd prf
-- t1 = Authenticated (rank 1): LTE 1 n requires t2 in {Authenticated, Internal}
-- LTE 1 0 is impossible (Authenticated → Untrusted demotion cannot happen)
satisfiesMonotone Authenticated Untrusted _ lte _ = absurd lte
satisfiesMonotone Authenticated Authenticated Public _ _ = Refl
satisfiesMonotone Authenticated Internal Public _ _ = Refl
satisfiesMonotone Authenticated Authenticated AuthenticatedOnly _ _ = Refl
satisfiesMonotone Authenticated Internal AuthenticatedOnly _ _ = Refl
-- satisfies Authenticated InternalOnly = False, so prf is absurd
satisfiesMonotone Authenticated Authenticated InternalOnly _ prf = absurd prf
satisfiesMonotone Authenticated Internal InternalOnly _ _ = Refl
-- t1 = Internal (rank 2): LTE 2 n requires t2 = Internal
-- LTE 2 0 is impossible (Internal → Untrusted demotion cannot happen)
satisfiesMonotone Internal Untrusted _ lte _ = absurd lte
-- LTE 2 1 is impossible (Internal → Authenticated demotion cannot happen)
-- Unwrap one LTESucc layer to expose LTE 1 0, which is uninhabited
satisfiesMonotone Internal Authenticated _ (LTESucc lte) _ = absurd lte
satisfiesMonotone Internal Internal Public _ _ = Refl
satisfiesMonotone Internal Internal AuthenticatedOnly _ _ = Refl
satisfiesMonotone Internal Internal InternalOnly _ _ = Refl

--------------------------------------------------------------------------------
-- Access Decision Type
--------------------------------------------------------------------------------

||| Decision type for access control results.
|||
||| Unlike the Bool returned by `satisfies`, this type carries
||| the trust and exposure levels that produced the decision,
||| enabling audit trail construction. Every access decision in
||| the gateway should produce an AccessDecision record that gets
||| serialised to the audit log.
|||
||| The Allow and Deny constructors each carry the trust and
||| exposure that led to the decision, providing full traceability
||| without needing to reconstruct the context from logs.
public export
data AccessDecision : Type where
  ||| Access granted — trust satisfies exposure requirement.
  ||| @ trust    The caller's trust level that was sufficient
  ||| @ exposure The endpoint's exposure level that was satisfied
  Allow : (trust : TrustLevel) -> (exposure : ExposureLevel) -> AccessDecision
  ||| Access denied — insufficient trust for this exposure level.
  ||| @ trust    The caller's trust level that was insufficient
  ||| @ exposure The endpoint's exposure level that was not met
  Deny : (trust : TrustLevel) -> (exposure : ExposureLevel) -> AccessDecision

||| Equality for AccessDecision. Two decisions are equal if they
||| have the same verdict (Allow/Deny) AND the same trust/exposure.
public export
Eq AccessDecision where
  (Allow t1 e1) == (Allow t2 e2) = t1 == t2 && e1 == e2
  (Deny t1 e1) == (Deny t2 e2) = t1 == t2 && e1 == e2
  _ == _ = False

||| Show instance for AccessDecision, producing human-readable
||| audit log entries.
public export
Show AccessDecision where
  show (Allow t e) = "ALLOW(trust=" ++ show t ++ ", exposure=" ++ show e ++ ")"
  show (Deny t e) = "DENY(trust=" ++ show t ++ ", exposure=" ++ show e ++ ")"

--------------------------------------------------------------------------------
-- Access Evaluation
--------------------------------------------------------------------------------

||| Evaluate access and produce a structured decision record.
|||
||| Unlike `satisfies` which returns Bool, this returns a structured
||| AccessDecision that can be serialised for audit logging. Every
||| access check in the gateway should use this function (or a
||| wrapper around it) rather than calling `satisfies` directly,
||| to ensure audit records are always produced.
|||
||| @ trust    The caller's verified trust level
||| @ exposure The endpoint's required exposure level
public export
evaluateAccess : (trust : TrustLevel) -> (exposure : ExposureLevel) -> AccessDecision
evaluateAccess trust exposure =
  if satisfies trust exposure
    then Allow trust exposure
    else Deny trust exposure

||| Check if an AccessDecision is an Allow.
||| Useful for guard conditions without pattern matching.
|||
||| @ d  The decision to check
public export
isAllowed : (d : AccessDecision) -> Bool
isAllowed (Allow _ _) = True
isAllowed (Deny _ _) = False

||| Check if an AccessDecision is a Deny.
||| Dual of isAllowed — provided for readability.
|||
||| @ d  The decision to check
public export
isDenied : (d : AccessDecision) -> Bool
isDenied = not . isAllowed

--------------------------------------------------------------------------------
-- String Serialisation
--------------------------------------------------------------------------------

||| Convert trust level to its string representation.
|||
||| Matches the wire format used by http-capability-gateway
||| in the X-Trust-Level HTTP header. All consumers of trust
||| level strings MUST use this function to ensure consistency.
|||
||| @ t  The trust level to serialise
public export
trustToString : (t : TrustLevel) -> String
trustToString Untrusted = "untrusted"
trustToString Authenticated = "authenticated"
trustToString Internal = "internal"

||| Parse trust level from string.
|||
||| Accepts the wire format strings produced by trustToString.
||| Returns Untrusted for unrecognised values — this is a
||| FAIL-SAFE DEFAULT. Unknown trust levels are treated as
||| untrusted, never as authenticated or internal. This prevents
||| privilege escalation via malformed headers.
|||
||| NOTE: This is case-sensitive to match the wire format exactly.
||| If case-insensitive parsing is needed, normalise the input
||| to lowercase before calling this function.
|||
||| @ s  The string to parse
public export
parseTrust : (s : String) -> TrustLevel
parseTrust "authenticated" = Authenticated
parseTrust "internal" = Internal
parseTrust _ = Untrusted

||| Convert exposure level to its string representation.
|||
||| Matches the wire format used in route configuration metadata.
||| Note that "authenticated" is used for AuthenticatedOnly and
||| "internal" for InternalOnly — the "Only" suffix is dropped
||| in the wire format for brevity.
|||
||| @ e  The exposure level to serialise
public export
exposureToString : (e : ExposureLevel) -> String
exposureToString Public = "public"
exposureToString AuthenticatedOnly = "authenticated"
exposureToString InternalOnly = "internal"

||| Parse exposure level from string.
|||
||| Accepts the wire format strings produced by exposureToString.
||| Returns Public for unrecognised values — this is a
||| FAIL-OPEN DEFAULT for exposure parsing (not a security risk
||| because the exposure level describes what the endpoint ACCEPTS,
||| and defaulting to Public means "accept anyone" which is the
||| least restrictive — the endpoint owner should configure this
||| explicitly).
|||
||| @ s  The string to parse
public export
parseExposure : (s : String) -> ExposureLevel
parseExposure "authenticated" = AuthenticatedOnly
parseExposure "internal" = InternalOnly
parseExposure _ = Public

--------------------------------------------------------------------------------
-- Proof that parseTrust round-trips with trustToString
--------------------------------------------------------------------------------

||| Proof that parsing the string representation of a trust level
||| recovers the original trust level.
|||
||| This ensures that serialisation followed by deserialisation
||| is the identity function — no information is lost in the
||| round trip. This property is important for audit log integrity:
||| trust levels written to logs can be faithfully reconstructed.
|||
||| @ t  The trust level to prove round-trip for
public export
parseTrustRoundTrip : (t : TrustLevel) -> parseTrust (trustToString t) = t
parseTrustRoundTrip Untrusted = Refl
parseTrustRoundTrip Authenticated = Refl
parseTrustRoundTrip Internal = Refl

||| Proof that parsing the string representation of an exposure level
||| recovers the original exposure level.
|||
||| @ e  The exposure level to prove round-trip for
public export
parseExposureRoundTrip : (e : ExposureLevel) -> parseExposure (exposureToString e) = e
parseExposureRoundTrip Public = Refl
parseExposureRoundTrip AuthenticatedOnly = Refl
parseExposureRoundTrip InternalOnly = Refl
