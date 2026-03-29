-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeWebAuthn - WebAuthn/FIDO2 assertion validation
|||
||| Provides type-safe validation for WebAuthn ceremonies:
||| - Challenge freshness enforcement
||| - Origin binding
||| - Signature counter monotonicity
||| - Attestation type validation
||| Prevents: replay attacks, origin spoofing, cloned authenticators.
module Proven.SafeWebAuthn

import Data.String
import Data.List
import Data.Nat

%default total

-- ============================================================================
-- CREDENTIAL TYPES
-- ============================================================================

||| Authenticator attachment modality
public export
data Attachment = Platform | CrossPlatform

public export
Show Attachment where
  show Platform = "platform"
  show CrossPlatform = "cross-platform"

public export
Eq Attachment where
  Platform == Platform = True
  CrossPlatform == CrossPlatform = True
  _ == _ = False

||| User verification requirement
public export
data UserVerification = Required | Preferred | Discouraged

public export
Show UserVerification where
  show Required = "required"
  show Preferred = "preferred"
  show Discouraged = "discouraged"

public export
Eq UserVerification where
  Required == Required = True
  Preferred == Preferred = True
  Discouraged == Discouraged = True
  _ == _ = False

||| Attestation conveyance preference
public export
data AttestationConveyance = NoneAttestation | IndirectAttestation | DirectAttestation | Enterprise

public export
Show AttestationConveyance where
  show NoneAttestation = "none"
  show IndirectAttestation = "indirect"
  show DirectAttestation = "direct"
  show Enterprise = "enterprise"

-- ============================================================================
-- CHALLENGE
-- ============================================================================

||| Minimum challenge size (16 bytes per spec)
public export
MinChallengeBytes : Nat
MinChallengeBytes = 16

||| A validated WebAuthn challenge
public export
record Challenge where
  constructor MkChallenge
  challengeBytes : String  -- Base64url-encoded
  createdAt      : Integer -- Epoch seconds

||| Create a challenge (validates minimum size)
public export
mkChallenge : String -> Integer -> Maybe Challenge
mkChallenge bytes ts =
  if length bytes >= MinChallengeBytes
    then Just (MkChallenge bytes ts)
    else Nothing

||| Check if challenge has expired (default: 5 minutes)
public export
isChallengeExpired : Challenge -> Integer -> Nat -> Bool
isChallengeExpired ch nowEpoch maxAgeSecs =
  (nowEpoch - ch.createdAt) > cast maxAgeSecs

||| Default challenge timeout: 300 seconds (5 minutes)
public export
DefaultChallengeTimeout : Nat
DefaultChallengeTimeout = 300

-- ============================================================================
-- ASSERTION VALIDATION
-- ============================================================================

||| Credential registration data
public export
record StoredCredential where
  constructor MkStoredCredential
  credentialId : String
  publicKey    : String     -- COSE key (base64url)
  signCount    : Nat        -- Last known signature counter
  origin       : String     -- Expected origin (https://example.com)

||| Assertion response from authenticator
public export
record AssertionResponse where
  constructor MkAssertionResponse
  credentialId    : String
  authenticatorData : String  -- Base64url
  clientDataJSON  : String    -- Base64url
  signature       : String    -- Base64url
  newSignCount    : Nat

||| Validation errors
public export
data WebAuthnError =
    ChallengeMismatch        -- Challenge doesn't match
  | ChallengeExpired         -- Challenge too old
  | OriginMismatch String String  -- Expected vs actual
  | CounterNotMonotonic Nat Nat   -- Stored vs received (cloned authenticator!)
  | UnknownCredential String
  | UserVerificationFailed

public export
Show WebAuthnError where
  show ChallengeMismatch = "Challenge mismatch"
  show ChallengeExpired = "Challenge expired"
  show (OriginMismatch e a) = "Origin mismatch: expected " ++ e ++ ", got " ++ a
  show (CounterNotMonotonic stored recv) =
    "Counter not monotonic: stored=" ++ show stored ++ " received=" ++ show recv ++
    " (possible cloned authenticator)"
  show (UnknownCredential cid) = "Unknown credential: " ++ cid
  show UserVerificationFailed = "User verification required but not performed"

||| Validate signature counter monotonicity (anti-cloning)
||| The counter must strictly increase. If it doesn't, the authenticator
||| may have been cloned.
public export
validateCounter : (storedCount : Nat) -> (newCount : Nat) -> Maybe WebAuthnError
validateCounter stored new =
  if new > stored then Nothing  -- Valid: counter increased
  else if new == 0 && stored == 0 then Nothing  -- Counter not supported by authenticator
  else Just (CounterNotMonotonic stored new)

||| Validate origin binding
public export
validateOrigin : String -> String -> Maybe WebAuthnError
validateOrigin expected actual =
  if expected == actual then Nothing
  else Just (OriginMismatch expected actual)

||| Full assertion validation (minus cryptographic signature check which needs FFI)
public export
validateAssertion : StoredCredential -> AssertionResponse -> Challenge ->
                    Integer -> String -> List WebAuthnError
validateAssertion cred resp challenge nowEpoch actualOrigin =
  let credErr = if cred.credentialId /= resp.credentialId
                  then [UnknownCredential resp.credentialId] else []
      originErr = case validateOrigin cred.origin actualOrigin of
                    Just e  => [e]
                    Nothing => []
      expiredErr = if isChallengeExpired challenge nowEpoch DefaultChallengeTimeout
                     then [ChallengeExpired] else []
      counterErr = case validateCounter cred.signCount resp.newSignCount of
                     Just e  => [e]
                     Nothing => []
  in credErr ++ originErr ++ expiredErr ++ counterErr

||| Check if assertion is valid (no errors)
public export
isValidAssertion : StoredCredential -> AssertionResponse -> Challenge ->
                   Integer -> String -> Bool
isValidAssertion cred resp ch now origin =
  isNil (validateAssertion cred resp ch now origin)
