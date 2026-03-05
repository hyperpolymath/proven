-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- ZeroTrust.Types : Core types for the Zero Trust authentication server.
-- Defines authentication factors, trust levels, policy decisions,
-- context signals, and session lifecycle states.

module ZeroTrust.Types

%default total

---------------------------------------------------------------------------
-- AuthFactor : Authentication methods for identity verification.
---------------------------------------------------------------------------

||| Authentication factors supported for identity verification.
public export
data AuthFactor : Type where
  Certificate : AuthFactor
  Token       : AuthFactor
  Biometric   : AuthFactor
  FIDO2       : AuthFactor
  TOTP        : AuthFactor
  Push        : AuthFactor

export
Show AuthFactor where
  show Certificate = "Certificate"
  show Token       = "Token"
  show Biometric   = "Biometric"
  show FIDO2       = "FIDO2"
  show TOTP        = "TOTP"
  show Push        = "Push"

---------------------------------------------------------------------------
-- TrustLevel : Computed trust score tiers.
---------------------------------------------------------------------------

||| Trust level computed from authentication factors and context signals.
public export
data TrustLevel : Type where
  None   : TrustLevel
  Low    : TrustLevel
  Medium : TrustLevel
  High   : TrustLevel
  Full   : TrustLevel

export
Show TrustLevel where
  show None   = "None"
  show Low    = "Low"
  show Medium = "Medium"
  show High   = "High"
  show Full   = "Full"

---------------------------------------------------------------------------
-- PolicyDecision : Access control decisions from the policy engine.
---------------------------------------------------------------------------

||| Decision returned by the zero trust policy engine.
public export
data PolicyDecision : Type where
  Allow      : PolicyDecision
  Deny       : PolicyDecision
  Challenge  : PolicyDecision
  StepUp     : PolicyDecision
  Quarantine : PolicyDecision

export
Show PolicyDecision where
  show Allow      = "Allow"
  show Deny       = "Deny"
  show Challenge  = "Challenge"
  show StepUp     = "StepUp"
  show Quarantine = "Quarantine"

---------------------------------------------------------------------------
-- ContextSignal : Environmental signals for trust computation.
---------------------------------------------------------------------------

||| Environmental and behavioural signals used in trust assessment.
public export
data ContextSignal : Type where
  DeviceHealth    : ContextSignal
  NetworkLocation : ContextSignal
  UserBehavior    : ContextSignal
  TimeOfDay       : ContextSignal
  GeoLocation     : ContextSignal
  RiskScore       : ContextSignal

export
Show ContextSignal where
  show DeviceHealth    = "DeviceHealth"
  show NetworkLocation = "NetworkLocation"
  show UserBehavior    = "UserBehavior"
  show TimeOfDay       = "TimeOfDay"
  show GeoLocation     = "GeoLocation"
  show RiskScore       = "RiskScore"

---------------------------------------------------------------------------
-- SessionState : Session lifecycle states.
---------------------------------------------------------------------------

||| Current state of an authenticated session.
public export
data SessionState : Type where
  Unauthenticated : SessionState
  PartialAuth     : SessionState
  Authenticated   : SessionState
  Elevated        : SessionState
  Locked          : SessionState

export
Show SessionState where
  show Unauthenticated = "Unauthenticated"
  show PartialAuth     = "PartialAuth"
  show Authenticated   = "Authenticated"
  show Elevated        = "Elevated"
  show Locked          = "Locked"
