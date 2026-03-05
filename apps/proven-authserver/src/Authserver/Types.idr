-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-authserver: Core protocol types for authentication server.
--
-- All types are closed sum types with total Show instances.
-- No parsers, no full implementations -- skeleton only.

module Authserver.Types

%default total

-- ============================================================================
-- AuthMethod
-- ============================================================================

||| Supported authentication methods.
public export
data AuthMethod : Type where
  ||| Username and password authentication.
  Password    : AuthMethod
  ||| X.509 client certificate authentication.
  Certificate : AuthMethod
  ||| OAuth 2.0 authorization code or implicit flow.
  OAuth2      : AuthMethod
  ||| Security Assertion Markup Language (federated SSO).
  SAML        : AuthMethod
  ||| FIDO2/WebAuthn hardware token authentication.
  FIDO2       : AuthMethod
  ||| Kerberos ticket-based authentication.
  Kerberos    : AuthMethod
  ||| Lightweight Directory Access Protocol bind.
  LDAP        : AuthMethod
  ||| Remote Authentication Dial-In User Service.
  RADIUS      : AuthMethod

export
Show AuthMethod where
  show Password    = "Password"
  show Certificate = "Certificate"
  show OAuth2      = "OAuth2"
  show SAML        = "SAML"
  show FIDO2       = "FIDO2"
  show Kerberos    = "Kerberos"
  show LDAP        = "LDAP"
  show RADIUS      = "RADIUS"

-- ============================================================================
-- TokenType
-- ============================================================================

||| Categories of tokens issued by the authentication server.
public export
data TokenType : Type where
  ||| Short-lived access token for API calls.
  Access  : TokenType
  ||| Long-lived refresh token for obtaining new access tokens.
  Refresh : TokenType
  ||| Identity token carrying user claims (OpenID Connect).
  ID      : TokenType
  ||| API key for machine-to-machine authentication.
  API     : TokenType

export
Show TokenType where
  show Access  = "Access"
  show Refresh = "Refresh"
  show ID      = "ID"
  show API     = "API"

-- ============================================================================
-- AuthResult
-- ============================================================================

||| Outcome of an authentication attempt.
public export
data AuthResult : Type where
  ||| Authentication succeeded.
  Success            : AuthResult
  ||| Credentials were invalid (wrong password, bad certificate, etc.).
  InvalidCredentials : AuthResult
  ||| Account is locked due to too many failed attempts.
  AccountLocked      : AuthResult
  ||| Account has expired or been deactivated.
  AccountExpired     : AuthResult
  ||| Primary authentication passed but MFA is required.
  MFARequired        : AuthResult
  ||| Source IP address is blocked by policy.
  IPBlocked          : AuthResult

export
Show AuthResult where
  show Success            = "Success"
  show InvalidCredentials = "InvalidCredentials"
  show AccountLocked      = "AccountLocked"
  show AccountExpired     = "AccountExpired"
  show MFARequired        = "MFARequired"
  show IPBlocked          = "IPBlocked"

-- ============================================================================
-- MFAMethod
-- ============================================================================

||| Supported multi-factor authentication methods.
public export
data MFAMethod : Type where
  ||| Time-based One-Time Password (RFC 6238).
  TOTP      : MFAMethod
  ||| SMS-delivered one-time code.
  SMS       : MFAMethod
  ||| Push notification to a registered device.
  Push      : MFAMethod
  ||| FIDO2/WebAuthn hardware second factor.
  FIDO2_MFA : MFAMethod
  ||| Email-delivered one-time code.
  Email     : MFAMethod

export
Show MFAMethod where
  show TOTP      = "TOTP"
  show SMS       = "SMS"
  show Push      = "Push"
  show FIDO2_MFA = "FIDO2_MFA"
  show Email     = "Email"

-- ============================================================================
-- SessionState
-- ============================================================================

||| State of an authenticated session.
public export
data SessionState : Type where
  ||| Session is active and valid.
  Active  : SessionState
  ||| Session has expired due to timeout.
  Expired : SessionState
  ||| Session was explicitly revoked (logout or admin action).
  Revoked : SessionState
  ||| Session is locked pending re-authentication.
  Locked  : SessionState

export
Show SessionState where
  show Active  = "Active"
  show Expired = "Expired"
  show Revoked = "Revoked"
  show Locked  = "Locked"
