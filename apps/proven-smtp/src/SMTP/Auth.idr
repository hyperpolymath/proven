-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- SMTP AUTH Mechanisms (RFC 4954)
--
-- Defines the supported SMTP authentication mechanisms as a sum type.
-- Each mechanism specifies how credentials are exchanged between client
-- and server. The actual cryptographic operations would be performed
-- by proven's SafeCrypto primitives (via FFI); this module defines
-- the protocol-level types and state machine.

module SMTP.Auth

%default total

-- ============================================================================
-- AUTH Mechanisms (RFC 4954, RFC 2195, RFC 4616)
-- ============================================================================

||| Supported SMTP authentication mechanisms.
public export
data AuthMechanism : Type where
  ||| PLAIN: Credentials sent as base64(NUL + username + NUL + password).
  ||| Simple but requires TLS for security (RFC 4616).
  PLAIN    : AuthMechanism
  ||| LOGIN: Legacy mechanism. Username and password sent in separate
  ||| base64-encoded exchanges. Widely supported but not standardised.
  LOGIN    : AuthMechanism
  ||| CRAM-MD5: Challenge-response using HMAC-MD5 (RFC 2195).
  ||| Server sends a challenge, client responds with HMAC digest.
  CRAM_MD5 : AuthMechanism

public export
Eq AuthMechanism where
  PLAIN    == PLAIN    = True
  LOGIN    == LOGIN    = True
  CRAM_MD5 == CRAM_MD5 = True
  _        == _        = False

public export
Show AuthMechanism where
  show PLAIN    = "PLAIN"
  show LOGIN    = "LOGIN"
  show CRAM_MD5 = "CRAM-MD5"

-- ============================================================================
-- Mechanism properties
-- ============================================================================

||| Parse a mechanism name string (case-insensitive).
public export
parseMechanism : String -> Maybe AuthMechanism
parseMechanism s = case toUpper s of
  "PLAIN"   => Just PLAIN
  "LOGIN"   => Just LOGIN
  "CRAM-MD5" => Just CRAM_MD5
  _          => Nothing

||| Whether the mechanism requires TLS for safe use.
||| PLAIN and LOGIN send credentials in cleartext (base64 is not encryption).
public export
requiresTLS : AuthMechanism -> Bool
requiresTLS PLAIN    = True
requiresTLS LOGIN    = True
requiresTLS CRAM_MD5 = False

||| Whether the mechanism uses challenge-response (server sends first).
public export
isChallengeResponse : AuthMechanism -> Bool
isChallengeResponse CRAM_MD5 = True
isChallengeResponse _        = False

||| The number of exchanges required to complete authentication.
public export
exchangeCount : AuthMechanism -> Nat
exchangeCount PLAIN    = 1  -- Single base64 blob
exchangeCount LOGIN    = 2  -- Username, then password
exchangeCount CRAM_MD5 = 1  -- Challenge + response in one exchange

||| List all supported mechanisms.
public export
allMechanisms : List AuthMechanism
allMechanisms = [PLAIN, LOGIN, CRAM_MD5]

-- ============================================================================
-- Authentication state machine
-- ============================================================================

||| States of an authentication exchange.
public export
data AuthState : Type where
  ||| Not yet started. Waiting for AUTH command.
  AuthIdle        : AuthState
  ||| Waiting for client credentials (PLAIN, CRAM-MD5 response).
  AwaitingCreds   : AuthMechanism -> AuthState
  ||| LOGIN: waiting for username.
  AwaitingUsername : AuthState
  ||| LOGIN: waiting for password (username already received).
  AwaitingPassword : (username : String) -> AuthState
  ||| Authentication succeeded.
  Authenticated   : (username : String) -> AuthState
  ||| Authentication failed.
  AuthFailed      : (reason : String) -> AuthState

public export
Show AuthState where
  show AuthIdle              = "Idle"
  show (AwaitingCreds m)     = "AwaitingCreds(" ++ show m ++ ")"
  show AwaitingUsername       = "AwaitingUsername"
  show (AwaitingPassword u)  = "AwaitingPassword(" ++ u ++ ")"
  show (Authenticated u)     = "Authenticated(" ++ u ++ ")"
  show (AuthFailed r)        = "AuthFailed(" ++ r ++ ")"

-- ============================================================================
-- Authentication result
-- ============================================================================

||| The outcome of an authentication attempt.
public export
data AuthResult : Type where
  ||| Authentication succeeded. Grants access.
  AuthSuccess  : (username : String) -> AuthResult
  ||| Authentication failed. Access denied.
  AuthFailure  : (reason : String) -> AuthResult
  ||| More data needed from the client (continue exchange).
  AuthContinue : (prompt : String) -> AuthResult

public export
Show AuthResult where
  show (AuthSuccess u)  = "Success(" ++ u ++ ")"
  show (AuthFailure r)  = "Failure(" ++ r ++ ")"
  show (AuthContinue p) = "Continue(" ++ p ++ ")"

||| Begin authentication with the given mechanism.
||| Returns the initial auth state and any server-side prompt.
public export
beginAuth : AuthMechanism -> (AuthState, AuthResult)
beginAuth PLAIN    = (AwaitingCreds PLAIN, AuthContinue "")
beginAuth LOGIN    = (AwaitingUsername, AuthContinue "Username:")
beginAuth CRAM_MD5 = (AwaitingCreds CRAM_MD5, AuthContinue "<challenge@server>")

||| Check if authentication is complete (succeeded or failed).
public export
isComplete : AuthState -> Bool
isComplete (Authenticated _) = True
isComplete (AuthFailed _)    = True
isComplete _                 = False

||| Check if authentication succeeded.
public export
isAuthenticated : AuthState -> Bool
isAuthenticated (Authenticated _) = True
isAuthenticated _                 = False

||| Get the authenticated username, if authentication succeeded.
public export
getUsername : AuthState -> Maybe String
getUsername (Authenticated u) = Just u
getUsername _                 = Nothing
