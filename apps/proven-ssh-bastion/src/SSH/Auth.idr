-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- SSH Authentication Layer (RFC 4252)
--
-- Defines authentication method types, request/response structures,
-- and result types.  All authentication outcomes are represented as
-- typed values — authentication failures are NOT exceptions.

module SSH.Auth

%default total

-- ============================================================================
-- Authentication Methods (RFC 4252 Section 5-8)
-- ============================================================================

||| Supported SSH authentication methods.
||| Each constructor maps to a method name in the SSH protocol.
public export
data AuthMethod : Type where
  ||| "publickey" — asymmetric key verification (RFC 4252 Section 7)
  PublicKey            : AuthMethod
  ||| "password" — plaintext password (RFC 4252 Section 8)
  Password             : AuthMethod
  ||| "keyboard-interactive" — challenge-response (RFC 4256)
  KeyboardInteractive  : AuthMethod
  ||| "none" — request available methods (RFC 4252 Section 5.2)
  AuthNone             : AuthMethod

public export
Eq AuthMethod where
  PublicKey           == PublicKey           = True
  Password            == Password            = True
  KeyboardInteractive == KeyboardInteractive = True
  AuthNone            == AuthNone            = True
  _                   == _                   = False

public export
Show AuthMethod where
  show PublicKey           = "publickey"
  show Password            = "password"
  show KeyboardInteractive = "keyboard-interactive"
  show AuthNone            = "none"

||| Parse a method name string to an AuthMethod.
||| Returns Nothing for unrecognised method names — no crash.
public export
authMethodFromString : String -> Maybe AuthMethod
authMethodFromString "publickey"             = Just PublicKey
authMethodFromString "password"              = Just Password
authMethodFromString "keyboard-interactive"  = Just KeyboardInteractive
authMethodFromString "none"                  = Just AuthNone
authMethodFromString _                       = Nothing

-- ============================================================================
-- Public Key Types
-- ============================================================================

||| Public key algorithm types used during publickey authentication.
public export
data PubKeyType : Type where
  ||| ssh-ed25519 — EdDSA on Curve25519
  Ed25519Key  : PubKeyType
  ||| rsa-sha2-256 — RSA with SHA-256
  RSA256Key   : PubKeyType
  ||| rsa-sha2-512 — RSA with SHA-512
  RSA512Key   : PubKeyType
  ||| ecdsa-sha2-nistp256 — ECDSA on NIST P-256
  ECDSA256Key : PubKeyType

public export
Eq PubKeyType where
  Ed25519Key  == Ed25519Key  = True
  RSA256Key   == RSA256Key   = True
  RSA512Key   == RSA512Key   = True
  ECDSA256Key == ECDSA256Key = True
  _           == _           = False

public export
Show PubKeyType where
  show Ed25519Key  = "ssh-ed25519"
  show RSA256Key   = "rsa-sha2-256"
  show RSA512Key   = "rsa-sha2-512"
  show ECDSA256Key = "ecdsa-sha2-nistp256"

-- ============================================================================
-- Authentication Request (RFC 4252 Section 5)
-- ============================================================================

||| An authentication request from the client.
||| The request carries method-specific data as a tagged union.
public export
data AuthRequest : Type where
  ||| Public key authentication attempt.
  ||| Carries the key type, key blob, and optional signature.
  PubKeyRequest  : (username : String)
                 -> (keyType : PubKeyType)
                 -> (keyBlob : List Bits8)
                 -> (signature : Maybe (List Bits8))
                 -> AuthRequest
  ||| Password authentication attempt.
  PasswordRequest : (username : String)
                  -> (password : String)
                  -> AuthRequest
  ||| Keyboard-interactive initial request (no responses yet).
  KBInteractiveRequest : (username : String)
                       -> (submethods : List String)
                       -> AuthRequest
  ||| "none" method — queries available methods.
  NoneRequest : (username : String) -> AuthRequest

||| Extract the username from any authentication request.
public export
requestUsername : AuthRequest -> String
requestUsername (PubKeyRequest u _ _ _)     = u
requestUsername (PasswordRequest u _)       = u
requestUsername (KBInteractiveRequest u _)  = u
requestUsername (NoneRequest u)             = u

||| Extract the method used in an authentication request.
public export
requestMethod : AuthRequest -> AuthMethod
requestMethod (PubKeyRequest _ _ _ _)    = PublicKey
requestMethod (PasswordRequest _ _)      = Password
requestMethod (KBInteractiveRequest _ _) = KeyboardInteractive
requestMethod (NoneRequest _)            = AuthNone

-- ============================================================================
-- Authentication Result (RFC 4252 Section 5.1)
-- ============================================================================

||| Result of an authentication attempt.
||| SSH allows partial success (multi-factor) — the remaining methods
||| list tells the client what else it must do.
public export
data AuthResult : Type where
  ||| SSH_MSG_USERAUTH_SUCCESS — user is authenticated
  AuthSuccess       : AuthResult
  ||| SSH_MSG_USERAUTH_FAILURE with partial_success=false
  ||| Carries the list of methods that can continue.
  AuthFailure       : (remainingMethods : List AuthMethod)
                    -> AuthResult
  ||| SSH_MSG_USERAUTH_FAILURE with partial_success=true
  ||| One factor succeeded; more are required.
  AuthPartialSuccess : (remainingMethods : List AuthMethod)
                     -> AuthResult

public export
Eq AuthResult where
  AuthSuccess             == AuthSuccess             = True
  (AuthFailure ms1)       == (AuthFailure ms2)       = ms1 == ms2
  (AuthPartialSuccess r1) == (AuthPartialSuccess r2) = r1 == r2
  _                       == _                       = False

public export
Show AuthResult where
  show AuthSuccess              = "SUCCESS"
  show (AuthFailure ms)         = "FAILURE(can_continue=" ++ show ms ++ ")"
  show (AuthPartialSuccess ms)  = "PARTIAL_SUCCESS(remaining=" ++ show ms ++ ")"

-- ============================================================================
-- Authentication state tracking
-- ============================================================================

||| Track the number of failed authentication attempts for a user.
||| The bastion enforces a maximum to prevent brute-force attacks.
public export
record AuthAttempts where
  constructor MkAuthAttempts
  ||| Username being authenticated
  username     : String
  ||| Number of failed attempts so far
  failedCount  : Nat
  ||| Maximum allowed failures before disconnect
  maxFailures  : Nat
  ||| Methods already tried and failed
  triedMethods : List AuthMethod

||| Create a fresh attempt tracker for a user.
public export
newAuthAttempts : (username : String) -> (maxFailures : Nat) -> AuthAttempts
newAuthAttempts user maxF = MkAuthAttempts
  { username     = user
  , failedCount  = 0
  , maxFailures  = maxF
  , triedMethods = []
  }

||| Record a failed authentication attempt.
||| Returns the updated tracker.
public export
recordFailure : AuthMethod -> AuthAttempts -> AuthAttempts
recordFailure method attempts =
  { failedCount  $= (+ 1)
  , triedMethods $= (method ::)
  } attempts

||| Check whether the user has exceeded the maximum failure count.
public export
isLockedOut : AuthAttempts -> Bool
isLockedOut a = a.failedCount >= a.maxFailures

||| Authentication errors that can occur at the protocol level.
public export
data AuthError : Type where
  ||| Too many failed attempts — connection should be terminated
  TooManyFailures   : (username : String) -> (count : Nat) -> AuthError
  ||| The requested method is not allowed by the bastion config
  MethodNotAllowed  : AuthMethod -> AuthError
  ||| Username exceeds maximum length
  UsernameTooLong   : (length : Nat) -> AuthError
  ||| Password exceeds maximum length
  PasswordTooLong   : (length : Nat) -> AuthError

public export
Show AuthError where
  show (TooManyFailures u n) = "Too many failures for " ++ u ++ " (" ++ show n ++ ")"
  show (MethodNotAllowed m)  = "Method not allowed: " ++ show m
  show (UsernameTooLong n)   = "Username too long: " ++ show n ++ " bytes"
  show (PasswordTooLong n)   = "Password too long: " ++ show n ++ " bytes"
