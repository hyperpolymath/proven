-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeOAuth operations
|||
||| This module exports OAuth 2.0/OIDC safety operations to the C ABI
||| via Idris2's RefC backend.
|||
||| Return conventions:
||| - State validation -> (status: Int, value: String)
||| - Bool checks -> Int (0 = false, 1 = true)
module Proven.FFI.SafeOAuth

import Proven.SafeOAuth

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

--------------------------------------------------------------------------------
-- State Parameter (CSRF Protection)
--------------------------------------------------------------------------------

export
proven_idris_oauth_is_valid_state : String -> Int
proven_idris_oauth_is_valid_state = encodeBool . isValidState

export
proven_idris_oauth_mk_state : String -> (Int, String)
proven_idris_oauth_mk_state s = case mkOAuthState s of
  Nothing => (1, "State must be >= 32 alphanumeric characters")
  Just (MkOAuthState v) => (0, v)

export
proven_idris_oauth_validate_state : String -> String -> Int
proven_idris_oauth_validate_state sent received =
  case (mkOAuthState sent, mkOAuthState received) of
    (Just s, Just r) => encodeBool (validateState s r)
    _ => 0  -- Invalid states don't match

--------------------------------------------------------------------------------
-- Nonce Validation
--------------------------------------------------------------------------------

export
proven_idris_oauth_is_valid_nonce : String -> Int
proven_idris_oauth_is_valid_nonce = encodeBool . isValidNonce

--------------------------------------------------------------------------------
-- Redirect URI Validation
--------------------------------------------------------------------------------

export
proven_idris_oauth_is_secure_redirect_uri : String -> Int
proven_idris_oauth_is_secure_redirect_uri = encodeBool . isSecureRedirectUri

export
proven_idris_oauth_is_valid_redirect_uri : String -> String -> Int
proven_idris_oauth_is_valid_redirect_uri uri allowedCsv =
  let allowed = split (== ',') allowedCsv
  in encodeBool (isValidRedirectUri uri allowed)

--------------------------------------------------------------------------------
-- Token Expiry
--------------------------------------------------------------------------------

export
proven_idris_oauth_is_token_expired : Int -> Int -> Int -> Int
proven_idris_oauth_is_token_expired expiresIn issuedAt currentTime =
  encodeBool (cast currentTime > cast issuedAt + cast expiresIn)

--------------------------------------------------------------------------------
-- Grant Type Info
--------------------------------------------------------------------------------

export
proven_idris_oauth_grant_type_name : Int -> String
proven_idris_oauth_grant_type_name 0 = show AuthorizationCode
proven_idris_oauth_grant_type_name 1 = show ClientCredentials
proven_idris_oauth_grant_type_name 2 = show RefreshToken
proven_idris_oauth_grant_type_name 3 = show DeviceCode
proven_idris_oauth_grant_type_name _ = "unknown"

--------------------------------------------------------------------------------
-- Code Exchange Validation
--------------------------------------------------------------------------------

export
proven_idris_oauth_validate_code_exchange : String -> String -> String -> String -> (Int, String)
proven_idris_oauth_validate_code_exchange sentState receivedState redirectUri allowedCsv =
  case (mkOAuthState sentState, mkOAuthState receivedState) of
    (Just s, Just r) =>
      let allowed = split (== ',') allowedCsv
      in case validateCodeExchange s r redirectUri allowed of
        Left StateMismatch => (1, "State mismatch (CSRF)")
        Left InsecureRedirect => (1, "Insecure redirect URI")
        Left _ => (1, "Validation error")
        Right () => (0, "OK")
    _ => (1, "Invalid state parameters")
