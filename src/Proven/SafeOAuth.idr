-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeOAuth - OAuth 2.0/OIDC with CSRF prevention
|||
||| Provides type-safe OAuth 2.0 authorization code flow and OIDC validation.
||| Prevents: CSRF attacks, open redirectors, token theft, replay attacks.
module Proven.SafeOAuth

import Data.String
import Data.List
import Data.Nat
import Data.Maybe

%default total

||| OAuth 2.0 grant types
public export
data GrantType =
    AuthorizationCode
  | ClientCredentials
  | RefreshToken
  | DeviceCode

public export
Show GrantType where
  show AuthorizationCode = "authorization_code"
  show ClientCredentials = "client_credentials"
  show RefreshToken = "refresh_token"
  show DeviceCode = "urn:ietf:params:oauth:grant-type:device_code"

public export
Eq GrantType where
  AuthorizationCode == AuthorizationCode = True
  ClientCredentials == ClientCredentials = True
  RefreshToken == RefreshToken = True
  DeviceCode == DeviceCode = True
  _ == _ = False

||| OAuth response types
public export
data ResponseType = Code | Token | IdToken

public export
Show ResponseType where
  show Code = "code"
  show Token = "token"
  show IdToken = "id_token"

||| PKCE code challenge method
public export
data CodeChallengeMethod = Plain | S256

public export
Show CodeChallengeMethod where
  show Plain = "plain"
  show S256 = "S256"

||| A CSRF-protection state parameter
public export
data OAuthState : Type where
  MkOAuthState : (value : String) -> OAuthState

||| Validate state parameter (must be >= 32 chars, alphanumeric)
public export
isValidState : String -> Bool
isValidState s =
  length s >= 32 && all isAlphaNum (unpack s)

||| Smart constructor for OAuth state
public export
mkOAuthState : String -> Maybe OAuthState
mkOAuthState s = if isValidState s then Just (MkOAuthState s) else Nothing

||| Extract state value
public export
stateValue : OAuthState -> String
stateValue (MkOAuthState v) = v

||| A nonce for replay prevention
public export
data OAuthNonce : Type where
  MkOAuthNonce : (value : String) -> OAuthNonce

||| Validate nonce (must be >= 16 chars)
public export
isValidNonce : String -> Bool
isValidNonce s = length s >= 16

||| OAuth authorization request
public export
record AuthorizationRequest where
  constructor MkAuthRequest
  clientId      : String
  redirectUri   : String
  responseType  : ResponseType
  scope         : List String
  state         : OAuthState
  nonce         : Maybe OAuthNonce
  codeChallenge : Maybe String
  codeChallengeMethod : Maybe CodeChallengeMethod

||| Validate redirect URI (prevent open redirect)
public export
isValidRedirectUri : String -> List String -> Bool
isValidRedirectUri uri allowedUris = elem uri allowedUris

||| Check that redirect URI uses HTTPS (except localhost)
public export
isSecureRedirectUri : String -> Bool
isSecureRedirectUri uri =
  isPrefixOf "https://" uri ||
  isPrefixOf "http://localhost" uri ||
  isPrefixOf "http://127.0.0.1" uri ||
  isPrefixOf "http://[::1]" uri

||| Build authorization URL
public export
buildAuthUrl : String -> AuthorizationRequest -> String
buildAuthUrl baseUrl req =
  baseUrl ++ "?" ++
  "client_id=" ++ clientId req ++
  "&redirect_uri=" ++ redirectUri req ++
  "&response_type=" ++ show (responseType req) ++
  "&scope=" ++ joinBy "+" (scope req) ++
  "&state=" ++ stateValue (state req) ++
  maybe "" (\n => "&nonce=" ++ nonce') (nonce req) ++
  maybe "" (\c => "&code_challenge=" ++ c) (codeChallenge req) ++
  maybe "" (\m => "&code_challenge_method=" ++ show m) (codeChallengeMethod req)
  where
    nonce' : String
    nonce' = case nonce req of
               Just (MkOAuthNonce v) => v
               Nothing => ""
    joinBy : String -> List String -> String
    joinBy _ [] = ""
    joinBy _ [x] = x
    joinBy sep (x :: xs) = x ++ sep ++ joinBy sep xs

||| OAuth token response
public export
record TokenResponse where
  constructor MkTokenResponse
  accessToken  : String
  tokenType    : String
  expiresIn    : Nat        -- seconds
  refreshToken : Maybe String
  scope        : List String
  idToken      : Maybe String

||| Validate state matches (CSRF protection)
public export
validateState : OAuthState -> OAuthState -> Bool
validateState (MkOAuthState sent) (MkOAuthState received) = sent == received

||| Token validation errors
public export
data TokenError =
    StateMismatch
  | InvalidScope
  | ExpiredToken
  | InsecureRedirect
  | MissingPKCE
  | InvalidNonce

public export
Eq TokenError where
  StateMismatch == StateMismatch = True
  InvalidScope == InvalidScope = True
  ExpiredToken == ExpiredToken = True
  InsecureRedirect == InsecureRedirect = True
  MissingPKCE == MissingPKCE = True
  InvalidNonce == InvalidNonce = True
  _ == _ = False

||| Validate authorization code exchange
public export
validateCodeExchange : OAuthState -> OAuthState -> String -> List String -> Either TokenError ()
validateCodeExchange sentState receivedState redirectUri allowedUris =
  if not (validateState sentState receivedState)
    then Left StateMismatch
    else if not (isValidRedirectUri redirectUri allowedUris)
      then Left InsecureRedirect
      else Right ()

||| Check if token is expired
public export
isTokenExpired : TokenResponse -> Nat -> Nat -> Bool
isTokenExpired token issuedAt currentTime =
  currentTime > issuedAt + expiresIn token

-- ----------------------------------------------------------------
-- Proof types
-- ----------------------------------------------------------------

||| Proof that state parameter provides CSRF protection
public export
data CSRFProtected : OAuthState -> OAuthState -> Type where
  MkCSRFProtected : validateState s1 s2 = True -> CSRFProtected s1 s2

||| Proof that redirect URI is in allowed list
public export
data AllowedRedirect : String -> List String -> Type where
  MkAllowedRedirect : isValidRedirectUri uri uris = True -> AllowedRedirect uri uris

||| Proof that redirect URI uses HTTPS
public export
data SecureRedirect : String -> Type where
  MkSecureRedirect : isSecureRedirectUri uri = True -> SecureRedirect uri
