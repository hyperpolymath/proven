-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeCSRF - Cross-Site Request Forgery prevention
|||
||| Provides type-safe CSRF token generation, validation, and
||| double-submit cookie pattern support per OWASP guidelines.
||| Prevents: session riding, cross-origin state changes, replay attacks.
module Proven.SafeCSRF

import Data.String
import Data.List
import Data.Nat
import Data.So

%default total

-- ============================================================================
-- TOKEN TYPES
-- ============================================================================

||| Minimum token entropy (bytes). OWASP recommends >= 128 bits = 16 bytes.
public export
MinTokenBytes : Nat
MinTokenBytes = 16

||| A validated CSRF token (non-empty, sufficient entropy)
public export
record CSRFToken where
  constructor MkCSRFToken
  tokenValue : String
  0 lengthOk : So (length tokenValue >= MinTokenBytes * 2)

||| Attempt to wrap a raw string as a CSRF token (hex-encoded, >= 32 chars)
public export
mkToken : String -> Maybe CSRFToken
mkToken s with (choose (length s >= MinTokenBytes * 2))
  mkToken s | Left prf  = Just (MkCSRFToken s prf)
  mkToken s | Right _   = Nothing

||| Extract the raw token value
public export
tokenString : CSRFToken -> String
tokenString = tokenValue

-- ============================================================================
-- VALIDATION
-- ============================================================================

||| Constant-time string comparison to prevent timing attacks.
||| Compares every character even after a mismatch.
public export
constantTimeEqual : String -> String -> Bool
constantTimeEqual a b =
  let as = unpack a
      bs = unpack b
  in if length as /= length bs then False
     else go as bs True
  where
    go : List Char -> List Char -> Bool -> Bool
    go [] [] acc = acc
    go (x :: xs) (y :: ys) acc = go xs ys (acc && x == y)
    go _ _ _ = False

||| Validate a submitted token against the expected token.
||| Uses constant-time comparison to prevent timing side-channels.
public export
validateToken : CSRFToken -> String -> Bool
validateToken expected submitted =
  constantTimeEqual (tokenString expected) submitted

-- ============================================================================
-- DOUBLE-SUBMIT COOKIE PATTERN
-- ============================================================================

||| A double-submit pair: cookie value + form/header value
public export
record DoubleSubmit where
  constructor MkDoubleSubmit
  cookieValue : String
  headerValue : String

||| Validate a double-submit pair (cookie must match header, constant-time)
public export
validateDoubleSubmit : DoubleSubmit -> Bool
validateDoubleSubmit ds = constantTimeEqual ds.cookieValue ds.headerValue

-- ============================================================================
-- SYNCHRONIZER TOKEN PATTERN
-- ============================================================================

||| Session-bound CSRF token storage
public export
record SessionCSRF where
  constructor MkSessionCSRF
  sessionId : String
  token     : CSRFToken

||| Validate a token submission against a session-bound token
public export
validateSessionToken : SessionCSRF -> String -> String -> Bool
validateSessionToken session submittedSessionId submittedToken =
  constantTimeEqual session.sessionId submittedSessionId &&
  validateToken session.token submittedToken

-- ============================================================================
-- TOKEN PROPERTIES
-- ============================================================================

||| Check if a token has sufficient entropy (character diversity)
public export
hasSufficientEntropy : String -> Bool
hasSufficientEntropy s =
  let chars = unpack s
      hasLower = any isLower chars
      hasUpper = any isUpper chars
      hasDigit = any isDigit chars
      classCount : Nat = (if hasLower then 1 else 0) +
                         (if hasUpper then 1 else 0) +
                         (if hasDigit then 1 else 0)
  in classCount >= 2 && length s >= MinTokenBytes * 2

||| Check if a string contains only valid hex characters
public export
isHexToken : String -> Bool
isHexToken s = all isHexChar (unpack s)
  where
    isHexChar : Char -> Bool
    isHexChar c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

-- ============================================================================
-- ORIGIN VALIDATION (defence in depth)
-- ============================================================================

||| Validate the Origin header matches expected origins
public export
validateOrigin : List String -> String -> Bool
validateOrigin allowedOrigins requestOrigin =
  any (\allowed => constantTimeEqual allowed requestOrigin) allowedOrigins

||| Validate the Referer header starts with an allowed origin
public export
validateReferer : List String -> String -> Bool
validateReferer allowedOrigins referer =
  any (\origin => isPrefixOf origin referer) allowedOrigins

||| Combined CSRF validation: token + origin check
public export
fullValidation : CSRFToken -> String -> List String -> String -> Bool
fullValidation token submittedToken allowedOrigins requestOrigin =
  validateToken token submittedToken && validateOrigin allowedOrigins requestOrigin
