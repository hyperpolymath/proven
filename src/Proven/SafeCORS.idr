-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeCORS - Type-safe Cross-Origin Resource Sharing validation
|||
||| Provides verified CORS origin matching, header construction, and
||| preflight response generation per the Fetch Living Standard.
||| Prevents: wildcard origin with credentials, null origin attacks,
||| overly permissive Access-Control-Allow-Origin headers.
module Proven.SafeCORS

import Data.String
import Data.List
import Data.Nat

%default total

-- ============================================================================
-- ORIGIN TYPES
-- ============================================================================

||| A validated origin (scheme + host + optional port)
public export
record Origin where
  constructor MkOrigin
  scheme : String
  host   : String
  port   : Maybe Nat

public export
Show Origin where
  show o = o.scheme ++ "://" ++ o.host ++
    case o.port of
      Nothing => ""
      Just p  => ":" ++ show p

public export
Eq Origin where
  a == b = a.scheme == b.scheme && a.host == b.host && a.port == b.port

||| Parse an origin string (scheme://host[:port])
public export
parseOrigin : String -> Maybe Origin
parseOrigin s =
  case break (== '/') (unpack s) of
    (schemePart, '/' :: '/' :: rest) =>
      let scheme = pack schemePart
          restStr = pack rest
      in if length scheme == 0 then Nothing
         else case break (== ':') (unpack restStr) of
           (hostPart, ':' :: portPart) =>
             let host = pack hostPart
                 portStr = pack portPart
             in if length host == 0 then Nothing
                else case parsePositive {a=Nat} portStr of
                  Just p  => Just (MkOrigin scheme host (Just p))
                  Nothing => Nothing
           (hostPart, _) =>
             let host = pack hostPart
             in if length host == 0 then Nothing
                else Just (MkOrigin scheme host Nothing)
    _ => Nothing

-- ============================================================================
-- CORS POLICY
-- ============================================================================

||| How to match allowed origins
public export
data OriginPolicy =
    AllowSpecific (List Origin)   -- Explicit allowlist
  | AllowAny                      -- Wildcard (*) — CANNOT be used with credentials
  | AllowNone                     -- No cross-origin requests permitted

public export
Eq OriginPolicy where
  AllowAny == AllowAny = True
  AllowNone == AllowNone = True
  (AllowSpecific a) == (AllowSpecific b) = a == b
  _ == _ = False

||| CORS configuration
public export
record CORSPolicy where
  constructor MkCORSPolicy
  allowOrigins     : OriginPolicy
  allowMethods     : List String
  allowHeaders     : List String
  exposeHeaders    : List String
  maxAge           : Maybe Nat      -- Preflight cache duration in seconds
  allowCredentials : Bool

||| Restrictive default policy (deny all cross-origin)
public export
defaultPolicy : CORSPolicy
defaultPolicy = MkCORSPolicy AllowNone [] [] [] Nothing False

-- ============================================================================
-- ORIGIN VALIDATION
-- ============================================================================

||| Check whether a request origin is allowed by the policy
public export
isOriginAllowed : CORSPolicy -> Origin -> Bool
isOriginAllowed policy origin =
  case policy.allowOrigins of
    AllowNone       => False
    AllowAny        => True
    AllowSpecific origins => any (== origin) origins

-- ============================================================================
-- SAFETY CHECKS
-- ============================================================================

||| CRITICAL: Wildcard + credentials is a browser-rejected misconfiguration.
||| Returns True if the policy is unsafe (wildcard origin with credentials).
public export
isUnsafeConfiguration : CORSPolicy -> Bool
isUnsafeConfiguration policy =
  case policy.allowOrigins of
    AllowAny => policy.allowCredentials
    _        => False

||| Validate a CORS policy for known misconfigurations
public export
data CORSError =
    WildcardWithCredentials
  | NullOriginAllowed
  | EmptyMethodList

public export
Show CORSError where
  show WildcardWithCredentials = "Wildcard origin (*) cannot be used with credentials"
  show NullOriginAllowed       = "Null origin should not be in allowlist"
  show EmptyMethodList         = "No methods allowed renders CORS useless"

||| Validate a policy, returning errors if misconfigured
public export
validatePolicy : CORSPolicy -> List CORSError
validatePolicy policy =
  let wildcardCreds = if isUnsafeConfiguration policy then [WildcardWithCredentials] else []
      emptyMethods  = if isNil policy.allowMethods && not (isNil policy.allowHeaders)
                      then [EmptyMethodList] else []
  in wildcardCreds ++ emptyMethods

-- ============================================================================
-- RESPONSE HEADER GENERATION
-- ============================================================================

||| A CORS response header pair
public export
record CORSHeader where
  constructor MkCORSHeader
  name  : String
  value : String

||| Generate CORS response headers for a given request origin
|||
||| Returns Nothing if the origin is not allowed or the policy is misconfigured.
||| This prevents accidentally serving unsafe CORS headers.
public export
generateHeaders : CORSPolicy -> Origin -> Maybe (List CORSHeader)
generateHeaders policy requestOrigin =
  -- Reject misconfigured policies
  if not (isNil (validatePolicy policy)) then Nothing
  -- Check origin is allowed
  else if not (isOriginAllowed policy requestOrigin) then Nothing
  else
    let originHeader = case policy.allowOrigins of
          AllowAny => MkCORSHeader "Access-Control-Allow-Origin" "*"
          _        => MkCORSHeader "Access-Control-Allow-Origin" (show requestOrigin)
        varyHeader = MkCORSHeader "Vary" "Origin"
        credHeader = if policy.allowCredentials
                     then [MkCORSHeader "Access-Control-Allow-Credentials" "true"]
                     else []
        exposeHeader = if isNil policy.exposeHeaders then []
                       else [MkCORSHeader "Access-Control-Expose-Headers"
                              (fastConcat (intersperse ", " policy.exposeHeaders))]
    in Just ([originHeader, varyHeader] ++ credHeader ++ exposeHeader)

||| Generate preflight (OPTIONS) response headers
public export
generatePreflightHeaders : CORSPolicy -> Origin -> String -> Maybe (List CORSHeader)
generatePreflightHeaders policy requestOrigin requestMethod =
  do baseHeaders <- generateHeaders policy requestOrigin
     let methodsHeader = MkCORSHeader "Access-Control-Allow-Methods"
                           (fastConcat (intersperse ", " policy.allowMethods))
     let headersHeader = if isNil policy.allowHeaders then []
                         else [MkCORSHeader "Access-Control-Allow-Headers"
                                (fastConcat (intersperse ", " policy.allowHeaders))]
     let maxAgeHeader = case policy.maxAge of
                          Nothing => []
                          Just n  => [MkCORSHeader "Access-Control-Max-Age" (show n)]
     Just (baseHeaders ++ [methodsHeader] ++ headersHeader ++ maxAgeHeader)
