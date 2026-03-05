-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-apiserver: Core protocol types for API gateway server.
--
-- All types are closed sum types with total Show instances.
-- No parsers, no full implementations -- skeleton only.

module Apiserver.Types

%default total

-- ============================================================================
-- AuthScheme
-- ============================================================================

||| Authentication schemes supported by the API gateway.
public export
data AuthScheme : Type where
  ||| Static API key passed in a header or query parameter.
  APIKey : AuthScheme
  ||| Bearer token (JWT or opaque).
  Bearer : AuthScheme
  ||| HTTP Basic authentication.
  Basic  : AuthScheme
  ||| OAuth 2.0 with token introspection.
  OAuth2 : AuthScheme
  ||| HMAC-signed request (e.g. AWS Signature V4).
  HMAC   : AuthScheme
  ||| Mutual TLS with client certificate validation.
  MTLS   : AuthScheme

export
Show AuthScheme where
  show APIKey = "APIKey"
  show Bearer = "Bearer"
  show Basic  = "Basic"
  show OAuth2 = "OAuth2"
  show HMAC   = "HMAC"
  show MTLS   = "mTLS"

-- ============================================================================
-- RateLimitStrategy
-- ============================================================================

||| Rate limiting algorithms available for API endpoints.
public export
data RateLimitStrategy : Type where
  ||| Fixed time window with a hard request cap.
  FixedWindow   : RateLimitStrategy
  ||| Sliding time window for smoother rate enforcement.
  SlidingWindow : RateLimitStrategy
  ||| Token bucket: bursts allowed up to bucket capacity.
  TokenBucket   : RateLimitStrategy
  ||| Leaky bucket: constant drain rate, queues excess.
  LeakyBucket   : RateLimitStrategy

export
Show RateLimitStrategy where
  show FixedWindow   = "FixedWindow"
  show SlidingWindow = "SlidingWindow"
  show TokenBucket   = "TokenBucket"
  show LeakyBucket   = "LeakyBucket"

-- ============================================================================
-- APIVersion
-- ============================================================================

||| API version identifiers for routing requests to the correct backend.
public export
data APIVersion : Type where
  ||| Version 1 -- initial stable API.
  V1         : APIVersion
  ||| Version 2 -- breaking changes from V1.
  V2         : APIVersion
  ||| Version 3 -- current latest.
  V3         : APIVersion
  ||| Alias for the most recent stable version.
  Latest     : APIVersion
  ||| Deprecated version still served but not recommended.
  Deprecated : APIVersion

export
Show APIVersion where
  show V1         = "V1"
  show V2         = "V2"
  show V3         = "V3"
  show Latest     = "Latest"
  show Deprecated = "Deprecated"

-- ============================================================================
-- ResponseFormat
-- ============================================================================

||| Wire formats supported for API responses.
public export
data ResponseFormat : Type where
  ||| JSON (RFC 8259).
  JSON        : ResponseFormat
  ||| XML.
  XML         : ResponseFormat
  ||| Protocol Buffers (binary).
  Protobuf    : ResponseFormat
  ||| MessagePack (binary).
  MessagePack : ResponseFormat

export
Show ResponseFormat where
  show JSON        = "JSON"
  show XML         = "XML"
  show Protobuf    = "Protobuf"
  show MessagePack = "MessagePack"

-- ============================================================================
-- GatewayError
-- ============================================================================

||| Error codes returned by the API gateway.
public export
data GatewayError : Type where
  ||| Request lacks valid authentication credentials.
  Unauthorized      : GatewayError
  ||| Client has exceeded their rate limit.
  RateLimited       : GatewayError
  ||| Requested endpoint does not exist.
  NotFound          : GatewayError
  ||| Request payload is malformed or invalid.
  BadRequest        : GatewayError
  ||| Backend service is unavailable.
  ServiceUnavailable : GatewayError
  ||| Circuit breaker is open for the target backend.
  CircuitOpen       : GatewayError

export
Show GatewayError where
  show Unauthorized       = "Unauthorized"
  show RateLimited        = "RateLimited"
  show NotFound           = "NotFound"
  show BadRequest         = "BadRequest"
  show ServiceUnavailable = "ServiceUnavailable"
  show CircuitOpen        = "CircuitOpen"
