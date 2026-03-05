-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Proxy.Types: Core protocol types for HTTP forward/reverse proxy.
--
-- Defines closed sum types for proxy modes, hop-by-hop headers that must
-- be stripped during proxying (RFC 2616 Section 13.5.1), cache control
-- directives (RFC 7234), and proxy-specific error conditions.

module Proxy.Types

%default total

-- ============================================================================
-- Proxy operating mode
-- ============================================================================

||| The two fundamental proxy operating modes.
||| Forward proxies act on behalf of clients; reverse proxies act on behalf
||| of origin servers.
public export
data ProxyMode : Type where
  ||| Forward proxy: client-facing, routes outbound requests.
  Forward : ProxyMode
  ||| Reverse proxy: server-facing, distributes inbound requests.
  Reverse : ProxyMode

public export
Eq ProxyMode where
  Forward == Forward = True
  Reverse == Reverse = True
  Forward == Reverse = False
  Reverse == Forward = False

public export
Show ProxyMode where
  show Forward = "Forward"
  show Reverse = "Reverse"

-- ============================================================================
-- Hop-by-hop headers (RFC 2616 Section 13.5.1)
-- ============================================================================

||| HTTP headers that MUST NOT be forwarded by a proxy.
||| These are hop-by-hop headers defined in RFC 2616 Section 13.5.1
||| and MUST be consumed by the first proxy that receives them.
public export
data HopByHopHeader : Type where
  ||| Connection header (RFC 2616 Section 14.10).
  Connection       : HopByHopHeader
  ||| Keep-Alive header (RFC 2068 Section 19.7.1).
  KeepAlive        : HopByHopHeader
  ||| Proxy-Authenticate header (RFC 2616 Section 14.33).
  ProxyAuth        : HopByHopHeader
  ||| Proxy-Authorization header (RFC 2616 Section 14.34).
  ProxyAuthz       : HopByHopHeader
  ||| TE header (RFC 2616 Section 14.39).
  TE               : HopByHopHeader
  ||| Trailers header (RFC 2616 Section 14.40).
  Trailers         : HopByHopHeader
  ||| Transfer-Encoding header (RFC 2616 Section 14.41).
  TransferEncoding : HopByHopHeader
  ||| Upgrade header (RFC 2616 Section 14.42).
  Upgrade          : HopByHopHeader

public export
Eq HopByHopHeader where
  Connection       == Connection       = True
  KeepAlive        == KeepAlive        = True
  ProxyAuth        == ProxyAuth        = True
  ProxyAuthz       == ProxyAuthz       = True
  TE               == TE               = True
  Trailers         == Trailers         = True
  TransferEncoding == TransferEncoding = True
  Upgrade          == Upgrade          = True
  _                == _                = False

public export
Show HopByHopHeader where
  show Connection       = "Connection"
  show KeepAlive        = "Keep-Alive"
  show ProxyAuth        = "Proxy-Authenticate"
  show ProxyAuthz       = "Proxy-Authorization"
  show TE               = "TE"
  show Trailers         = "Trailers"
  show TransferEncoding = "Transfer-Encoding"
  show Upgrade          = "Upgrade"

-- ============================================================================
-- Cache directives (RFC 7234)
-- ============================================================================

||| Cache-Control directives relevant to proxy caching (RFC 7234).
public export
data CacheDirective : Type where
  ||| Response must not be served from cache without revalidation.
  NoCache          : CacheDirective
  ||| Response must not be stored in any cache.
  NoStore          : CacheDirective
  ||| Maximum age in seconds before the response is considered stale.
  MaxAge           : CacheDirective
  ||| Response may be cached by any cache (shared or private).
  Public           : CacheDirective
  ||| Response is intended for a single user and must not be shared.
  Private          : CacheDirective
  ||| Cache must revalidate stale responses before serving.
  MustRevalidate   : CacheDirective

public export
Eq CacheDirective where
  NoCache        == NoCache        = True
  NoStore        == NoStore        = True
  MaxAge         == MaxAge         = True
  Public         == Public         = True
  Private        == Private        = True
  MustRevalidate == MustRevalidate = True
  _              == _              = False

public export
Show CacheDirective where
  show NoCache        = "no-cache"
  show NoStore        = "no-store"
  show MaxAge         = "max-age"
  show Public         = "public"
  show Private        = "private"
  show MustRevalidate = "must-revalidate"

-- ============================================================================
-- Proxy error conditions
-- ============================================================================

||| Error conditions specific to proxy operation.
||| These map to HTTP 5xx status codes returned when the proxy cannot
||| fulfil a request due to upstream failures.
public export
data ProxyError : Type where
  ||| Upstream server returned an invalid response (HTTP 502).
  BadGateway      : ProxyError
  ||| Upstream server did not respond in time (HTTP 504).
  GatewayTimeout  : ProxyError
  ||| Upstream server actively refused the connection.
  UpstreamRefused : ProxyError
  ||| TLS handshake with upstream server failed.
  UpstreamTLS     : ProxyError

public export
Eq ProxyError where
  BadGateway      == BadGateway      = True
  GatewayTimeout  == GatewayTimeout  = True
  UpstreamRefused == UpstreamRefused = True
  UpstreamTLS     == UpstreamTLS     = True
  _               == _               = False

public export
Show ProxyError where
  show BadGateway      = "BadGateway"
  show GatewayTimeout  = "GatewayTimeout"
  show UpstreamRefused = "UpstreamRefused"
  show UpstreamTLS     = "UpstreamTLS"

-- ============================================================================
-- Enumerations of all constructors (useful for testing/display)
-- ============================================================================

||| All proxy modes.
public export
allProxyModes : List ProxyMode
allProxyModes = [Forward, Reverse]

||| All hop-by-hop headers.
public export
allHopByHopHeaders : List HopByHopHeader
allHopByHopHeaders = [Connection, KeepAlive, ProxyAuth, ProxyAuthz,
                      TE, Trailers, TransferEncoding, Upgrade]

||| All cache directives.
public export
allCacheDirectives : List CacheDirective
allCacheDirectives = [NoCache, NoStore, MaxAge, Public, Private, MustRevalidate]

||| All proxy errors.
public export
allProxyErrors : List ProxyError
allProxyErrors = [BadGateway, GatewayTimeout, UpstreamRefused, UpstreamTLS]
