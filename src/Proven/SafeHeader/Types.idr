-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Safe HTTP header types and constraints
|||
||| This module defines types for safe HTTP header handling including:
||| - Header name validation
||| - Header value validation
||| - Common header types
||| - Security header presets
module Proven.SafeHeader.Types

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Header Name Types
--------------------------------------------------------------------------------

||| Maximum header name length (RFC 7230 recommends reasonable limits)
public export
maxNameLength : Nat
maxNameLength = 256

||| Maximum header value length
public export
maxValueLength : Nat
maxValueLength = 8192

||| Maximum total headers size
public export
maxTotalSize : Nat
maxTotalSize = 65536

||| Validated header name (token per RFC 7230)
public export
record HeaderName where
  constructor MkHeaderName
  ||| The header name (lowercase canonical form)
  name : String
  ||| Original case (for display)
  originalCase : String
  ||| Proof name is bounded
  0 bounded : length (unpack name) <= maxNameLength = True

public export
Eq HeaderName where
  h1 == h2 = h1.name == h2.name

public export
Show HeaderName where
  show h = h.originalCase

||| Validated header value
public export
record HeaderValue where
  constructor MkHeaderValue
  ||| The header value (trimmed)
  value : String
  ||| Proof value is bounded
  0 bounded : length (unpack value) <= maxValueLength = True

public export
Eq HeaderValue where
  v1 == v2 = v1.value == v2.value

public export
Show HeaderValue where
  show v = v.value

--------------------------------------------------------------------------------
-- Header Entry
--------------------------------------------------------------------------------

||| A single header entry
public export
record Header where
  constructor MkHeader
  ||| Header name
  name : HeaderName
  ||| Header value
  value : HeaderValue

public export
Eq Header where
  h1 == h2 = h1.name == h2.name && h1.value == h2.value

public export
Show Header where
  show h = show h.name ++ ": " ++ show h.value

||| Collection of headers
public export
Headers : Type
Headers = List Header

--------------------------------------------------------------------------------
-- Header Category
--------------------------------------------------------------------------------

||| HTTP header category
public export
data HeaderCategory : Type where
  ||| General headers (Connection, Date, etc.)
  General : HeaderCategory
  ||| Request headers (Accept, Host, etc.)
  Request : HeaderCategory
  ||| Response headers (Server, Set-Cookie, etc.)
  Response : HeaderCategory
  ||| Entity headers (Content-Type, Content-Length, etc.)
  Entity : HeaderCategory
  ||| Security headers (CSP, HSTS, etc.)
  Security : HeaderCategory
  ||| Caching headers (Cache-Control, ETag, etc.)
  Caching : HeaderCategory
  ||| Custom/Extension headers
  Custom : HeaderCategory

public export
Show HeaderCategory where
  show General = "general"
  show Request = "request"
  show Response = "response"
  show Entity = "entity"
  show Security = "security"
  show Caching = "caching"
  show Custom = "custom"

public export
Eq HeaderCategory where
  General == General = True
  Request == Request = True
  Response == Response = True
  Entity == Entity = True
  Security == Security = True
  Caching == Caching = True
  Custom == Custom = True
  _ == _ = False

--------------------------------------------------------------------------------
-- Well-Known Headers
--------------------------------------------------------------------------------

||| Well-known header name
public export
data WellKnownHeader : Type where
  -- General
  HdrConnection : WellKnownHeader
  HdrDate : WellKnownHeader
  HdrTransferEncoding : WellKnownHeader
  HdrUpgrade : WellKnownHeader
  HdrVia : WellKnownHeader
  -- Request
  HdrAccept : WellKnownHeader
  HdrAcceptCharset : WellKnownHeader
  HdrAcceptEncoding : WellKnownHeader
  HdrAcceptLanguage : WellKnownHeader
  HdrAuthorization : WellKnownHeader
  HdrCookie : WellKnownHeader
  HdrHost : WellKnownHeader
  HdrIfMatch : WellKnownHeader
  HdrIfModifiedSince : WellKnownHeader
  HdrIfNoneMatch : WellKnownHeader
  HdrOrigin : WellKnownHeader
  HdrReferer : WellKnownHeader
  HdrUserAgent : WellKnownHeader
  -- Response
  HdrAge : WellKnownHeader
  HdrAllow : WellKnownHeader
  HdrLocation : WellKnownHeader
  HdrRetryAfter : WellKnownHeader
  HdrServer : WellKnownHeader
  HdrSetCookie : WellKnownHeader
  HdrWWWAuthenticate : WellKnownHeader
  -- Entity
  HdrContentDisposition : WellKnownHeader
  HdrContentEncoding : WellKnownHeader
  HdrContentLanguage : WellKnownHeader
  HdrContentLength : WellKnownHeader
  HdrContentLocation : WellKnownHeader
  HdrContentRange : WellKnownHeader
  HdrContentType : WellKnownHeader
  HdrExpires : WellKnownHeader
  HdrLastModified : WellKnownHeader
  -- Caching
  HdrCacheControl : WellKnownHeader
  HdrETag : WellKnownHeader
  HdrPragma : WellKnownHeader
  HdrVary : WellKnownHeader
  -- Security
  HdrContentSecurityPolicy : WellKnownHeader
  HdrStrictTransportSecurity : WellKnownHeader
  HdrXContentTypeOptions : WellKnownHeader
  HdrXFrameOptions : WellKnownHeader
  HdrXXSSProtection : WellKnownHeader
  HdrReferrerPolicy : WellKnownHeader
  HdrPermissionsPolicy : WellKnownHeader
  -- CORS
  HdrAccessControlAllowOrigin : WellKnownHeader
  HdrAccessControlAllowMethods : WellKnownHeader
  HdrAccessControlAllowHeaders : WellKnownHeader
  HdrAccessControlExposeHeaders : WellKnownHeader
  HdrAccessControlMaxAge : WellKnownHeader
  HdrAccessControlAllowCredentials : WellKnownHeader

public export
Show WellKnownHeader where
  show HdrConnection = "Connection"
  show HdrDate = "Date"
  show HdrTransferEncoding = "Transfer-Encoding"
  show HdrUpgrade = "Upgrade"
  show HdrVia = "Via"
  show HdrAccept = "Accept"
  show HdrAcceptCharset = "Accept-Charset"
  show HdrAcceptEncoding = "Accept-Encoding"
  show HdrAcceptLanguage = "Accept-Language"
  show HdrAuthorization = "Authorization"
  show HdrCookie = "Cookie"
  show HdrHost = "Host"
  show HdrIfMatch = "If-Match"
  show HdrIfModifiedSince = "If-Modified-Since"
  show HdrIfNoneMatch = "If-None-Match"
  show HdrOrigin = "Origin"
  show HdrReferer = "Referer"
  show HdrUserAgent = "User-Agent"
  show HdrAge = "Age"
  show HdrAllow = "Allow"
  show HdrLocation = "Location"
  show HdrRetryAfter = "Retry-After"
  show HdrServer = "Server"
  show HdrSetCookie = "Set-Cookie"
  show HdrWWWAuthenticate = "WWW-Authenticate"
  show HdrContentDisposition = "Content-Disposition"
  show HdrContentEncoding = "Content-Encoding"
  show HdrContentLanguage = "Content-Language"
  show HdrContentLength = "Content-Length"
  show HdrContentLocation = "Content-Location"
  show HdrContentRange = "Content-Range"
  show HdrContentType = "Content-Type"
  show HdrExpires = "Expires"
  show HdrLastModified = "Last-Modified"
  show HdrCacheControl = "Cache-Control"
  show HdrETag = "ETag"
  show HdrPragma = "Pragma"
  show HdrVary = "Vary"
  show HdrContentSecurityPolicy = "Content-Security-Policy"
  show HdrStrictTransportSecurity = "Strict-Transport-Security"
  show HdrXContentTypeOptions = "X-Content-Type-Options"
  show HdrXFrameOptions = "X-Frame-Options"
  show HdrXXSSProtection = "X-XSS-Protection"
  show HdrReferrerPolicy = "Referrer-Policy"
  show HdrPermissionsPolicy = "Permissions-Policy"
  show HdrAccessControlAllowOrigin = "Access-Control-Allow-Origin"
  show HdrAccessControlAllowMethods = "Access-Control-Allow-Methods"
  show HdrAccessControlAllowHeaders = "Access-Control-Allow-Headers"
  show HdrAccessControlExposeHeaders = "Access-Control-Expose-Headers"
  show HdrAccessControlMaxAge = "Access-Control-Max-Age"
  show HdrAccessControlAllowCredentials = "Access-Control-Allow-Credentials"

||| Get lowercase name for well-known header
public export
wellKnownName : WellKnownHeader -> String
wellKnownName h = toLower (show h)

||| Get category for well-known header
public export
headerCategory : WellKnownHeader -> HeaderCategory
headerCategory HdrConnection = General
headerCategory HdrDate = General
headerCategory HdrTransferEncoding = General
headerCategory HdrUpgrade = General
headerCategory HdrVia = General
headerCategory HdrAccept = Request
headerCategory HdrAcceptCharset = Request
headerCategory HdrAcceptEncoding = Request
headerCategory HdrAcceptLanguage = Request
headerCategory HdrAuthorization = Request
headerCategory HdrCookie = Request
headerCategory HdrHost = Request
headerCategory HdrIfMatch = Request
headerCategory HdrIfModifiedSince = Request
headerCategory HdrIfNoneMatch = Request
headerCategory HdrOrigin = Request
headerCategory HdrReferer = Request
headerCategory HdrUserAgent = Request
headerCategory HdrAge = Response
headerCategory HdrAllow = Response
headerCategory HdrLocation = Response
headerCategory HdrRetryAfter = Response
headerCategory HdrServer = Response
headerCategory HdrSetCookie = Response
headerCategory HdrWWWAuthenticate = Response
headerCategory HdrContentDisposition = Entity
headerCategory HdrContentEncoding = Entity
headerCategory HdrContentLanguage = Entity
headerCategory HdrContentLength = Entity
headerCategory HdrContentLocation = Entity
headerCategory HdrContentRange = Entity
headerCategory HdrContentType = Entity
headerCategory HdrExpires = Entity
headerCategory HdrLastModified = Entity
headerCategory HdrCacheControl = Caching
headerCategory HdrETag = Caching
headerCategory HdrPragma = Caching
headerCategory HdrVary = Caching
headerCategory HdrContentSecurityPolicy = Security
headerCategory HdrStrictTransportSecurity = Security
headerCategory HdrXContentTypeOptions = Security
headerCategory HdrXFrameOptions = Security
headerCategory HdrXXSSProtection = Security
headerCategory HdrReferrerPolicy = Security
headerCategory HdrPermissionsPolicy = Security
headerCategory HdrAccessControlAllowOrigin = Security
headerCategory HdrAccessControlAllowMethods = Security
headerCategory HdrAccessControlAllowHeaders = Security
headerCategory HdrAccessControlExposeHeaders = Security
headerCategory HdrAccessControlMaxAge = Security
headerCategory HdrAccessControlAllowCredentials = Security

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

||| Header parsing/validation errors
public export
data HeaderError : Type where
  ||| Header name too long
  NameTooLong : (name : String) -> (len : Nat) -> HeaderError
  ||| Header value too long
  ValueTooLong : (name : String) -> (len : Nat) -> HeaderError
  ||| Invalid header name character
  InvalidNameChar : (name : String) -> (char : Char) -> HeaderError
  ||| Invalid header value character
  InvalidValueChar : (name : String) -> (char : Char) -> HeaderError
  ||| Empty header name
  EmptyName : HeaderError
  ||| Header injection attempt (CRLF)
  HeaderInjection : (name : String) -> (value : String) -> HeaderError
  ||| Total headers too large
  TotalSizeTooLarge : (size : Nat) -> HeaderError
  ||| Duplicate header (when not allowed)
  DuplicateHeader : (name : String) -> HeaderError
  ||| Missing required header
  MissingHeader : (name : String) -> HeaderError
  ||| Invalid header value format
  InvalidValueFormat : (name : String) -> (value : String) -> (reason : String) -> HeaderError

public export
Show HeaderError where
  show (NameTooLong name len) =
    "Header name too long: " ++ show len ++ " chars (max " ++ show maxNameLength ++ ")"
  show (ValueTooLong name len) =
    "Header value too long for " ++ name ++ ": " ++ show len ++ " chars"
  show (InvalidNameChar name char) =
    "Invalid character '" ++ singleton char ++ "' in header name: " ++ name
  show (InvalidValueChar name char) =
    "Invalid character in header value for " ++ name
  show EmptyName =
    "Empty header name"
  show (HeaderInjection name value) =
    "Header injection attempt detected in " ++ name
  show (TotalSizeTooLarge size) =
    "Total headers size too large: " ++ show size ++ " bytes"
  show (DuplicateHeader name) =
    "Duplicate header: " ++ name
  show (MissingHeader name) =
    "Missing required header: " ++ name
  show (InvalidValueFormat name value reason) =
    "Invalid value for " ++ name ++ ": " ++ reason

public export
Eq HeaderError where
  NameTooLong n1 l1 == NameTooLong n2 l2 = n1 == n2 && l1 == l2
  ValueTooLong n1 l1 == ValueTooLong n2 l2 = n1 == n2 && l1 == l2
  InvalidNameChar n1 c1 == InvalidNameChar n2 c2 = n1 == n2 && c1 == c2
  InvalidValueChar n1 c1 == InvalidValueChar n2 c2 = n1 == n2 && c1 == c2
  EmptyName == EmptyName = True
  HeaderInjection n1 v1 == HeaderInjection n2 v2 = n1 == n2 && v1 == v2
  TotalSizeTooLarge s1 == TotalSizeTooLarge s2 = s1 == s2
  DuplicateHeader n1 == DuplicateHeader n2 = n1 == n2
  MissingHeader n1 == MissingHeader n2 = n1 == n2
  InvalidValueFormat n1 v1 r1 == InvalidValueFormat n2 v2 r2 = n1 == n2 && v1 == v2 && r1 == r2
  _ == _ = False

||| Header result type
public export
HeaderResult : Type -> Type
HeaderResult = Result HeaderError

--------------------------------------------------------------------------------
-- Validation Options
--------------------------------------------------------------------------------

||| Header validation options
public export
record HeaderOptions where
  constructor MkHeaderOptions
  ||| Maximum header name length
  maxNameLen : Nat
  ||| Maximum header value length
  maxValueLen : Nat
  ||| Maximum total size
  maxTotalLen : Nat
  ||| Allow duplicate headers
  allowDuplicates : Bool
  ||| Allow empty values
  allowEmptyValues : Bool
  ||| Strict token validation
  strictTokens : Bool
  ||| Block dangerous headers (e.g., Proxy-*)
  blockDangerous : Bool

||| Default header options
public export
defaultOptions : HeaderOptions
defaultOptions = MkHeaderOptions
  { maxNameLen = maxNameLength
  , maxValueLen = maxValueLength
  , maxTotalLen = maxTotalSize
  , allowDuplicates = True
  , allowEmptyValues = True
  , strictTokens = False
  , blockDangerous = True
  }

||| Strict header options
public export
strictOptions : HeaderOptions
strictOptions = MkHeaderOptions
  { maxNameLen = 128
  , maxValueLen = 4096
  , maxTotalLen = 32768
  , allowDuplicates = False
  , allowEmptyValues = False
  , strictTokens = True
  , blockDangerous = True
  }

--------------------------------------------------------------------------------
-- Dangerous Headers
--------------------------------------------------------------------------------

||| Headers that should be blocked in user input
public export
dangerousHeaders : List String
dangerousHeaders =
  [ "proxy-authorization"
  , "proxy-authenticate"
  , "proxy-connection"
  , "transfer-encoding"
  , "content-length"  -- Should be set by framework
  , "host"            -- Should be set by framework
  , "connection"
  , "keep-alive"
  , "upgrade"
  , "te"
  , "trailer"
  ]

||| Check if header name is dangerous
public export
isDangerousHeader : String -> Bool
isDangerousHeader name = toLower name `elem` dangerousHeaders

--------------------------------------------------------------------------------
-- Token Characters (RFC 7230)
--------------------------------------------------------------------------------

||| Valid token characters per RFC 7230
||| token = 1*tchar
||| tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." /
|||         "^" / "_" / "`" / "|" / "~" / DIGIT / ALPHA
public export
isTokenChar : Char -> Bool
isTokenChar c =
  isAlphaNum c ||
  c == '!' || c == '#' || c == '$' || c == '%' || c == '&' ||
  c == '\'' || c == '*' || c == '+' || c == '-' || c == '.' ||
  c == '^' || c == '_' || c == '`' || c == '|' || c == '~'

||| Check if string is valid token
public export
isValidToken : String -> Bool
isValidToken s = not (null (unpack s)) && all isTokenChar (unpack s)

||| Check for CRLF injection
public export
hasCRLF : String -> Bool
hasCRLF s = isInfixOf "\r" s || isInfixOf "\n" s

