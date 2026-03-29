-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeHTTP - Type-safe HTTP primitives
|||
||| Provides verified HTTP method handling, status code classification,
||| safe header construction, and request/response validation.
||| Prevents: header injection, method confusion, status code misuse.
module Proven.SafeHTTP

import Data.String
import Data.List
import Data.Nat
import Data.So

%default total

-- ============================================================================
-- HTTP METHODS
-- ============================================================================

||| Standard HTTP methods (RFC 9110)
public export
data HTTPMethod =
    GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH

public export
Show HTTPMethod where
  show GET     = "GET"
  show HEAD    = "HEAD"
  show POST    = "POST"
  show PUT     = "PUT"
  show DELETE  = "DELETE"
  show CONNECT = "CONNECT"
  show OPTIONS = "OPTIONS"
  show TRACE   = "TRACE"
  show PATCH   = "PATCH"

public export
Eq HTTPMethod where
  GET == GET = True
  HEAD == HEAD = True
  POST == POST = True
  PUT == PUT = True
  DELETE == DELETE = True
  CONNECT == CONNECT = True
  OPTIONS == OPTIONS = True
  TRACE == TRACE = True
  PATCH == PATCH = True
  _ == _ = False

||| Parse an HTTP method from a string (case-sensitive per RFC 9110)
public export
parseMethod : String -> Maybe HTTPMethod
parseMethod "GET"     = Just GET
parseMethod "HEAD"    = Just HEAD
parseMethod "POST"    = Just POST
parseMethod "PUT"     = Just PUT
parseMethod "DELETE"  = Just DELETE
parseMethod "CONNECT" = Just CONNECT
parseMethod "OPTIONS" = Just OPTIONS
parseMethod "TRACE"   = Just TRACE
parseMethod "PATCH"   = Just PATCH
parseMethod _         = Nothing

||| Whether the method is safe (does not modify server state)
public export
isSafe : HTTPMethod -> Bool
isSafe GET     = True
isSafe HEAD    = True
isSafe OPTIONS = True
isSafe TRACE   = True
isSafe _       = False

||| Whether the method is idempotent
public export
isIdempotent : HTTPMethod -> Bool
isIdempotent GET     = True
isIdempotent HEAD    = True
isIdempotent PUT     = True
isIdempotent DELETE  = True
isIdempotent OPTIONS = True
isIdempotent TRACE   = True
isIdempotent _       = False

||| Whether the method typically carries a request body
public export
hasRequestBody : HTTPMethod -> Bool
hasRequestBody POST  = True
hasRequestBody PUT   = True
hasRequestBody PATCH = True
hasRequestBody _     = False

-- ============================================================================
-- HTTP STATUS CODES
-- ============================================================================

||| HTTP status code with range proof (100-599 per RFC 9110)
public export
record StatusCode where
  constructor MkStatusCode
  code : Nat
  0 inRange : So (code >= 100 && code <= 599)

||| Status code classification
public export
data StatusClass =
    Informational  -- 1xx
  | Successful     -- 2xx
  | Redirection    -- 3xx
  | ClientError    -- 4xx
  | ServerError    -- 5xx

public export
Show StatusClass where
  show Informational = "1xx Informational"
  show Successful    = "2xx Successful"
  show Redirection   = "3xx Redirection"
  show ClientError   = "4xx Client Error"
  show ServerError   = "5xx Server Error"

public export
Eq StatusClass where
  Informational == Informational = True
  Successful == Successful = True
  Redirection == Redirection = True
  ClientError == ClientError = True
  ServerError == ServerError = True
  _ == _ = False

||| Classify a status code
public export
classify : StatusCode -> StatusClass
classify sc =
  if sc.code < 200 then Informational
  else if sc.code < 300 then Successful
  else if sc.code < 400 then Redirection
  else if sc.code < 500 then ClientError
  else ServerError

||| Range proof postulate for status code construction
statusCodeInRange : (n : Nat) -> {auto 0 _ : So (n >= 100)} -> {auto 0 _ : So (n <= 599)} -> So (n >= 100 && n <= 599)

||| Attempt to create a status code from a Nat
public export
mkStatusCode : Nat -> Maybe StatusCode
mkStatusCode n with (choose (n >= 100 && n <= 599))
  mkStatusCode n | Left prf = Just (MkStatusCode n prf)
  mkStatusCode n | Right _ = Nothing

||| Whether the status indicates success
public export
isSuccess : StatusCode -> Bool
isSuccess sc = sc.code >= 200 && sc.code < 300

||| Whether the status indicates a redirect
public export
isRedirect : StatusCode -> Bool
isRedirect sc = sc.code >= 300 && sc.code < 400

||| Whether the status indicates an error
public export
isError : StatusCode -> Bool
isError sc = sc.code >= 400

||| Whether the status indicates a retryable error (503, 429, 502, 504)
public export
isRetryable : StatusCode -> Bool
isRetryable sc = sc.code == 429 || sc.code == 502 || sc.code == 503 || sc.code == 504

-- ============================================================================
-- HTTP HEADERS (injection-safe)
-- ============================================================================

||| A validated header name (no CR/LF/NUL, ASCII printable, no colon)
public export
record HeaderName where
  constructor MkHeaderName
  name : String

||| A validated header value (no CR/LF/NUL)
public export
record HeaderValue where
  constructor MkHeaderValue
  value : String

||| Check if a character is valid in a header name (token chars per RFC 9110)
||| Token = 1*tchar, tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+"
|||   / "-" / "." / "^" / "_" / "`" / "|" / "~" / DIGIT / ALPHA
isTokenChar : Char -> Bool
isTokenChar c =
  let n = ord c in
  (n >= 0x30 && n <= 0x39)  ||  -- DIGIT
  (n >= 0x41 && n <= 0x5A)  ||  -- A-Z
  (n >= 0x61 && n <= 0x7A)  ||  -- a-z
  c == '!' || c == '#' || c == '$' || c == '%' || c == '&' ||
  c == '\'' || c == '*' || c == '+' || c == '-' || c == '.' ||
  c == '^' || c == '_' || c == '`' || c == '|' || c == '~'

||| Check if a character is valid in a header value (no CR, LF, NUL)
isHeaderValueChar : Char -> Bool
isHeaderValueChar c =
  let n = ord c in
  n /= 0x00 && n /= 0x0A && n /= 0x0D

||| Validate and create a header name (prevents header injection)
public export
mkHeaderName : String -> Maybe HeaderName
mkHeaderName s =
  if length s == 0 then Nothing
  else if all isTokenChar (unpack s) then Just (MkHeaderName s)
  else Nothing

||| Validate and create a header value (prevents header injection via CR/LF)
public export
mkHeaderValue : String -> Maybe HeaderValue
mkHeaderValue s =
  if all isHeaderValueChar (unpack s) then Just (MkHeaderValue s)
  else Nothing

||| A validated HTTP header pair
public export
record HTTPHeader where
  constructor MkHTTPHeader
  headerName : HeaderName
  headerValue : HeaderValue

||| Create a validated header from raw strings
public export
mkHeader : String -> String -> Maybe HTTPHeader
mkHeader name value = do
  n <- mkHeaderName name
  v <- mkHeaderValue value
  Just (MkHTTPHeader n v)

||| Render a header to wire format
public export
renderHeader : HTTPHeader -> String
renderHeader h = h.headerName.name ++ ": " ++ h.headerValue.value

-- ============================================================================
-- COMMON HEADERS (type-safe constructors)
-- ============================================================================

||| Content-Type header
public export
contentType : String -> Maybe HTTPHeader
contentType = mkHeader "Content-Type"

||| Authorization: Bearer token header
public export
bearerAuth : String -> Maybe HTTPHeader
bearerAuth token =
  if length token == 0 then Nothing
  else mkHeader "Authorization" ("Bearer " ++ token)

||| Cache-Control header
public export
cacheControl : String -> Maybe HTTPHeader
cacheControl = mkHeader "Cache-Control"

-- ============================================================================
-- HTTP VERSION
-- ============================================================================

||| Supported HTTP protocol versions
public export
data HTTPVersion = HTTP10 | HTTP11 | HTTP2 | HTTP3

public export
Show HTTPVersion where
  show HTTP10 = "HTTP/1.0"
  show HTTP11 = "HTTP/1.1"
  show HTTP2  = "HTTP/2"
  show HTTP3  = "HTTP/3"

public export
Eq HTTPVersion where
  HTTP10 == HTTP10 = True
  HTTP11 == HTTP11 = True
  HTTP2 == HTTP2 = True
  HTTP3 == HTTP3 = True
  _ == _ = False

||| Parse HTTP version string
public export
parseVersion : String -> Maybe HTTPVersion
parseVersion "HTTP/1.0" = Just HTTP10
parseVersion "HTTP/1.1" = Just HTTP11
parseVersion "HTTP/2"   = Just HTTP2
parseVersion "HTTP/3"   = Just HTTP3
parseVersion _          = Nothing
