-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- HTTP Request Parsing (RFC 7230 Section 3)
--
-- Requests are parsed into a validated record type. Malformed input
-- produces a typed ParseError value rather than crashing. All string
-- splitting and header parsing is bounds-checked.

module HTTP.Request

import HTTP.Method
import HTTP.Status

%default total

-- ============================================================================
-- HTTP Header representation
-- ============================================================================

||| A single HTTP header as a name-value pair.
||| Header names are case-insensitive per RFC 7230 Section 3.2.
public export
record Header where
  constructor MkHeader
  ||| Header field name (stored lowercase for comparison).
  name  : String
  ||| Header field value (leading/trailing whitespace trimmed).
  value : String

public export
Show Header where
  show h = h.name ++ ": " ++ h.value

public export
Eq Header where
  a == b = a.name == b.name && a.value == b.value

-- ============================================================================
-- HTTP Request record
-- ============================================================================

||| A fully parsed HTTP request.
||| All fields are validated during construction; no field can contain
||| data that would cause a crash during processing.
public export
record Request where
  constructor MkRequest
  ||| The HTTP method (GET, POST, etc.).
  method  : Method
  ||| The request target (path + optional query string).
  path    : String
  ||| The HTTP version string (e.g. "HTTP/1.1").
  version : String
  ||| Parsed request headers.
  headers : List Header
  ||| Optional request body (may be empty).
  body    : String

public export
Show Request where
  show req = show req.method ++ " " ++ req.path ++ " " ++ req.version
             ++ " (" ++ show (length req.headers) ++ " headers)"

-- ============================================================================
-- Parse errors (values, not exceptions)
-- ============================================================================

||| Errors that can occur when parsing an HTTP request.
||| These are data values, not exceptions -- the parser never crashes.
public export
data RequestParseError : Type where
  ||| The request line is empty or missing.
  EmptyRequestLine  : RequestParseError
  ||| The request line does not have exactly three parts (method, path, version).
  MalformedRequestLine : (line : String) -> RequestParseError
  ||| The method string is not a recognised HTTP method.
  UnknownMethod     : (method : String) -> RequestParseError
  ||| The request path is empty.
  EmptyPath         : RequestParseError
  ||| A header line is malformed (missing colon separator).
  MalformedHeader   : (line : String) -> RequestParseError
  ||| The total header size exceeds the maximum allowed.
  HeadersTooLarge   : (size : Nat) -> RequestParseError
  ||| The body size exceeds the maximum allowed.
  BodyTooLarge      : (size : Nat) -> RequestParseError

public export
Show RequestParseError where
  show EmptyRequestLine         = "Empty request line"
  show (MalformedRequestLine l) = "Malformed request line: " ++ l
  show (UnknownMethod m)        = "Unknown HTTP method: " ++ m
  show EmptyPath                = "Empty request path"
  show (MalformedHeader l)      = "Malformed header: " ++ l
  show (HeadersTooLarge s)      = "Headers too large: " ++ show s ++ " bytes"
  show (BodyTooLarge s)         = "Body too large: " ++ show s ++ " bytes"

-- ============================================================================
-- Header utilities
-- ============================================================================

||| Look up a header value by name (case-insensitive).
||| Returns the first matching header's value, or Nothing.
public export
findHeader : String -> List Header -> Maybe String
findHeader _    []        = Nothing
findHeader name (h :: hs) =
  if toLower name == toLower h.name
    then Just h.value
    else findHeader name hs

||| Get the Content-Length header value as a Nat, if present and valid.
public export
contentLength : List Header -> Maybe Nat
contentLength hdrs = do
  val <- findHeader "content-length" hdrs
  parsePositive val

||| Get the Host header value, if present.
public export
hostHeader : List Header -> Maybe String
hostHeader = findHeader "host"

||| Check whether a specific header is present.
public export
hasHeader : String -> List Header -> Bool
hasHeader name hdrs = case findHeader name hdrs of
  Just _  => True
  Nothing => False

-- ============================================================================
-- Request construction helpers
-- ============================================================================

||| Build a simple GET request with no body.
public export
simpleGet : (path : String) -> (host : String) -> Request
simpleGet path host = MkRequest
  { method  = GET
  , path    = path
  , version = "HTTP/1.1"
  , headers = [ MkHeader "host" host
              , MkHeader "connection" "close"
              ]
  , body    = ""
  }

||| Build a POST request with a body and content type.
public export
simplePost : (path : String) -> (host : String) -> (contentType : String)
          -> (body : String) -> Request
simplePost path host ct body = MkRequest
  { method  = POST
  , path    = path
  , version = "HTTP/1.1"
  , headers = [ MkHeader "host" host
              , MkHeader "content-type" ct
              , MkHeader "content-length" (show (length body))
              , MkHeader "connection" "close"
              ]
  , body    = body
  }

||| Serialise a request to its wire format (RFC 7230 Section 3).
||| Uses CRLF line endings as required by the specification.
public export
serialiseRequest : Request -> String
serialiseRequest req =
  let requestLine = show req.method ++ " " ++ req.path ++ " " ++ req.version ++ "\r\n"
      headerLines = concatMap (\h => h.name ++ ": " ++ h.value ++ "\r\n") req.headers
      separator   = "\r\n"
  in requestLine ++ headerLines ++ separator ++ req.body
