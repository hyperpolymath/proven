-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- HTTP Response Construction and Serialisation (RFC 7230 Section 3.1.2)
--
-- Responses are built as validated records and serialised to wire format.
-- The serialiser is total: every valid response produces a well-formed
-- byte stream. No status code or header combination can cause a crash.

module HTTP.Response

import HTTP.Status
import HTTP.Request

%default total

-- ============================================================================
-- HTTP Response record
-- ============================================================================

||| A fully constructed HTTP response ready for serialisation.
public export
record Response where
  constructor MkResponse
  ||| The HTTP version string (e.g. "HTTP/1.1").
  version : String
  ||| The response status code.
  status  : StatusCode
  ||| Response headers.
  headers : List Header
  ||| Response body (may be empty for 204, HEAD responses, etc.).
  body    : String

public export
Show Response where
  show resp = resp.version ++ " " ++ show resp.status
              ++ " (" ++ show (length resp.headers) ++ " headers, "
              ++ show (length resp.body) ++ " bytes body)"

-- ============================================================================
-- Serialisation to wire format
-- ============================================================================

||| Serialise a response to its wire format (RFC 7230 Section 3).
||| Produces a complete HTTP response string with CRLF line endings.
||| This function is total: all status codes and headers are handled.
public export
serialiseResponse : Response -> String
serialiseResponse resp =
  let statusLine  = resp.version ++ " "
                     ++ show (statusToCode resp.status) ++ " "
                     ++ reasonPhrase resp.status ++ "\r\n"
      headerLines = concatMap (\h => h.name ++ ": " ++ h.value ++ "\r\n") resp.headers
      separator   = "\r\n"
  in statusLine ++ headerLines ++ separator ++ resp.body

||| Calculate the total byte count of the serialised response.
||| Useful for logging and monitoring without materialising the string.
public export
estimatedSize : Response -> Nat
estimatedSize resp =
  let statusLen  = length resp.version + 1
                   + length (show (statusToCode resp.status)) + 1
                   + length (reasonPhrase resp.status) + 2
      headerLen  = foldl (\acc, h => acc + length h.name + 2
                                    + length h.value + 2) 0 resp.headers
      sepLen     = 2
      bodyLen    = length resp.body
  in statusLen + headerLen + sepLen + bodyLen

-- ============================================================================
-- Response builders
-- ============================================================================

||| Build a simple response with a text body.
||| Automatically adds Content-Type and Content-Length headers.
public export
textResponse : StatusCode -> String -> Response
textResponse status body = MkResponse
  { version = "HTTP/1.1"
  , status  = status
  , headers = [ MkHeader "content-type" "text/plain; charset=utf-8"
              , MkHeader "content-length" (show (length body))
              , MkHeader "connection" "close"
              ]
  , body    = body
  }

||| Build an HTML response with appropriate content type.
public export
htmlResponse : StatusCode -> String -> Response
htmlResponse status body = MkResponse
  { version = "HTTP/1.1"
  , status  = status
  , headers = [ MkHeader "content-type" "text/html; charset=utf-8"
              , MkHeader "content-length" (show (length body))
              , MkHeader "connection" "close"
              ]
  , body    = body
  }

||| Build a JSON response with appropriate content type.
public export
jsonResponse : StatusCode -> String -> Response
jsonResponse status body = MkResponse
  { version = "HTTP/1.1"
  , status  = status
  , headers = [ MkHeader "content-type" "application/json"
              , MkHeader "content-length" (show (length body))
              , MkHeader "connection" "close"
              ]
  , body    = body
  }

||| Build a redirect response (301 or 302) with a Location header.
public export
redirectResponse : StatusCode -> (location : String) -> Response
redirectResponse status loc = MkResponse
  { version = "HTTP/1.1"
  , status  = status
  , headers = [ MkHeader "location" loc
              , MkHeader "content-length" "0"
              , MkHeader "connection" "close"
              ]
  , body    = ""
  }

||| Build a 204 No Content response (no body, no Content-Length needed).
public export
noContentResponse : Response
noContentResponse = MkResponse
  { version = "HTTP/1.1"
  , status  = NoContent
  , headers = [MkHeader "connection" "close"]
  , body    = ""
  }

||| Build a standard error response with a human-readable body.
public export
errorResponse : StatusCode -> Response
errorResponse status =
  let body = show (statusToCode status) ++ " " ++ reasonPhrase status
  in textResponse status body

||| Add a header to an existing response.
public export
addHeader : String -> String -> Response -> Response
addHeader name value resp =
  { headers $= (MkHeader name value ::) } resp

||| Set the Server header on a response.
public export
withServer : String -> Response -> Response
withServer serverName = addHeader "server" serverName
