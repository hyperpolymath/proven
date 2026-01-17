-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeHTTP - Safe HTTP request/response handling
|||
||| This module provides safe construction and parsing of HTTP
||| messages with validation of methods, status codes, and headers.
module Proven.SafeHTTP

import public Proven.Core
import Proven.SafeUrl
import Proven.SafeHeader
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- HTTP Methods
--------------------------------------------------------------------------------

||| Standard HTTP methods
public export
data Method
  = GET
  | POST
  | PUT
  | DELETE
  | PATCH
  | HEAD
  | OPTIONS
  | TRACE
  | CONNECT
  | Custom String  -- For extensions

public export
Eq Method where
  GET == GET = True
  POST == POST = True
  PUT == PUT = True
  DELETE == DELETE = True
  PATCH == PATCH = True
  HEAD == HEAD = True
  OPTIONS == OPTIONS = True
  TRACE == TRACE = True
  CONNECT == CONNECT = True
  Custom a == Custom b = a == b
  _ == _ = False

public export
Show Method where
  show GET = "GET"
  show POST = "POST"
  show PUT = "PUT"
  show DELETE = "DELETE"
  show PATCH = "PATCH"
  show HEAD = "HEAD"
  show OPTIONS = "OPTIONS"
  show TRACE = "TRACE"
  show CONNECT = "CONNECT"
  show (Custom m) = m

||| Parse method from string
public export
parseMethod : String -> Method
parseMethod "GET" = GET
parseMethod "POST" = POST
parseMethod "PUT" = PUT
parseMethod "DELETE" = DELETE
parseMethod "PATCH" = PATCH
parseMethod "HEAD" = HEAD
parseMethod "OPTIONS" = OPTIONS
parseMethod "TRACE" = TRACE
parseMethod "CONNECT" = CONNECT
parseMethod m = Custom m

||| Check if method is safe (no side effects)
public export
isSafe : Method -> Bool
isSafe GET = True
isSafe HEAD = True
isSafe OPTIONS = True
isSafe TRACE = True
isSafe _ = False

||| Check if method is idempotent
public export
isIdempotent : Method -> Bool
isIdempotent GET = True
isIdempotent HEAD = True
isIdempotent PUT = True
isIdempotent DELETE = True
isIdempotent OPTIONS = True
isIdempotent TRACE = True
isIdempotent _ = False

||| Check if method can have a body
public export
canHaveBody : Method -> Bool
canHaveBody GET = False
canHaveBody HEAD = False
canHaveBody OPTIONS = False
canHaveBody TRACE = False
canHaveBody _ = True

--------------------------------------------------------------------------------
-- HTTP Version
--------------------------------------------------------------------------------

||| HTTP protocol version
public export
data HttpVersion
  = HTTP10  -- HTTP/1.0
  | HTTP11  -- HTTP/1.1
  | HTTP2   -- HTTP/2
  | HTTP3   -- HTTP/3

public export
Show HttpVersion where
  show HTTP10 = "HTTP/1.0"
  show HTTP11 = "HTTP/1.1"
  show HTTP2 = "HTTP/2"
  show HTTP3 = "HTTP/3"

||| Parse HTTP version
public export
parseVersion : String -> Maybe HttpVersion
parseVersion "HTTP/1.0" = Just HTTP10
parseVersion "HTTP/1.1" = Just HTTP11
parseVersion "HTTP/2" = Just HTTP2
parseVersion "HTTP/2.0" = Just HTTP2
parseVersion "HTTP/3" = Just HTTP3
parseVersion _ = Nothing

--------------------------------------------------------------------------------
-- Status Codes
--------------------------------------------------------------------------------

||| HTTP status code categories
public export
data StatusCategory
  = Informational  -- 1xx
  | Success        -- 2xx
  | Redirection    -- 3xx
  | ClientError    -- 4xx
  | ServerError    -- 5xx

||| Get category from status code
public export
statusCategory : Nat -> Maybe StatusCategory
statusCategory n =
  if n >= 100 && n < 200 then Just Informational
  else if n >= 200 && n < 300 then Just Success
  else if n >= 300 && n < 400 then Just Redirection
  else if n >= 400 && n < 500 then Just ClientError
  else if n >= 500 && n < 600 then Just ServerError
  else Nothing

||| Common status codes with reason phrases
public export
statusReason : Nat -> String
statusReason 100 = "Continue"
statusReason 101 = "Switching Protocols"
statusReason 200 = "OK"
statusReason 201 = "Created"
statusReason 202 = "Accepted"
statusReason 204 = "No Content"
statusReason 206 = "Partial Content"
statusReason 301 = "Moved Permanently"
statusReason 302 = "Found"
statusReason 303 = "See Other"
statusReason 304 = "Not Modified"
statusReason 307 = "Temporary Redirect"
statusReason 308 = "Permanent Redirect"
statusReason 400 = "Bad Request"
statusReason 401 = "Unauthorized"
statusReason 403 = "Forbidden"
statusReason 404 = "Not Found"
statusReason 405 = "Method Not Allowed"
statusReason 406 = "Not Acceptable"
statusReason 408 = "Request Timeout"
statusReason 409 = "Conflict"
statusReason 410 = "Gone"
statusReason 411 = "Length Required"
statusReason 413 = "Payload Too Large"
statusReason 414 = "URI Too Long"
statusReason 415 = "Unsupported Media Type"
statusReason 416 = "Range Not Satisfiable"
statusReason 422 = "Unprocessable Entity"
statusReason 429 = "Too Many Requests"
statusReason 500 = "Internal Server Error"
statusReason 501 = "Not Implemented"
statusReason 502 = "Bad Gateway"
statusReason 503 = "Service Unavailable"
statusReason 504 = "Gateway Timeout"
statusReason _ = "Unknown"

||| Check if status indicates success
public export
isSuccess : Nat -> Bool
isSuccess n = n >= 200 && n < 300

||| Check if status indicates redirect
public export
isRedirect : Nat -> Bool
isRedirect n = n >= 300 && n < 400

||| Check if status indicates error
public export
isError : Nat -> Bool
isError n = n >= 400

--------------------------------------------------------------------------------
-- HTTP Headers (using list of pairs)
--------------------------------------------------------------------------------

||| HTTP headers as a list of name-value pairs
public export
Headers : Type
Headers = List (String, String)

||| Empty headers
public export
emptyHeaders : Headers
emptyHeaders = []

||| Add a header (allows duplicates)
public export
addHeader : String -> String -> Headers -> Headers
addHeader name value headers = (name, value) :: headers

||| Set a header (replaces existing)
public export
setHeader : String -> String -> Headers -> Headers
setHeader name value headers =
  (name, value) :: filter (\(n, _) => toLower n /= toLower name) headers

||| Get first header value by name (case-insensitive)
public export
getHeader : String -> Headers -> Maybe String
getHeader name headers =
  case find (\(n, _) => toLower n == toLower name) headers of
    Just (_, v) => Just v
    Nothing => Nothing

||| Get all header values by name (for headers that can repeat)
public export
getHeaders : String -> Headers -> List String
getHeaders name = map snd . filter (\(n, _) => toLower n == toLower name)

||| Remove all headers with name
public export
removeHeader : String -> Headers -> Headers
removeHeader name = filter (\(n, _) => toLower n /= toLower name)

||| Check if header exists
public export
hasHeader : String -> Headers -> Bool
hasHeader name headers = isJust (getHeader name headers)

--------------------------------------------------------------------------------
-- HTTP Request
--------------------------------------------------------------------------------

||| HTTP request
public export
record Request where
  constructor MkRequest
  method : Method
  path : String
  version : HttpVersion
  headers : Headers
  body : Maybe String

||| Create a simple GET request
public export
get : String -> Request
get path = MkRequest GET path HTTP11 [] Nothing

||| Create a POST request with body
public export
post : String -> String -> Request
post path body = MkRequest POST path HTTP11 [] (Just body)

||| Create a PUT request with body
public export
put : String -> String -> Request
put path body = MkRequest PUT path HTTP11 [] (Just body)

||| Create a DELETE request
public export
delete : String -> Request
delete path = MkRequest DELETE path HTTP11 [] Nothing

||| Add header to request
public export
withHeader : String -> String -> Request -> Request
withHeader name value req =
  MkRequest req.method req.path req.version
            (addHeader name value req.headers) req.body

||| Set request body
public export
withBody : String -> Request -> Request
withBody body req =
  MkRequest req.method req.path req.version req.headers (Just body)

||| Render request to string
public export
renderRequest : Request -> String
renderRequest req =
  let line = show req.method ++ " " ++ req.path ++ " " ++ show req.version
      hdrs = map (\(n, v) => n ++ ": " ++ v) req.headers
      bodyPart = case req.body of
                   Nothing => ""
                   Just b => "\r\n" ++ b
  in unlines (line :: hdrs) ++ bodyPart

--------------------------------------------------------------------------------
-- HTTP Response
--------------------------------------------------------------------------------

||| HTTP response
public export
record Response where
  constructor MkResponse
  version : HttpVersion
  status : Nat
  reason : String
  headers : Headers
  body : Maybe String

||| Create a response
public export
response : Nat -> Response
response status =
  MkResponse HTTP11 status (statusReason status) [] Nothing

||| Create an OK response
public export
ok : Response
ok = response 200

||| Create a 201 Created response
public export
created : Response
created = response 201

||| Create a 204 No Content response
public export
noContent : Response
noContent = response 204

||| Create a 400 Bad Request response
public export
badRequest : Response
badRequest = response 400

||| Create a 404 Not Found response
public export
notFound : Response
notFound = response 404

||| Create a 500 Internal Server Error response
public export
internalError : Response
internalError = response 500

||| Add header to response
public export
withHeaderR : String -> String -> Response -> Response
withHeaderR name value resp =
  MkResponse resp.version resp.status resp.reason
             (addHeader name value resp.headers) resp.body

||| Set response body
public export
withBodyR : String -> Response -> Response
withBodyR body resp =
  MkResponse resp.version resp.status resp.reason resp.headers (Just body)

||| Render response to string
public export
renderResponse : Response -> String
renderResponse resp =
  let line = show resp.version ++ " " ++ show resp.status ++ " " ++ resp.reason
      hdrs = map (\(n, v) => n ++ ": " ++ v) resp.headers
      bodyPart = case resp.body of
                   Nothing => ""
                   Just b => "\r\n" ++ b
  in unlines (line :: hdrs) ++ bodyPart

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| HTTP errors
public export
data HttpError
  = InvalidMethod String
  | InvalidVersion String
  | InvalidStatusCode Nat
  | MissingHostHeader
  | InvalidContentLength String
  | HeaderTooLong Nat
  | BodyTooLarge Nat

public export
Show HttpError where
  show (InvalidMethod m) = "Invalid HTTP method: " ++ m
  show (InvalidVersion v) = "Invalid HTTP version: " ++ v
  show (InvalidStatusCode c) = "Invalid status code: " ++ show c
  show MissingHostHeader = "Missing required Host header"
  show (InvalidContentLength s) = "Invalid Content-Length: " ++ s
  show (HeaderTooLong n) = "Header exceeds maximum length: " ++ show n
  show (BodyTooLarge n) = "Body exceeds maximum size: " ++ show n

||| Validate a request
public export
validateRequest : Request -> Either HttpError ()
validateRequest req =
  if req.version == HTTP11 && not (hasHeader "Host" req.headers)
    then Left MissingHostHeader
    else Right ()

||| Validate a response
public export
validateResponse : Response -> Either HttpError ()
validateResponse resp =
  case statusCategory resp.status of
    Nothing => Left (InvalidStatusCode resp.status)
    Just _ => Right ()

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Show Request where
  show req = show req.method ++ " " ++ req.path

public export
Show Response where
  show resp = show resp.status ++ " " ++ resp.reason
