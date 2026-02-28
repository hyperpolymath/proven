-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- HTTP Status Codes (RFC 7231 Section 6)
--
-- Status codes are represented as a sum type with category classification.
-- Each code carries its numeric value and reason phrase at the type level,
-- ensuring that only valid status codes can be constructed.

module HTTP.Status

%default total

-- ============================================================================
-- Status code categories (RFC 7231 Section 6)
-- ============================================================================

||| HTTP response status code categories.
||| Categorises codes by their first digit per RFC 7231 Section 6.
public export
data StatusCategory : Type where
  ||| 1xx: Request received, continuing process.
  Informational   : StatusCategory
  ||| 2xx: Request successfully received, understood, and accepted.
  Success         : StatusCategory
  ||| 3xx: Further action needs to be taken to complete the request.
  Redirect        : StatusCategory
  ||| 4xx: Request contains bad syntax or cannot be fulfilled.
  ClientError     : StatusCategory
  ||| 5xx: Server failed to fulfil an apparently valid request.
  ServerError     : StatusCategory

public export
Eq StatusCategory where
  Informational == Informational = True
  Success       == Success       = True
  Redirect      == Redirect      = True
  ClientError   == ClientError   = True
  ServerError   == ServerError   = True
  _             == _             = False

public export
Show StatusCategory where
  show Informational = "Informational"
  show Success       = "Success"
  show Redirect      = "Redirect"
  show ClientError   = "ClientError"
  show ServerError   = "ServerError"

-- ============================================================================
-- Status codes
-- ============================================================================

||| Common HTTP status codes as a closed sum type.
||| Only well-known codes from RFC 7231 and related RFCs are included.
public export
data StatusCode : Type where
  -- 1xx Informational
  Continue           : StatusCode  -- 100
  SwitchingProtocols : StatusCode  -- 101
  -- 2xx Success
  OK                 : StatusCode  -- 200
  Created            : StatusCode  -- 201
  Accepted           : StatusCode  -- 202
  NoContent          : StatusCode  -- 204
  -- 3xx Redirection
  MovedPermanently   : StatusCode  -- 301
  Found              : StatusCode  -- 302
  NotModified        : StatusCode  -- 304
  TemporaryRedirect  : StatusCode  -- 307
  PermanentRedirect  : StatusCode  -- 308
  -- 4xx Client Error
  BadRequest         : StatusCode  -- 400
  Unauthorized       : StatusCode  -- 401
  Forbidden          : StatusCode  -- 403
  NotFound           : StatusCode  -- 404
  MethodNotAllowed   : StatusCode  -- 405
  RequestTimeout     : StatusCode  -- 408
  Conflict           : StatusCode  -- 409
  Gone               : StatusCode  -- 410
  LengthRequired     : StatusCode  -- 411
  PayloadTooLarge    : StatusCode  -- 413
  URITooLong         : StatusCode  -- 414
  UnsupportedMedia   : StatusCode  -- 415
  TooManyRequests    : StatusCode  -- 429
  -- 5xx Server Error
  InternalError      : StatusCode  -- 500
  NotImplemented     : StatusCode  -- 501
  BadGateway         : StatusCode  -- 502
  ServiceUnavailable : StatusCode  -- 503
  GatewayTimeout     : StatusCode  -- 504

public export
Eq StatusCode where
  Continue           == Continue           = True
  SwitchingProtocols == SwitchingProtocols = True
  OK                 == OK                 = True
  Created            == Created            = True
  Accepted           == Accepted           = True
  NoContent          == NoContent          = True
  MovedPermanently   == MovedPermanently   = True
  Found              == Found              = True
  NotModified        == NotModified        = True
  TemporaryRedirect  == TemporaryRedirect  = True
  PermanentRedirect  == PermanentRedirect  = True
  BadRequest         == BadRequest         = True
  Unauthorized       == Unauthorized       = True
  Forbidden          == Forbidden          = True
  NotFound           == NotFound           = True
  MethodNotAllowed   == MethodNotAllowed   = True
  RequestTimeout     == RequestTimeout     = True
  Conflict           == Conflict           = True
  Gone               == Gone               = True
  LengthRequired     == LengthRequired     = True
  PayloadTooLarge    == PayloadTooLarge    = True
  URITooLong         == URITooLong         = True
  UnsupportedMedia   == UnsupportedMedia   = True
  TooManyRequests    == TooManyRequests    = True
  InternalError      == InternalError      = True
  NotImplemented     == NotImplemented     = True
  BadGateway         == BadGateway         = True
  ServiceUnavailable == ServiceUnavailable = True
  GatewayTimeout     == GatewayTimeout     = True
  _                  == _                  = False

-- ============================================================================
-- Numeric codes and reason phrases
-- ============================================================================

||| Convert a status code to its numeric value (Nat for safety, no overflow).
public export
statusToCode : StatusCode -> Nat
statusToCode Continue           = 100
statusToCode SwitchingProtocols = 101
statusToCode OK                 = 200
statusToCode Created            = 201
statusToCode Accepted           = 202
statusToCode NoContent          = 204
statusToCode MovedPermanently   = 301
statusToCode Found              = 302
statusToCode NotModified        = 304
statusToCode TemporaryRedirect  = 307
statusToCode PermanentRedirect  = 308
statusToCode BadRequest         = 400
statusToCode Unauthorized       = 401
statusToCode Forbidden          = 403
statusToCode NotFound           = 404
statusToCode MethodNotAllowed   = 405
statusToCode RequestTimeout     = 408
statusToCode Conflict           = 409
statusToCode Gone               = 410
statusToCode LengthRequired     = 411
statusToCode PayloadTooLarge    = 413
statusToCode URITooLong         = 414
statusToCode UnsupportedMedia   = 415
statusToCode TooManyRequests    = 429
statusToCode InternalError      = 500
statusToCode NotImplemented     = 501
statusToCode BadGateway         = 502
statusToCode ServiceUnavailable = 503
statusToCode GatewayTimeout     = 504

||| Standard reason phrase for a status code (RFC 7231).
public export
reasonPhrase : StatusCode -> String
reasonPhrase Continue           = "Continue"
reasonPhrase SwitchingProtocols = "Switching Protocols"
reasonPhrase OK                 = "OK"
reasonPhrase Created            = "Created"
reasonPhrase Accepted           = "Accepted"
reasonPhrase NoContent          = "No Content"
reasonPhrase MovedPermanently   = "Moved Permanently"
reasonPhrase Found              = "Found"
reasonPhrase NotModified        = "Not Modified"
reasonPhrase TemporaryRedirect  = "Temporary Redirect"
reasonPhrase PermanentRedirect  = "Permanent Redirect"
reasonPhrase BadRequest         = "Bad Request"
reasonPhrase Unauthorized       = "Unauthorized"
reasonPhrase Forbidden          = "Forbidden"
reasonPhrase NotFound           = "Not Found"
reasonPhrase MethodNotAllowed   = "Method Not Allowed"
reasonPhrase RequestTimeout     = "Request Timeout"
reasonPhrase Conflict           = "Conflict"
reasonPhrase Gone               = "Gone"
reasonPhrase LengthRequired     = "Length Required"
reasonPhrase PayloadTooLarge    = "Payload Too Large"
reasonPhrase URITooLong         = "URI Too Long"
reasonPhrase UnsupportedMedia   = "Unsupported Media Type"
reasonPhrase TooManyRequests    = "Too Many Requests"
reasonPhrase InternalError      = "Internal Server Error"
reasonPhrase NotImplemented     = "Not Implemented"
reasonPhrase BadGateway         = "Bad Gateway"
reasonPhrase ServiceUnavailable = "Service Unavailable"
reasonPhrase GatewayTimeout     = "Gateway Timeout"

public export
Show StatusCode where
  show code = show (statusToCode code) ++ " " ++ reasonPhrase code

-- ============================================================================
-- Classification and queries
-- ============================================================================

||| Determine the category of a status code from its numeric value.
public export
categorise : StatusCode -> StatusCategory
categorise Continue           = Informational
categorise SwitchingProtocols = Informational
categorise OK                 = Success
categorise Created            = Success
categorise Accepted           = Success
categorise NoContent          = Success
categorise MovedPermanently   = Redirect
categorise Found              = Redirect
categorise NotModified        = Redirect
categorise TemporaryRedirect  = Redirect
categorise PermanentRedirect  = Redirect
categorise BadRequest         = ClientError
categorise Unauthorized       = ClientError
categorise Forbidden          = ClientError
categorise NotFound           = ClientError
categorise MethodNotAllowed   = ClientError
categorise RequestTimeout     = ClientError
categorise Conflict           = ClientError
categorise Gone               = ClientError
categorise LengthRequired     = ClientError
categorise PayloadTooLarge    = ClientError
categorise URITooLong         = ClientError
categorise UnsupportedMedia   = ClientError
categorise TooManyRequests    = ClientError
categorise InternalError      = ServerError
categorise NotImplemented     = ServerError
categorise BadGateway         = ServerError
categorise ServiceUnavailable = ServerError
categorise GatewayTimeout     = ServerError

||| Whether the status code indicates success (2xx).
public export
isSuccess : StatusCode -> Bool
isSuccess code = categorise code == Success

||| Whether the status code indicates an error (4xx or 5xx).
public export
isError : StatusCode -> Bool
isError code = categorise code == ClientError || categorise code == ServerError

||| Whether the status code indicates a redirect (3xx).
public export
isRedirect : StatusCode -> Bool
isRedirect code = categorise code == Redirect

||| Parse a numeric code to a StatusCode. Returns Nothing for unknown codes.
public export
fromCode : Nat -> Maybe StatusCode
fromCode 100 = Just Continue
fromCode 101 = Just SwitchingProtocols
fromCode 200 = Just OK
fromCode 201 = Just Created
fromCode 202 = Just Accepted
fromCode 204 = Just NoContent
fromCode 301 = Just MovedPermanently
fromCode 302 = Just Found
fromCode 304 = Just NotModified
fromCode 307 = Just TemporaryRedirect
fromCode 308 = Just PermanentRedirect
fromCode 400 = Just BadRequest
fromCode 401 = Just Unauthorized
fromCode 403 = Just Forbidden
fromCode 404 = Just NotFound
fromCode 405 = Just MethodNotAllowed
fromCode 408 = Just RequestTimeout
fromCode 409 = Just Conflict
fromCode 410 = Just Gone
fromCode 411 = Just LengthRequired
fromCode 413 = Just PayloadTooLarge
fromCode 414 = Just URITooLong
fromCode 415 = Just UnsupportedMedia
fromCode 429 = Just TooManyRequests
fromCode 500 = Just InternalError
fromCode 501 = Just NotImplemented
fromCode 502 = Just BadGateway
fromCode 503 = Just ServiceUnavailable
fromCode 504 = Just GatewayTimeout
fromCode _   = Nothing
