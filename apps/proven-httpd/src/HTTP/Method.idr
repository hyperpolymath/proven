-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- HTTP Request Methods (RFC 7231 Section 4, RFC 5789)
--
-- All standard HTTP/1.1 methods represented as a closed sum type.
-- No unknown or malformed method can crash the server; unrecognised
-- method strings parse to Nothing rather than throwing an exception.

module HTTP.Method

%default total

-- ============================================================================
-- HTTP Methods (RFC 7231 Section 4)
-- ============================================================================

||| Standard HTTP request methods.
||| Each constructor corresponds to a method defined in RFC 7231 or RFC 5789.
public export
data Method : Type where
  ||| Retrieve a representation of the target resource.
  GET     : Method
  ||| Perform resource-specific processing on the request payload.
  POST    : Method
  ||| Replace all current representations of the target resource.
  PUT     : Method
  ||| Remove all current representations of the target resource.
  DELETE  : Method
  ||| Apply partial modifications to a resource (RFC 5789).
  PATCH   : Method
  ||| Same as GET but only transfer the status line and header section.
  HEAD    : Method
  ||| Describe the communication options for the target resource.
  OPTIONS : Method
  ||| Establish a tunnel to the server identified by the target resource.
  CONNECT : Method
  ||| Perform a message loop-back test along the path to the target.
  TRACE   : Method

public export
Eq Method where
  GET     == GET     = True
  POST    == POST    = True
  PUT     == PUT     = True
  DELETE  == DELETE  = True
  PATCH   == PATCH   = True
  HEAD    == HEAD    = True
  OPTIONS == OPTIONS = True
  CONNECT == CONNECT = True
  TRACE   == TRACE   = True
  _       == _       = False

public export
Show Method where
  show GET     = "GET"
  show POST    = "POST"
  show PUT     = "PUT"
  show DELETE  = "DELETE"
  show PATCH   = "PATCH"
  show HEAD    = "HEAD"
  show OPTIONS = "OPTIONS"
  show CONNECT = "CONNECT"
  show TRACE   = "TRACE"

-- ============================================================================
-- Parsing and classification
-- ============================================================================

||| Parse a method string into a typed Method value.
||| Returns Nothing for unrecognised methods (safe, no crash).
public export
parseMethod : String -> Maybe Method
parseMethod "GET"     = Just GET
parseMethod "POST"    = Just POST
parseMethod "PUT"     = Just PUT
parseMethod "DELETE"  = Just DELETE
parseMethod "PATCH"   = Just PATCH
parseMethod "HEAD"    = Just HEAD
parseMethod "OPTIONS" = Just OPTIONS
parseMethod "CONNECT" = Just CONNECT
parseMethod "TRACE"   = Just TRACE
parseMethod _         = Nothing

||| Serialise a Method back to its canonical string representation.
public export
methodToString : Method -> String
methodToString = show

||| Whether the method is considered "safe" (RFC 7231 Section 4.2.1).
||| Safe methods are essentially read-only and should not cause side effects.
public export
isSafe : Method -> Bool
isSafe GET     = True
isSafe HEAD    = True
isSafe OPTIONS = True
isSafe TRACE   = True
isSafe _       = False

||| Whether the method is idempotent (RFC 7231 Section 4.2.2).
||| Idempotent methods produce the same result regardless of repetition.
public export
isIdempotent : Method -> Bool
isIdempotent GET     = True
isIdempotent HEAD    = True
isIdempotent PUT     = True
isIdempotent DELETE  = True
isIdempotent OPTIONS = True
isIdempotent TRACE   = True
isIdempotent _       = False

||| Whether the method typically carries a request body.
public export
hasRequestBody : Method -> Bool
hasRequestBody POST  = True
hasRequestBody PUT   = True
hasRequestBody PATCH = True
hasRequestBody _     = False

||| List all standard HTTP methods.
public export
allMethods : List Method
allMethods = [GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS, CONNECT, TRACE]
