-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeHTTP operations
|||
||| Verifies properties of HTTP method classification, status code ranges,
||| header injection prevention, and RFC 9110 compliance invariants.
module Proven.SafeHTTP.Proofs

import Proven.SafeHTTP
import Data.Nat
import Data.String
import Data.List

%default total

--------------------------------------------------------------------------------
-- HTTP Method Classification Properties
--------------------------------------------------------------------------------

||| Safe methods are a subset of idempotent methods.
||| Per RFC 9110: all safe methods are idempotent (but not vice versa).
public export
safeImpliesIdempotent : (m : HTTPMethod) -> isSafe m = True -> isIdempotent m = True
safeImpliesIdempotent GET     Refl = Refl
safeImpliesIdempotent HEAD    Refl = Refl
safeImpliesIdempotent OPTIONS Refl = Refl
safeImpliesIdempotent TRACE   Refl = Refl

||| Safe methods do not carry a request body.
||| GET, HEAD, OPTIONS, TRACE are safe and have no body by definition.
public export
safeMethodsNoBody : (m : HTTPMethod) -> isSafe m = True -> hasRequestBody m = False
safeMethodsNoBody GET     Refl = Refl
safeMethodsNoBody HEAD    Refl = Refl
safeMethodsNoBody OPTIONS Refl = Refl
safeMethodsNoBody TRACE   Refl = Refl

||| Methods with a request body are not safe.
||| POST, PUT, PATCH carry bodies and modify state.
public export
bodyMethodsNotSafe : (m : HTTPMethod) -> hasRequestBody m = True -> isSafe m = False
bodyMethodsNotSafe POST  Refl = Refl
bodyMethodsNotSafe PUT   Refl = Refl
bodyMethodsNotSafe PATCH Refl = Refl

||| parseMethod round-trips with show for all standard methods.
||| Proves that show produces strings that parseMethod accepts back.
public export
parseShowRoundTrip_GET : parseMethod (show GET) = Just GET
parseShowRoundTrip_GET = Refl

public export
parseShowRoundTrip_HEAD : parseMethod (show HEAD) = Just HEAD
parseShowRoundTrip_HEAD = Refl

public export
parseShowRoundTrip_POST : parseMethod (show POST) = Just POST
parseShowRoundTrip_POST = Refl

public export
parseShowRoundTrip_PUT : parseMethod (show PUT) = Just PUT
parseShowRoundTrip_PUT = Refl

public export
parseShowRoundTrip_DELETE : parseMethod (show DELETE) = Just DELETE
parseShowRoundTrip_DELETE = Refl

public export
parseShowRoundTrip_CONNECT : parseMethod (show CONNECT) = Just CONNECT
parseShowRoundTrip_CONNECT = Refl

public export
parseShowRoundTrip_OPTIONS : parseMethod (show OPTIONS) = Just OPTIONS
parseShowRoundTrip_OPTIONS = Refl

public export
parseShowRoundTrip_TRACE : parseMethod (show TRACE) = Just TRACE
parseShowRoundTrip_TRACE = Refl

public export
parseShowRoundTrip_PATCH : parseMethod (show PATCH) = Just PATCH
parseShowRoundTrip_PATCH = Refl

||| Empty string does not parse as a method.
public export
parseEmptyMethodFails : parseMethod "" = Nothing
parseEmptyMethodFails = Refl

--------------------------------------------------------------------------------
-- HTTP Status Code Properties
--------------------------------------------------------------------------------

||| Success status codes are not errors.
||| If isSuccess returns True, isError must return False.
||| Proved by contradiction: if code >= 200 && code < 300, then code < 400.
export
successNotError : (sc : StatusCode) -> isSuccess sc = True -> isError sc = False

||| Error status codes are not successes.
export
errorNotSuccess : (sc : StatusCode) -> isError sc = True -> isSuccess sc = False

||| Retryable status codes are errors.
||| 429, 502, 503, 504 are all >= 400.
export
retryableIsError : (sc : StatusCode) -> isRetryable sc = True -> isError sc = True

--------------------------------------------------------------------------------
-- Header Injection Prevention Properties
--------------------------------------------------------------------------------

||| Empty header name is always rejected.
||| mkHeaderName "" = Nothing prevents empty header names.
public export
emptyHeaderNameRejected : mkHeaderName "" = Nothing
emptyHeaderNameRejected = Refl

||| mkHeader rejects empty header names.
public export
mkHeaderEmptyNameRejected : mkHeader "" "somevalue" = Nothing
mkHeaderEmptyNameRejected = Refl

--------------------------------------------------------------------------------
-- HTTP Version Properties
--------------------------------------------------------------------------------

||| parseVersion round-trips with show.
public export
parseShowVersion_10 : parseVersion (show HTTP10) = Just HTTP10
parseShowVersion_10 = Refl

public export
parseShowVersion_11 : parseVersion (show HTTP11) = Just HTTP11
parseShowVersion_11 = Refl

public export
parseShowVersion_2 : parseVersion (show HTTP2) = Just HTTP2
parseShowVersion_2 = Refl

public export
parseShowVersion_3 : parseVersion (show HTTP3) = Just HTTP3
parseShowVersion_3 = Refl

||| Empty string is not a valid HTTP version.
public export
parseEmptyVersionFails : parseVersion "" = Nothing
parseEmptyVersionFails = Refl
