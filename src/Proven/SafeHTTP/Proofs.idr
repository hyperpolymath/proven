-- SPDX-License-Identifier: MPL-2.0
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

||| OWED: success status codes are not errors. If `isSuccess sc = True`
||| (i.e. `sc.code >= 200 && sc.code < 300`), then `isError sc = False`
||| (i.e. `sc.code >= 400` is false). Held back by Idris2 0.8.0 not
||| reducing the comparison-chain `>=`/`<` on the abstract `Nat`
||| `sc.code` to literal `True`/`False` by `Refl` alone — `sc.code` is
||| existentially bound by `MkStatusCode` and only carries the range
||| witness `So (code >= 100 && code <= 599)`, not a concrete value.
||| Same blocker family as SafeChecksum's `Nat`-mod reductions.
||| Discharge once a `Data.Nat` linear-arithmetic reflective tactic
||| is available, or by deriving via `boolAnd`/`boolNot` case-split on
||| the `So` witness plus `lteTransitive` (200 <= sc.code < 300 < 400).
export
0 successNotError : (sc : StatusCode) -> isSuccess sc = True -> isError sc = False

||| OWED: error status codes are not successes. If `isError sc = True`
||| (i.e. `sc.code >= 400`), then `isSuccess sc = False` (i.e.
||| `sc.code >= 200 && sc.code < 300` is false because `sc.code < 300`
||| contradicts `sc.code >= 400`). Held back by Idris2 0.8.0 not
||| reducing `Nat` order comparisons on the abstract record-projected
||| `sc.code` by `Refl`. Same blocker family as `successNotError`.
||| Discharge once a `Data.Nat` linear-arithmetic reflective tactic
||| is available, or by case-split on `>=`/`<` with `lteTransitive`.
export
0 errorNotSuccess : (sc : StatusCode) -> isError sc = True -> isSuccess sc = False

||| OWED: retryable status codes are errors. 429, 502, 503, 504 are
||| each `>= 400`, so `isRetryable sc = True` implies `isError sc =
||| True`. Held back by Idris2 0.8.0 not reducing the four-way `||`
||| equality-disjunction (`sc.code == 429 || ... || sc.code == 504`)
||| into a definite value of `sc.code >= 400` by `Refl` — the `Nat`
||| equality primitives on the abstract record field do not unfold.
||| Same blocker family as `successNotError`. Discharge once a
||| `Data.Nat` reflective tactic is available, or by four-arm case
||| analysis on the `||` chain with literal `lteSucc`-derived proofs
||| `429 >= 400`, `502 >= 400`, `503 >= 400`, `504 >= 400`.
export
0 retryableIsError : (sc : StatusCode) -> isRetryable sc = True -> isError sc = True

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
