-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- HTTP Route Matching and Dispatch (Pattern-based)
--
-- Routes are defined as a list of (method, path-pattern, handler) tuples.
-- The router matches incoming requests against the route table and dispatches
-- to the first matching handler. Unmatched requests produce a 404 or 405
-- response automatically. The dispatch function is total.

module HTTP.Router

import HTTP.Method
import HTTP.Status
import HTTP.Request
import HTTP.Response

%default total

-- ============================================================================
-- Route pattern matching
-- ============================================================================

||| A path segment in a route pattern. Supports exact matches and wildcards.
public export
data PathSegment : Type where
  ||| Match a literal path segment exactly.
  Exact    : String -> PathSegment
  ||| Match any single path segment and capture its value.
  Capture  : String -> PathSegment
  ||| Match any remaining path segments (must be last).
  Wildcard : PathSegment

public export
Show PathSegment where
  show (Exact s)   = s
  show (Capture n) = ":" ++ n
  show Wildcard    = "*"

public export
Eq PathSegment where
  (Exact a)   == (Exact b)   = a == b
  (Capture a) == (Capture b) = a == b
  Wildcard    == Wildcard    = True
  _           == _           = False

||| A route pattern is a list of path segments to match against.
public export
RoutePattern : Type
RoutePattern = List PathSegment

-- ============================================================================
-- Captured parameters
-- ============================================================================

||| A captured path parameter from a route match.
public export
record PathParam where
  constructor MkPathParam
  ||| The parameter name (from the Capture segment).
  paramName  : String
  ||| The captured value from the actual request path.
  paramValue : String

public export
Show PathParam where
  show p = p.paramName ++ "=" ++ p.paramValue

||| Look up a captured parameter by name.
public export
lookupParam : String -> List PathParam -> Maybe String
lookupParam _    []        = Nothing
lookupParam name (p :: ps) =
  if name == p.paramName
    then Just p.paramValue
    else lookupParam name ps

-- ============================================================================
-- Route matching
-- ============================================================================

||| Split a path string on '/' separators, filtering empty segments.
public export
splitPath : String -> List String
splitPath path = filter (/= "") (toList (split (== '/') path))

||| Match a route pattern against a list of path segments.
||| Returns captured parameters on success, Nothing on failure.
public export
matchPattern : RoutePattern -> List String -> Maybe (List PathParam)
matchPattern []             []         = Just []
matchPattern []             (_ :: _)   = Nothing
matchPattern (_ :: _)       []         = Nothing
matchPattern (Wildcard :: _) _         = Just []  -- Wildcard matches remainder
matchPattern ((Exact e) :: ps) (s :: ss) =
  if e == s then matchPattern ps ss else Nothing
matchPattern ((Capture n) :: ps) (s :: ss) =
  case matchPattern ps ss of
    Just params => Just (MkPathParam n s :: params)
    Nothing     => Nothing

-- ============================================================================
-- Route table
-- ============================================================================

||| A handler function that takes a request and captured params, producing
||| a response. Handlers are pure functions.
public export
Handler : Type
Handler = Request -> List PathParam -> Response

||| A single route entry: method + pattern + handler.
public export
record Route where
  constructor MkRoute
  ||| The HTTP method this route matches.
  routeMethod  : Method
  ||| The path pattern to match against.
  routePattern : RoutePattern
  ||| The handler to invoke on match.
  routeHandler : Handler

||| A route table is an ordered list of routes. The first match wins.
public export
RouteTable : Type
RouteTable = List Route

-- ============================================================================
-- Router dispatch
-- ============================================================================

||| Attempt to match a request against a route table.
||| Returns the first matching route's handler result, or an error response.
||| - If the path matches but the method does not: 405 Method Not Allowed.
||| - If no path matches at all: 404 Not Found.
public export
dispatch : RouteTable -> Request -> Response
dispatch routes req =
  let segments = splitPath req.path
      result   = tryRoutes routes segments req False
  in result
  where
    ||| Try each route in order. The `pathMatched` flag tracks whether any
    ||| route matched the path (but not necessarily the method).
    tryRoutes : RouteTable -> List String -> Request -> (pathMatched : Bool) -> Response
    tryRoutes []       _    _   True  = errorResponse MethodNotAllowed
    tryRoutes []       _    _   False = errorResponse NotFound
    tryRoutes (r :: rs) segs rq pm =
      case matchPattern r.routePattern segs of
        Nothing     => tryRoutes rs segs rq pm
        Just params =>
          if r.routeMethod == rq.method
            then r.routeHandler rq params
            else tryRoutes rs segs rq True

-- ============================================================================
-- Route builder helpers
-- ============================================================================

||| Create a GET route with an exact path.
public export
get : String -> Handler -> Route
get path handler = MkRoute GET (map Exact (splitPath path)) handler

||| Create a POST route with an exact path.
public export
post : String -> Handler -> Route
post path handler = MkRoute POST (map Exact (splitPath path)) handler

||| Create a PUT route with an exact path.
public export
put : String -> Handler -> Route
put path handler = MkRoute PUT (map Exact (splitPath path)) handler

||| Create a DELETE route with an exact path.
public export
delete : String -> Handler -> Route
delete path handler = MkRoute DELETE (map Exact (splitPath path)) handler

||| Create a route with a parameterised path.
||| Example: routeWith GET [Exact "users", Capture "id"] handler
public export
routeWith : Method -> RoutePattern -> Handler -> Route
routeWith = MkRoute
