-- SPDX-License-Identifier: PMPL-1.0
module ProvenConcat.RouterTable

import Proven.SafeUrl
import Proven.SafeRegex
import Proven.SafeHeader
import Proven.SafeContentType

%default total

public export
record RouteRequest where
  constructor MkRouteRequest
  url : String
  headers : List (String, String)
  contentType : String

public export
record RouteMatch where
  constructor MkRouteMatch
  routeId : String
  params : List (String, String)

public export
data RouteError
  = NoMatch
  | InvalidUrl String
  | InvalidHeaders String
  | InvalidContentType String

public export
matchRoute : RouteRequest -> Either RouteError RouteMatch
matchRoute _ = Left NoMatch
