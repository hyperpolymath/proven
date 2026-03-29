-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeSameSite - SameSite cookie attribute validation
|||
||| Ensures correct SameSite cookie configuration to prevent
||| CSRF attacks via cookie scope. Complements SafeCSRF and SafeCookie.
module Proven.SafeSameSite

import Data.String
import Data.Nat

%default total

||| SameSite attribute values
public export
data SameSite = Strict | Lax | None

public export
Show SameSite where
  show Strict = "Strict"
  show Lax    = "Lax"
  show None   = "None"

public export
Eq SameSite where
  Strict == Strict = True
  Lax == Lax = True
  None == None = True
  _ == _ = False

||| Cookie security configuration
public export
record CookieSecurity where
  constructor MkCookieSecurity
  sameSite : SameSite
  secure   : Bool      -- Secure flag (HTTPS only)
  httpOnly : Bool      -- HttpOnly flag (no JS access)
  path     : String
  domain   : String

||| Validation errors
public export
data SameSiteError =
    NoneWithoutSecure     -- SameSite=None requires Secure flag
  | MissingSameSite       -- No SameSite attribute (browser defaults vary)
  | LaxForSensitive       -- Lax insufficient for sensitive operations

public export
Show SameSiteError where
  show NoneWithoutSecure = "SameSite=None requires the Secure flag"
  show MissingSameSite   = "SameSite attribute missing (browser defaults vary)"
  show LaxForSensitive   = "SameSite=Lax insufficient for sensitive operations"

||| Validate cookie security for CSRF prevention
public export
validateSameSite : CookieSecurity -> List SameSiteError
validateSameSite cfg =
  let noneSecure = if cfg.sameSite == None && not cfg.secure
                     then [NoneWithoutSecure] else []
  in noneSecure

||| Recommended security for session cookies
public export
sessionCookieSecurity : CookieSecurity
sessionCookieSecurity = MkCookieSecurity Lax True True "/" ""

||| Recommended security for CSRF tokens
public export
csrfCookieSecurity : CookieSecurity
csrfCookieSecurity = MkCookieSecurity Strict True True "/" ""

||| Recommended security for cross-site cookies (e.g., SSO)
public export
crossSiteCookieSecurity : CookieSecurity
crossSiteCookieSecurity = MkCookieSecurity None True True "/" ""

||| Render Set-Cookie attributes
public export
renderAttributes : CookieSecurity -> String
renderAttributes cfg =
  "; SameSite=" ++ show cfg.sameSite ++
  (if cfg.secure then "; Secure" else "") ++
  (if cfg.httpOnly then "; HttpOnly" else "") ++
  (if cfg.path /= "" then "; Path=" ++ cfg.path else "") ++
  (if cfg.domain /= "" then "; Domain=" ++ cfg.domain else "")

||| Check if configuration is safe (no validation errors)
public export
isSafe : CookieSecurity -> Bool
isSafe = isNil . validateSameSite
