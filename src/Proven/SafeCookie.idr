-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeCookie - Safe HTTP cookie handling
|||
||| This module provides safe cookie operations including:
||| - Cookie creation and validation
||| - Injection prevention (semicolons, newlines)
||| - Prefix enforcement (__Host-, __Secure-)
||| - SameSite security
||| - Secure-by-default attributes
|||
||| Example usage:
||| ```idris
||| -- Create cookie safely
||| case mkCookieDefault "session" token of
|||   Ok cookie => setCookieHeader cookie
|||   Err (CookieInjection _ _) => rejectRequest
|||   Err e => handleError e
|||
||| -- Create session cookie
||| case mkSessionCookie "__Host-session" sessionId of
|||   Ok cookie => setCookieHeader cookie
|||   Err e => handleError e
||| ```
module Proven.SafeCookie

import public Proven.Core
import public Proven.SafeCookie.Types
import public Proven.SafeCookie.Parser
import public Proven.SafeCookie.Proofs

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- High-Level API
--------------------------------------------------------------------------------

||| Create a validated cookie with default options
public export
cookie : String -> String -> CookieResult Cookie
cookie = mkCookieDefault

||| Create cookie with custom attributes
public export
cookieWith : String -> String -> CookieAttributes -> CookieResult Cookie
cookieWith = mkCookie defaultOptions

||| Create cookie with full options
public export
cookieFull : CookieOptions -> String -> String -> CookieAttributes -> CookieResult Cookie
cookieFull = mkCookie

||| Create session cookie (no expiration)
public export
session : String -> String -> CookieResult Cookie
session = mkSessionCookie

||| Create strict security cookie
public export
strict : String -> String -> CookieResult Cookie
strict = mkStrictCookie

--------------------------------------------------------------------------------
-- Common Cookie Patterns
--------------------------------------------------------------------------------

||| Create authentication cookie
public export
auth : String -> String -> Integer -> CookieResult Cookie
auth = authCookie

||| Create CSRF token cookie (JavaScript accessible)
public export
csrf : String -> String -> CookieResult Cookie
csrf = csrfCookie

||| Create remember-me cookie (30 days)
public export
remember : String -> String -> CookieResult Cookie
remember = rememberMeCookie

||| Create cross-site tracking cookie
public export
tracking : String -> String -> CookieResult Cookie
tracking = trackingCookie

--------------------------------------------------------------------------------
-- Collection API
--------------------------------------------------------------------------------

||| Empty cookie collection
public export
empty : Cookies
empty = []

||| Get cookie by name
public export
get : Cookies -> String -> Maybe Cookie
get = getCookie

||| Get cookie value by name
public export
value : Cookies -> String -> Maybe String
value = getCookieValue

||| Check if cookie exists
public export
has : Cookies -> String -> Bool
has = hasCookie

||| Add or update cookie
public export
set : Cookies -> Cookie -> CookieResult Cookies
set = setCookie defaultOptions

||| Remove cookie by name
public export
remove : Cookies -> String -> Cookies
remove = removeCookie

--------------------------------------------------------------------------------
-- Parsing API
--------------------------------------------------------------------------------

||| Parse Cookie header from request
public export
parse : String -> CookieResult Cookies
parse = parseCookieHeaderDefault

||| Parse with options
public export
parseWith : CookieOptions -> String -> CookieResult Cookies
parseWith = parseCookieHeader

--------------------------------------------------------------------------------
-- Building API
--------------------------------------------------------------------------------

||| Build Set-Cookie header value
public export
build : Cookie -> String
build = buildSetCookie

||| Build delete cookie header
public export
delete : String -> String
delete = buildDeleteCookie

--------------------------------------------------------------------------------
-- Attribute Builders
--------------------------------------------------------------------------------

||| Set domain
public export
domain : String -> CookieAttributes -> CookieAttributes
domain = withDomain

||| Set path
public export
path : String -> CookieAttributes -> CookieAttributes
path = withPath

||| Set Max-Age
public export
maxAge : Integer -> CookieAttributes -> CookieAttributes
maxAge = withMaxAge

||| Set Secure flag
public export
secure : CookieAttributes -> CookieAttributes
secure = withSecure

||| Set HttpOnly flag
public export
httpOnly : CookieAttributes -> CookieAttributes
httpOnly = withHttpOnly

||| Set SameSite attribute
public export
sameSite : SameSite -> CookieAttributes -> CookieAttributes
sameSite = withSameSite

||| Set Partitioned flag (CHIPS)
public export
partitioned : CookieAttributes -> CookieAttributes
partitioned = withPartitioned

--------------------------------------------------------------------------------
-- Expiration Presets
--------------------------------------------------------------------------------

||| 1 hour expiration
public export
hour : Integer
hour = oneHour

||| 1 day expiration
public export
day : Integer
day = oneDay

||| 1 week expiration
public export
week : Integer
week = oneWeek

||| 30 days expiration
public export
month : Integer
month = thirtyDays

||| 1 year expiration
public export
year : Integer
year = oneYear

--------------------------------------------------------------------------------
-- Attribute Presets
--------------------------------------------------------------------------------

||| Default secure attributes
public export
defaults : CookieAttributes
defaults = defaultAttributes

||| Session cookie attributes
public export
sessionAttrs : CookieAttributes
sessionAttrs = sessionAttributes

||| Strict security attributes
public export
strictAttrs : CookieAttributes
strictAttrs = strictAttributes

||| Cross-site cookie attributes
public export
crossSiteAttrs : CookieAttributes
crossSiteAttrs = crossSiteAttributes

--------------------------------------------------------------------------------
-- Options Presets
--------------------------------------------------------------------------------

||| Default options
public export
defaultOpts : CookieOptions
defaultOpts = defaultOptions

||| Strict options
public export
strictOpts : CookieOptions
strictOpts = strictOptions

--------------------------------------------------------------------------------
-- Validation Helpers
--------------------------------------------------------------------------------

||| Validate cookie name
public export
validName : String -> CookieResult CookieName
validName = validateNameDefault

||| Validate cookie value
public export
validValue : String -> String -> CookieResult CookieValue
validValue = validateValueDefault

||| Validate domain
public export
validDomain : String -> CookieResult String
validDomain = validateDomain defaultOptions

||| Validate path
public export
validPath : String -> CookieResult String
validPath = validatePath

--------------------------------------------------------------------------------
-- Error Helpers
--------------------------------------------------------------------------------

||| Check if error is injection attempt
public export
isInjection : CookieError -> Bool
isInjection (CookieInjection _ _) = True
isInjection _ = False

||| Check if error is size related
public export
isSizeError : CookieError -> Bool
isSizeError (NameTooLong _ _) = True
isSizeError (ValueTooLong _ _) = True
isSizeError (CookieTooLarge _) = True
isSizeError (TooManyCookies _) = True
isSizeError _ = False

||| Check if error is prefix violation
public export
isPrefixError : CookieError -> Bool
isPrefixError (PrefixViolation _ _ _) = True
isPrefixError _ = False

||| Get user-friendly error message
public export
friendlyError : CookieError -> String
friendlyError (NameTooLong name len) =
  "Cookie name '" ++ name ++ "' is too long (" ++ show len ++ " chars)"
friendlyError (ValueTooLong name len) =
  "Cookie value for '" ++ name ++ "' is too long (" ++ show len ++ " chars)"
friendlyError (InvalidNameChar name char) =
  "Invalid character '" ++ singleton char ++ "' in cookie name '" ++ name ++ "'"
friendlyError (InvalidValueChar name char) =
  "Invalid character in value for cookie '" ++ name ++ "'"
friendlyError EmptyName =
  "Cookie name cannot be empty"
friendlyError (CookieInjection name value) =
  "Possible cookie injection attack detected in '" ++ name ++ "'"
friendlyError (InvalidDomain domain reason) =
  "Invalid domain '" ++ domain ++ "': " ++ reason
friendlyError (InvalidPath path reason) =
  "Invalid path '" ++ path ++ "': " ++ reason
friendlyError (PrefixViolation name prefix reason) =
  "Cookie prefix " ++ show prefix ++ " violation for '" ++ name ++ "': " ++ reason
friendlyError SameSiteNoneRequiresSecure =
  "SameSite=None cookies must have the Secure attribute"
friendlyError (CookieTooLarge size) =
  "Cookie too large (" ++ show size ++ " bytes)"
friendlyError (TooManyCookies count) =
  "Too many cookies (" ++ show count ++ ")"
friendlyError (InvalidExpiration reason) =
  "Invalid expiration: " ++ reason

--------------------------------------------------------------------------------
-- Security Helpers
--------------------------------------------------------------------------------

||| Check if cookie has __Host- prefix
public export
isHostPrefixed : String -> Bool
isHostPrefixed name = getPrefix name == HostPrefix

||| Check if cookie has __Secure- prefix
public export
isSecurePrefixed : String -> Bool
isSecurePrefixed name = getPrefix name == SecurePrefix

||| Check if cookie has any security prefix
public export
hasPrefixSecurity : String -> Bool
hasPrefixSecurity name = getPrefix name /= NoPrefix

||| Check if attributes are secure
public export
isSecureCookie : CookieAttributes -> Bool
isSecureCookie attrs = attrs.secure && attrs.httpOnly

||| Check if attributes prevent CSRF
public export
preventsCsrf : CookieAttributes -> Bool
preventsCsrf attrs = case attrs.sameSite of
  Just Strict => True
  Just Lax => True
  _ => False

