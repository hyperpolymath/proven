-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Cookie parsing and building
|||
||| This module provides safe cookie operations including:
||| - Cookie header parsing
||| - Set-Cookie building
||| - Attribute handling
module Proven.SafeCookie.Parser

import Proven.Core
import Proven.SafeCookie.Types
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Name Validation
--------------------------------------------------------------------------------

||| Validate cookie name
export
validateName : CookieOptions -> String -> CookieResult CookieName
validateName opts name =
  let len = length (unpack name)
  in if null (unpack name)
       then Err EmptyName
       else if len > opts.maxNameLen
         then Err (NameTooLong name len)
         else case find (not . isValidNameChar) (unpack name) of
                Just c => Err (InvalidNameChar name c)
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
                Nothing => Ok (MkCookieName name (believe_me Refl))

||| Validate name with default options
export
validateNameDefault : String -> CookieResult CookieName
validateNameDefault = validateName defaultOptions

--------------------------------------------------------------------------------
-- Value Validation
--------------------------------------------------------------------------------

||| Validate cookie value
export
validateValue : CookieOptions -> String -> String -> CookieResult CookieValue
validateValue opts name value =
  let len = length (unpack value)
  in if len > opts.maxValueLen
       then Err (ValueTooLong name len)
       else if hasInjectionChar value
         then Err (CookieInjection name value)
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
         else Ok (MkCookieValue value (believe_me Refl))

||| Validate value with default options
export
validateValueDefault : String -> String -> CookieResult CookieValue
validateValueDefault = validateValue defaultOptions

--------------------------------------------------------------------------------
-- Domain Validation
--------------------------------------------------------------------------------

||| Validate cookie domain
export
validateDomain : CookieOptions -> String -> CookieResult String
validateDomain opts domain =
  let lower = toLower domain
  in if null (unpack lower)
       then Err (InvalidDomain domain "domain cannot be empty")
       else if isPrefixOf "." lower
         then Ok lower
         else if isInfixOf ".." lower
           then Err (InvalidDomain domain "consecutive dots")
           else if not (null opts.allowedDomains) && not (lower `elem` opts.allowedDomains)
             then Err (InvalidDomain domain "domain not in allowed list")
             else Ok lower

--------------------------------------------------------------------------------
-- Path Validation
--------------------------------------------------------------------------------

||| Validate cookie path
export
validatePath : String -> CookieResult String
validatePath path =
  if null (unpack path)
    then Ok "/"
    else if not (isPrefixOf "/" path)
      then Err (InvalidPath path "path must start with /")
      else if hasInjectionChar path
        then Err (InvalidPath path "contains invalid characters")
        else Ok path

--------------------------------------------------------------------------------
-- Prefix Validation
--------------------------------------------------------------------------------

||| Validate cookie prefix requirements
export
validatePrefix : CookieOptions -> String -> CookieAttributes -> CookieResult ()
validatePrefix opts name attrs =
  if not opts.enforcePrefixes
    then Ok ()
    else case getPrefix name of
           NoPrefix => Ok ()
           SecurePrefix =>
             if not attrs.secure
               then Err (PrefixViolation name SecurePrefix "requires Secure attribute")
               else Ok ()
           HostPrefix =>
             if not attrs.secure
               then Err (PrefixViolation name HostPrefix "requires Secure attribute")
               else if isJust attrs.domain
                 then Err (PrefixViolation name HostPrefix "Domain attribute not allowed")
                 else if attrs.path /= Just "/"
                   then Err (PrefixViolation name HostPrefix "Path must be /")
                   else Ok ()

--------------------------------------------------------------------------------
-- Attributes Validation
--------------------------------------------------------------------------------

||| Validate cookie attributes
export
validateAttributes : CookieOptions -> String -> CookieAttributes -> CookieResult CookieAttributes
validateAttributes opts name attrs = do
  -- Check SameSite=None requires Secure
  case attrs.sameSite of
    Just None => if not attrs.secure then Err SameSiteNoneRequiresSecure else Ok ()
    _ => Ok ()
  -- Check required flags
  if opts.requireSecure && not attrs.secure
    then Err (PrefixViolation name NoPrefix "Secure flag required")
    else Ok ()
  if opts.requireHttpOnly && not attrs.httpOnly
    then Err (PrefixViolation name NoPrefix "HttpOnly flag required")
    else Ok ()
  -- Validate domain if present
  case attrs.domain of
    Just d => do
      _ <- validateDomain opts d
      Ok ()
    Nothing => Ok ()
  -- Validate path if present
  case attrs.path of
    Just p => do
      _ <- validatePath p
      Ok ()
    Nothing => Ok ()
  -- Validate prefix
  validatePrefix opts name attrs
  Ok attrs

--------------------------------------------------------------------------------
-- Cookie Creation
--------------------------------------------------------------------------------

||| Create a validated cookie
export
mkCookie : CookieOptions -> String -> String -> CookieAttributes -> CookieResult Cookie
mkCookie opts name value attrs = do
  validName <- validateName opts name
  validValue <- validateValue opts name value
  validAttrs <- validateAttributes opts name attrs
  let cookie = MkCookie validName validValue validAttrs
  -- Check total size
  let size = cookieSize cookie
  if size > opts.maxSize
    then Err (CookieTooLarge size)
    else Ok cookie
  where
    cookieSize : Cookie -> Nat
    cookieSize c = length (unpack name) + 1 + length (unpack value) + 100  -- Approximate attribute overhead

||| Create cookie with default options and attributes
export
mkCookieDefault : String -> String -> CookieResult Cookie
mkCookieDefault name value = mkCookie defaultOptions name value defaultAttributes

||| Create session cookie
export
mkSessionCookie : String -> String -> CookieResult Cookie
mkSessionCookie name value = mkCookie defaultOptions name value sessionAttributes

||| Create strict cookie
export
mkStrictCookie : String -> String -> CookieResult Cookie
mkStrictCookie name value = mkCookie strictOptions name value strictAttributes

--------------------------------------------------------------------------------
-- Cookie Header Parsing
--------------------------------------------------------------------------------

||| Parse Cookie header (request)
export
parseCookieHeader : CookieOptions -> String -> CookieResult Cookies
parseCookieHeader opts header =
  let pairs = split (== ';') header
  in traverse (parsePair opts) (filter (not . null . unpack . trim) pairs)
  where
    parsePair : CookieOptions -> String -> CookieResult Cookie
    parsePair opts pair =
      case break (== '=') (trim pair) of
        (name, rest) =>
          if null (unpack rest)
            then mkCookie opts (trim name) "" defaultAttributes
            else mkCookie opts (trim name) (trim (drop 1 rest)) defaultAttributes

||| Parse with default options
export
parseCookieHeaderDefault : String -> CookieResult Cookies
parseCookieHeaderDefault = parseCookieHeader defaultOptions

--------------------------------------------------------------------------------
-- Set-Cookie Building
--------------------------------------------------------------------------------

||| Build Set-Cookie header value
export
buildSetCookie : Cookie -> String
buildSetCookie cookie =
  let base = cookie.name.name ++ "=" ++ cookie.value.value
      attrs = cookie.attributes
      parts = base :: buildAttributes attrs
  in joinBy "; " parts
  where
    buildAttributes : CookieAttributes -> List String
    buildAttributes a =
      catMaybes
        [ map (\d => "Domain=" ++ d) a.domain
        , map (\p => "Path=" ++ p) a.path
        , map (\e => "Expires=" ++ formatExpires e) a.expires
        , map (\m => "Max-Age=" ++ show m) a.maxAge
        , if a.secure then Just "Secure" else Nothing
        , if a.httpOnly then Just "HttpOnly" else Nothing
        , map (\s => "SameSite=" ++ show s) a.sameSite
        , if a.partitioned then Just "Partitioned" else Nothing
        ]

    formatExpires : Integer -> String
    formatExpires ts = "Thu, 01 Jan 1970 00:00:00 GMT"  -- Simplified

||| Build Set-Cookie for deletion (expired)
export
buildDeleteCookie : String -> String
buildDeleteCookie name =
  name ++ "=; Max-Age=0; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT"

--------------------------------------------------------------------------------
-- Cookie Collection Operations
--------------------------------------------------------------------------------

||| Get cookie by name
export
getCookie : Cookies -> String -> Maybe Cookie
getCookie cookies name = find (\c => c.name.name == name) cookies

||| Get cookie value by name
export
getCookieValue : Cookies -> String -> Maybe String
getCookieValue cookies name =
  map (\c => c.value.value) (getCookie cookies name)

||| Check if cookie exists
export
hasCookie : Cookies -> String -> Bool
hasCookie cookies name = isJust (getCookie cookies name)

||| Add or update cookie
export
setCookie : CookieOptions -> Cookies -> Cookie -> CookieResult Cookies
setCookie opts cookies cookie =
  let filtered = filter (\c => c.name.name /= cookie.name.name) cookies
      newCookies = cookie :: filtered
  in if length newCookies > maxCookiesPerDomain
       then Err (TooManyCookies (length newCookies))
       else Ok newCookies

||| Remove cookie by name
export
removeCookie : Cookies -> String -> Cookies
removeCookie cookies name = filter (\c => c.name.name /= name) cookies

--------------------------------------------------------------------------------
-- Attribute Builders
--------------------------------------------------------------------------------

||| Set domain attribute
export
withDomain : String -> CookieAttributes -> CookieAttributes
withDomain d attrs = { domain := Just d } attrs

||| Set path attribute
export
withPath : String -> CookieAttributes -> CookieAttributes
withPath p attrs = { path := Just p } attrs

||| Set Max-Age attribute
export
withMaxAge : Integer -> CookieAttributes -> CookieAttributes
withMaxAge age attrs = { maxAge := Just age } attrs

||| Set Secure attribute
export
withSecure : CookieAttributes -> CookieAttributes
withSecure attrs = { secure := True } attrs

||| Set HttpOnly attribute
export
withHttpOnly : CookieAttributes -> CookieAttributes
withHttpOnly attrs = { httpOnly := True } attrs

||| Set SameSite attribute
export
withSameSite : SameSite -> CookieAttributes -> CookieAttributes
withSameSite ss attrs = { sameSite := Just ss } attrs

||| Set Partitioned attribute
export
withPartitioned : CookieAttributes -> CookieAttributes
withPartitioned attrs = { partitioned := True } attrs

--------------------------------------------------------------------------------
-- Expiration Helpers
--------------------------------------------------------------------------------

||| Calculate max age for N days
export
daysToSeconds : Nat -> Integer
daysToSeconds days = cast days * 24 * 60 * 60

||| Calculate max age for N hours
export
hoursToSeconds : Nat -> Integer
hoursToSeconds hours = cast hours * 60 * 60

||| Calculate max age for N minutes
export
minutesToSeconds : Nat -> Integer
minutesToSeconds minutes = cast minutes * 60

||| Common expiration: 1 hour
export
oneHour : Integer
oneHour = hoursToSeconds 1

||| Common expiration: 1 day
export
oneDay : Integer
oneDay = daysToSeconds 1

||| Common expiration: 1 week
export
oneWeek : Integer
oneWeek = daysToSeconds 7

||| Common expiration: 30 days
export
thirtyDays : Integer
thirtyDays = daysToSeconds 30

||| Common expiration: 1 year
export
oneYear : Integer
oneYear = daysToSeconds 365

--------------------------------------------------------------------------------
-- Common Cookie Patterns
--------------------------------------------------------------------------------

||| Create authentication token cookie
export
authCookie : String -> String -> Integer -> CookieResult Cookie
authCookie name token maxAge =
  mkCookie strictOptions name token
    (withMaxAge maxAge $ withSameSite Strict strictAttributes)

||| Create CSRF token cookie
export
csrfCookie : String -> String -> CookieResult Cookie
csrfCookie name token =
  let attrs = { httpOnly := False } strictAttributes  -- JS needs access
  in mkCookie defaultOptions name token (withSameSite Strict attrs)

||| Create remember-me cookie
export
rememberMeCookie : String -> String -> CookieResult Cookie
rememberMeCookie name value =
  mkCookie defaultOptions name value
    (withMaxAge thirtyDays $ withSameSite Lax defaultAttributes)

||| Create tracking cookie (cross-site)
export
trackingCookie : String -> String -> CookieResult Cookie
trackingCookie name value =
  mkCookie defaultOptions name value crossSiteAttributes

