-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Safe cookie types and constraints
|||
||| This module defines types for safe HTTP cookie handling including:
||| - Cookie attributes (Secure, HttpOnly, SameSite)
||| - Expiration handling
||| - Domain/path validation
module Proven.SafeCookie.Types

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Cookie Constraints
--------------------------------------------------------------------------------

||| Maximum cookie name length
public export
maxNameLength : Nat
maxNameLength = 256

||| Maximum cookie value length
public export
maxValueLength : Nat
maxValueLength = 4096

||| Maximum total cookie size (name + value + attributes)
public export
maxCookieSize : Nat
maxCookieSize = 4096

||| Maximum number of cookies per domain
public export
maxCookiesPerDomain : Nat
maxCookiesPerDomain = 50

--------------------------------------------------------------------------------
-- SameSite Attribute
--------------------------------------------------------------------------------

||| SameSite cookie attribute
public export
data SameSite : Type where
  ||| Cookie sent with same-site requests only
  Strict : SameSite
  ||| Cookie sent with same-site + top-level navigations
  Lax : SameSite
  ||| Cookie always sent (requires Secure)
  None : SameSite

public export
Show SameSite where
  show Strict = "Strict"
  show Lax = "Lax"
  show None = "None"

public export
Eq SameSite where
  Strict == Strict = True
  Lax == Lax = True
  None == None = True
  _ == _ = False

--------------------------------------------------------------------------------
-- Cookie Attributes
--------------------------------------------------------------------------------

||| Cookie security attributes
public export
record CookieAttributes where
  constructor MkCookieAttributes
  ||| Cookie domain
  domain : Maybe String
  ||| Cookie path
  path : Maybe String
  ||| Expiration (Unix timestamp in seconds)
  expires : Maybe Integer
  ||| Max-Age in seconds
  maxAge : Maybe Integer
  ||| Secure flag (HTTPS only)
  secure : Bool
  ||| HttpOnly flag (no JS access)
  httpOnly : Bool
  ||| SameSite attribute
  sameSite : Maybe SameSite
  ||| Partitioned (CHIPS)
  partitioned : Bool

public export
Eq CookieAttributes where
  a1 == a2 = a1.domain == a2.domain &&
             a1.path == a2.path &&
             a1.expires == a2.expires &&
             a1.maxAge == a2.maxAge &&
             a1.secure == a2.secure &&
             a1.httpOnly == a2.httpOnly &&
             a1.sameSite == a2.sameSite &&
             a1.partitioned == a2.partitioned

||| Default attributes (secure defaults)
public export
defaultAttributes : CookieAttributes
defaultAttributes = MkCookieAttributes
  { domain = Nothing
  , path = Just "/"
  , expires = Nothing
  , maxAge = Nothing
  , secure = True
  , httpOnly = True
  , sameSite = Just Lax
  , partitioned = False
  }

||| Session cookie attributes (no expiration)
public export
sessionAttributes : CookieAttributes
sessionAttributes = MkCookieAttributes
  { domain = Nothing
  , path = Just "/"
  , expires = Nothing
  , maxAge = Nothing
  , secure = True
  , httpOnly = True
  , sameSite = Just Strict
  , partitioned = False
  }

||| Strict security attributes
public export
strictAttributes : CookieAttributes
strictAttributes = MkCookieAttributes
  { domain = Nothing
  , path = Just "/"
  , expires = Nothing
  , maxAge = Nothing
  , secure = True
  , httpOnly = True
  , sameSite = Just Strict
  , partitioned = False
  }

||| Cross-site cookie attributes (requires Secure)
public export
crossSiteAttributes : CookieAttributes
crossSiteAttributes = MkCookieAttributes
  { domain = Nothing
  , path = Just "/"
  , expires = Nothing
  , maxAge = Nothing
  , secure = True
  , httpOnly = True
  , sameSite = Just None
  , partitioned = True
  }

--------------------------------------------------------------------------------
-- Cookie Name
--------------------------------------------------------------------------------

||| Validated cookie name
public export
record CookieName where
  constructor MkCookieName
  ||| The cookie name
  name : String
  ||| Proof name is bounded
  0 bounded : length (unpack name) <= maxNameLength = True

public export
Eq CookieName where
  c1 == c2 = c1.name == c2.name

public export
Show CookieName where
  show c = c.name

--------------------------------------------------------------------------------
-- Cookie Value
--------------------------------------------------------------------------------

||| Validated cookie value
public export
record CookieValue where
  constructor MkCookieValue
  ||| The cookie value
  value : String
  ||| Proof value is bounded
  0 bounded : length (unpack value) <= maxValueLength = True

public export
Eq CookieValue where
  c1 == c2 = c1.value == c2.value

public export
Show CookieValue where
  show c = c.value

--------------------------------------------------------------------------------
-- Cookie
--------------------------------------------------------------------------------

||| Complete validated cookie
public export
record Cookie where
  constructor MkCookie
  ||| Cookie name
  name : CookieName
  ||| Cookie value
  value : CookieValue
  ||| Cookie attributes
  attributes : CookieAttributes

public export
Eq Cookie where
  c1 == c2 = c1.name == c2.name && c1.value == c2.value

public export
Show Cookie where
  show c = show c.name ++ "=" ++ show c.value

||| Collection of cookies
public export
Cookies : Type
Cookies = List Cookie

--------------------------------------------------------------------------------
-- Cookie Prefixes
--------------------------------------------------------------------------------

||| Cookie prefix type (security mechanism)
public export
data CookiePrefix : Type where
  ||| No prefix
  NoPrefix : CookiePrefix
  ||| __Secure- prefix (requires Secure, set from secure origin)
  SecurePrefix : CookiePrefix
  ||| __Host- prefix (requires Secure, no Domain, Path=/)
  HostPrefix : CookiePrefix

public export
Show CookiePrefix where
  show NoPrefix = ""
  show SecurePrefix = "__Secure-"
  show HostPrefix = "__Host-"

public export
Eq CookiePrefix where
  NoPrefix == NoPrefix = True
  SecurePrefix == SecurePrefix = True
  HostPrefix == HostPrefix = True
  _ == _ = False

||| Get prefix from cookie name
public export
getPrefix : String -> CookiePrefix
getPrefix name =
  if isPrefixOf "__Host-" name then HostPrefix
  else if isPrefixOf "__Secure-" name then SecurePrefix
  else NoPrefix

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

||| Cookie parsing/validation errors
public export
data CookieError : Type where
  ||| Cookie name too long
  NameTooLong : (name : String) -> (len : Nat) -> CookieError
  ||| Cookie value too long
  ValueTooLong : (name : String) -> (len : Nat) -> CookieError
  ||| Invalid cookie name character
  InvalidNameChar : (name : String) -> (char : Char) -> CookieError
  ||| Invalid cookie value character
  InvalidValueChar : (name : String) -> (char : Char) -> CookieError
  ||| Empty cookie name
  EmptyName : CookieError
  ||| Cookie injection attempt
  CookieInjection : (name : String) -> (value : String) -> CookieError
  ||| Invalid domain
  InvalidDomain : (domain : String) -> (reason : String) -> CookieError
  ||| Invalid path
  InvalidPath : (path : String) -> (reason : String) -> CookieError
  ||| Prefix requirements not met
  PrefixViolation : (name : String) -> (prefix : CookiePrefix) -> (reason : String) -> CookieError
  ||| SameSite=None requires Secure
  SameSiteNoneRequiresSecure : CookieError
  ||| Total cookie size too large
  CookieTooLarge : (size : Nat) -> CookieError
  ||| Too many cookies
  TooManyCookies : (count : Nat) -> CookieError
  ||| Invalid expiration
  InvalidExpiration : (reason : String) -> CookieError

public export
Show CookieError where
  show (NameTooLong name len) =
    "Cookie name too long: " ++ show len ++ " chars (max " ++ show maxNameLength ++ ")"
  show (ValueTooLong name len) =
    "Cookie value too long for " ++ name ++ ": " ++ show len ++ " chars"
  show (InvalidNameChar name char) =
    "Invalid character '" ++ singleton char ++ "' in cookie name: " ++ name
  show (InvalidValueChar name char) =
    "Invalid character '" ++ singleton char ++ "' in cookie value"
  show EmptyName =
    "Empty cookie name"
  show (CookieInjection name value) =
    "Cookie injection attempt detected in " ++ name
  show (InvalidDomain domain reason) =
    "Invalid domain '" ++ domain ++ "': " ++ reason
  show (InvalidPath path reason) =
    "Invalid path '" ++ path ++ "': " ++ reason
  show (PrefixViolation name prefix reason) =
    "Cookie prefix " ++ show prefix ++ " violation for " ++ name ++ ": " ++ reason
  show SameSiteNoneRequiresSecure =
    "SameSite=None requires Secure attribute"
  show (CookieTooLarge size) =
    "Cookie too large: " ++ show size ++ " bytes (max " ++ show maxCookieSize ++ ")"
  show (TooManyCookies count) =
    "Too many cookies: " ++ show count ++ " (max " ++ show maxCookiesPerDomain ++ ")"
  show (InvalidExpiration reason) =
    "Invalid expiration: " ++ reason

public export
Eq CookieError where
  NameTooLong n1 l1 == NameTooLong n2 l2 = n1 == n2 && l1 == l2
  ValueTooLong n1 l1 == ValueTooLong n2 l2 = n1 == n2 && l1 == l2
  InvalidNameChar n1 c1 == InvalidNameChar n2 c2 = n1 == n2 && c1 == c2
  InvalidValueChar n1 c1 == InvalidValueChar n2 c2 = n1 == n2 && c1 == c2
  EmptyName == EmptyName = True
  CookieInjection n1 v1 == CookieInjection n2 v2 = n1 == n2 && v1 == v2
  InvalidDomain d1 r1 == InvalidDomain d2 r2 = d1 == d2 && r1 == r2
  InvalidPath p1 r1 == InvalidPath p2 r2 = p1 == p2 && r1 == r2
  PrefixViolation n1 p1 r1 == PrefixViolation n2 p2 r2 = n1 == n2 && p1 == p2 && r1 == r2
  SameSiteNoneRequiresSecure == SameSiteNoneRequiresSecure = True
  CookieTooLarge s1 == CookieTooLarge s2 = s1 == s2
  TooManyCookies c1 == TooManyCookies c2 = c1 == c2
  InvalidExpiration r1 == InvalidExpiration r2 = r1 == r2
  _ == _ = False

||| Cookie result type
public export
CookieResult : Type -> Type
CookieResult = Result CookieError

--------------------------------------------------------------------------------
-- Validation Options
--------------------------------------------------------------------------------

||| Cookie validation options
public export
record CookieOptions where
  constructor MkCookieOptions
  ||| Maximum name length
  maxNameLen : Nat
  ||| Maximum value length
  maxValueLen : Nat
  ||| Maximum total cookie size
  maxSize : Nat
  ||| Require Secure flag
  requireSecure : Bool
  ||| Require HttpOnly flag
  requireHttpOnly : Bool
  ||| Enforce prefix requirements
  enforcePrefixes : Bool
  ||| Allowed domains (empty = any)
  allowedDomains : List String

||| Default options
public export
defaultOptions : CookieOptions
defaultOptions = MkCookieOptions
  { maxNameLen = maxNameLength
  , maxValueLen = maxValueLength
  , maxSize = maxCookieSize
  , requireSecure = False
  , requireHttpOnly = False
  , enforcePrefixes = True
  , allowedDomains = []
  }

||| Strict options
public export
strictOptions : CookieOptions
strictOptions = MkCookieOptions
  { maxNameLen = 128
  , maxValueLen = 2048
  , maxSize = 2048
  , requireSecure = True
  , requireHttpOnly = True
  , enforcePrefixes = True
  , allowedDomains = []
  }

--------------------------------------------------------------------------------
-- Cookie Character Validation
--------------------------------------------------------------------------------

||| Valid cookie name characters (token per RFC 6265)
public export
isValidNameChar : Char -> Bool
isValidNameChar c =
  isAlphaNum c ||
  c == '!' || c == '#' || c == '$' || c == '%' || c == '&' ||
  c == '\'' || c == '*' || c == '+' || c == '-' || c == '.' ||
  c == '^' || c == '_' || c == '`' || c == '|' || c == '~'

||| Valid cookie value characters (cookie-octet per RFC 6265)
public export
isValidValueChar : Char -> Bool
isValidValueChar c =
  let code = ord c
  in code == 0x21 ||
     (code >= 0x23 && code <= 0x2B) ||
     (code >= 0x2D && code <= 0x3A) ||
     (code >= 0x3C && code <= 0x5B) ||
     (code >= 0x5D && code <= 0x7E)

||| Check for injection characters
public export
hasInjectionChar : String -> Bool
hasInjectionChar s =
  isInfixOf ";" s || isInfixOf "\r" s || isInfixOf "\n" s

