-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeCookie operations
|||
||| This module exports safe cookie handling to the C ABI via Idris2's RefC backend.
||| All functions are proven total and prevent cookie injection attacks.
|||
||| Return conventions:
||| - CookieResult Cookie → (status: Int, setCookieHeader: String)
|||   - status = 0: Success, setCookieHeader is valid
|||   - status = 1: Error, setCookieHeader contains error message
||| - CookieResult Cookies → (status: Int, cookieCount: Int, error: String)
||| - Bool → Int (0 = false, 1 = true)
||| - SameSite → Int (0 = None, 1 = Lax, 2 = Strict)
||| - CookiePrefix → Int (0 = NoPrefix, 1 = SecurePrefix, 2 = HostPrefix)
module Proven.FFI.SafeCookie

import Proven.SafeCookie
import Proven.SafeCookie.Types
import Proven.SafeCookie.Parser
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode SameSite as Int
encodeSameSite : SameSite -> Int
encodeSameSite None = 0
encodeSameSite Lax = 1
encodeSameSite Strict = 2

||| Decode Int to SameSite
decodeSameSite : Int -> Maybe SameSite
decodeSameSite 0 = Just None
decodeSameSite 1 = Just Lax
decodeSameSite 2 = Just Strict
decodeSameSite _ = Nothing

||| Encode CookiePrefix as Int
encodeCookiePrefix : CookiePrefix -> Int
encodeCookiePrefix NoPrefix = 0
encodeCookiePrefix SecurePrefix = 1
encodeCookiePrefix HostPrefix = 2

||| Encode CookieResult Cookie as (status, setCookieHeader or error)
encodeCookieResult : CookieResult Cookie -> (Int, String)
encodeCookieResult (Err err) = (1, friendlyError err)
encodeCookieResult (Ok cookie) = (0, buildSetCookie cookie)

||| Encode CookieResult Cookies as (status, count, error)
encodeCookiesResult : CookieResult Cookies -> (Int, Int, String)
encodeCookiesResult (Err err) = (1, 0, friendlyError err)
encodeCookiesResult (Ok cookies) = (0, cast (length cookies), "")

||| Decode Int to Nat (clamp negative)
decodeNat : Int -> Nat
decodeNat n = if n < 0 then Z else fromInteger (cast n)

--------------------------------------------------------------------------------
-- Cookie Creation
--------------------------------------------------------------------------------

export
proven_idris_cookie_make_default : String -> String -> (Int, String)
proven_idris_cookie_make_default name value =
  encodeCookieResult (mkCookieDefault name value)

export
proven_idris_cookie_make_session : String -> String -> (Int, String)
proven_idris_cookie_make_session name value =
  encodeCookieResult (mkSessionCookie name value)

export
proven_idris_cookie_make_strict : String -> String -> (Int, String)
proven_idris_cookie_make_strict name value =
  encodeCookieResult (mkStrictCookie name value)

export
proven_idris_cookie_make_auth : String -> String -> Integer -> (Int, String)
proven_idris_cookie_make_auth name value maxAge =
  encodeCookieResult (authCookie name value maxAge)

export
proven_idris_cookie_make_csrf : String -> String -> (Int, String)
proven_idris_cookie_make_csrf name value =
  encodeCookieResult (csrfCookie name value)

export
proven_idris_cookie_make_remember : String -> String -> (Int, String)
proven_idris_cookie_make_remember name value =
  encodeCookieResult (rememberMeCookie name value)

--------------------------------------------------------------------------------
-- Cookie Parsing
--------------------------------------------------------------------------------

export
proven_idris_cookie_parse : String -> (Int, Int, String)
proven_idris_cookie_parse header =
  encodeCookiesResult (parseCookieHeaderDefault header)

--------------------------------------------------------------------------------
-- Building and Deletion
--------------------------------------------------------------------------------

export
proven_idris_cookie_build_delete : String -> String
proven_idris_cookie_build_delete name =
  buildDeleteCookie name

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

export
proven_idris_cookie_validate_name : String -> (Int, String)
proven_idris_cookie_validate_name name =
  case validateNameDefault name of
    Err err => (1, friendlyError err)
    Ok cookieName => (0, show cookieName)

export
proven_idris_cookie_validate_value : String -> String -> (Int, String)
proven_idris_cookie_validate_value name value =
  case validateValueDefault name value of
    Err err => (1, friendlyError err)
    Ok cookieValue => (0, show cookieValue)

export
proven_idris_cookie_validate_domain : String -> (Int, String)
proven_idris_cookie_validate_domain domain =
  case validateDomain defaultOptions domain of
    Err err => (1, friendlyError err)
    Ok d => (0, d)

export
proven_idris_cookie_validate_path : String -> (Int, String)
proven_idris_cookie_validate_path path =
  case validatePath path of
    Err err => (1, friendlyError err)
    Ok p => (0, p)

--------------------------------------------------------------------------------
-- Security Checks
--------------------------------------------------------------------------------

export
proven_idris_cookie_get_prefix : String -> Int
proven_idris_cookie_get_prefix name =
  encodeCookiePrefix (getPrefix name)

export
proven_idris_cookie_is_host_prefixed : String -> Int
proven_idris_cookie_is_host_prefixed name =
  encodeBool (isHostPrefixed name)

export
proven_idris_cookie_is_secure_prefixed : String -> Int
proven_idris_cookie_is_secure_prefixed name =
  encodeBool (isSecurePrefixed name)

export
proven_idris_cookie_has_prefix_security : String -> Int
proven_idris_cookie_has_prefix_security name =
  encodeBool (hasPrefixSecurity name)

--------------------------------------------------------------------------------
-- Expiration Presets
--------------------------------------------------------------------------------

export
proven_idris_cookie_hour : Integer
proven_idris_cookie_hour = oneHour

export
proven_idris_cookie_day : Integer
proven_idris_cookie_day = oneDay

export
proven_idris_cookie_week : Integer
proven_idris_cookie_week = oneWeek

export
proven_idris_cookie_month : Integer
proven_idris_cookie_month = thirtyDays

export
proven_idris_cookie_year : Integer
proven_idris_cookie_year = oneYear

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

export
proven_idris_cookie_max_name_length : Int
proven_idris_cookie_max_name_length = cast maxNameLength

export
proven_idris_cookie_max_value_length : Int
proven_idris_cookie_max_value_length = cast maxValueLength

export
proven_idris_cookie_max_cookie_size : Int
proven_idris_cookie_max_cookie_size = cast maxCookieSize

export
proven_idris_cookie_max_per_domain : Int
proven_idris_cookie_max_per_domain = cast maxCookiesPerDomain
