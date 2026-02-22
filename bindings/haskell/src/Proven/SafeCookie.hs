{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe HTTP cookie operations via libproven FFI.
--
-- Cookie injection detection and Set-Cookie building are performed by the
-- Idris 2 verified core.
module Proven.SafeCookie
  ( hasInjection
  , validateCookieName
  , validateCookieValue
  , getCookiePrefix
  , buildDeleteCookie
  ) where

import Proven.FFI (c_proven_cookie_has_injection, c_proven_cookie_validate_name,
                   c_proven_cookie_validate_value, c_proven_cookie_get_prefix,
                   c_proven_cookie_build_delete, c_proven_free_string)
import Proven.Core (withCStringLen', boolResultToBool, intResultToMaybe,
                    stringResultToMaybe)

-- | Check for cookie injection characters (semicolon, CR, LF).
-- Delegates to @proven_cookie_has_injection@ in libproven.
hasInjection :: String -> IO (Maybe Bool)
hasInjection val = withCStringLen' val $ \ptr len ->
  boolResultToBool <$> c_proven_cookie_has_injection ptr len

-- | Validate a cookie name.
-- Delegates to @proven_cookie_validate_name@ in libproven.
validateCookieName :: String -> IO (Maybe Bool)
validateCookieName name = withCStringLen' name $ \ptr len ->
  boolResultToBool <$> c_proven_cookie_validate_name ptr len

-- | Validate a cookie value.
-- Delegates to @proven_cookie_validate_value@ in libproven.
validateCookieValue :: String -> IO (Maybe Bool)
validateCookieValue val = withCStringLen' val $ \ptr len ->
  boolResultToBool <$> c_proven_cookie_validate_value ptr len

-- | Get cookie prefix type (0=none, 1=__Secure-, 2=__Host-).
-- Delegates to @proven_cookie_get_prefix@ in libproven.
getCookiePrefix :: String -> IO (Maybe Int)
getCookiePrefix name = withCStringLen' name $ \ptr len ->
  intResultToMaybe <$> c_proven_cookie_get_prefix ptr len

-- | Build a delete cookie header value.
-- Delegates to @proven_cookie_build_delete@ in libproven.
buildDeleteCookie :: String -> IO (Maybe String)
buildDeleteCookie name = withCStringLen' name $ \ptr len -> do
  sr <- c_proven_cookie_build_delete ptr len
  stringResultToMaybe c_proven_free_string sr
