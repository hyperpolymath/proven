{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe HTTP header operations via libproven FFI.
--
-- CRLF injection detection and header rendering are performed by the
-- Idris 2 verified core.
module Proven.SafeHeader
  ( hasCrlf
  , isValidHeaderName
  , isDangerousHeader
  , renderHeader
  , buildCsp
  , buildHsts
  ) where

import Foreign.C.Types (CChar)
import Proven.FFI (c_proven_header_has_crlf, c_proven_header_is_valid_name,
                   c_proven_header_is_dangerous, c_proven_header_render,
                   c_proven_header_build_csp, c_proven_header_build_hsts,
                   c_proven_free_string)
import Proven.Core (withCStringLen', boolResultToBool, stringResultToMaybe)

-- | Check for CRLF injection characters in a header value.
-- Delegates to @proven_header_has_crlf@ in libproven.
hasCrlf :: String -> IO (Maybe Bool)
hasCrlf val = withCStringLen' val $ \ptr len ->
  boolResultToBool <$> c_proven_header_has_crlf ptr len

-- | Check if a header name is a valid token per RFC 7230.
-- Delegates to @proven_header_is_valid_name@ in libproven.
isValidHeaderName :: String -> IO (Maybe Bool)
isValidHeaderName name = withCStringLen' name $ \ptr len ->
  boolResultToBool <$> c_proven_header_is_valid_name ptr len

-- | Check if a header name is in the dangerous headers list.
-- Delegates to @proven_header_is_dangerous@ in libproven.
isDangerousHeader :: String -> IO (Maybe Bool)
isDangerousHeader name = withCStringLen' name $ \ptr len ->
  boolResultToBool <$> c_proven_header_is_dangerous ptr len

-- | Create a validated header string "Name: Value".
-- Delegates to @proven_header_render@ in libproven.
renderHeader :: String -> String -> IO (Maybe String)
renderHeader name value =
  withCStringLen' name $ \nPtr nLen ->
    withCStringLen' value $ \vPtr vLen -> do
      sr <- c_proven_header_render nPtr nLen vPtr vLen
      stringResultToMaybe c_proven_free_string sr

-- | Build a Content-Security-Policy header value from JSON directives.
-- Delegates to @proven_header_build_csp@ in libproven.
buildCsp :: String -> IO (Maybe String)
buildCsp directives = withCStringLen' directives $ \ptr len -> do
  sr <- c_proven_header_build_csp ptr len
  stringResultToMaybe c_proven_free_string sr

-- | Build an HSTS header value.
-- Delegates to @proven_header_build_hsts@ in libproven.
buildHsts :: Int -> Bool -> Bool -> IO (Maybe String)
buildHsts maxAge includeSubdomains preload = do
  sr <- c_proven_header_build_hsts
          (fromIntegral maxAge)
          (if includeSubdomains then 1 else 0 :: CChar)
          (if preload then 1 else 0 :: CChar)
  stringResultToMaybe c_proven_free_string sr
