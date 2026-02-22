{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe string operations via libproven FFI.
--
-- All escaping and validation is performed by the Idris 2 verified core.
-- This module only marshals strings to/from the C ABI.
module Proven.SafeString
  ( isValidUtf8
  , escapeHtml
  , escapeSql
  , escapeJs
  ) where

import qualified Data.ByteString as BS
import Proven.FFI (c_proven_string_is_valid_utf8, c_proven_string_escape_html,
                   c_proven_string_escape_sql, c_proven_string_escape_js,
                   c_proven_free_string)
import Proven.Core (withCStringLen', withByteString, boolResultToBool,
                    stringResultToMaybe)

-- | Check if a byte string is valid UTF-8.
-- Delegates to @proven_string_is_valid_utf8@ in libproven.
isValidUtf8 :: BS.ByteString -> IO (Maybe Bool)
isValidUtf8 bs = withByteString bs $ \ptr len ->
  boolResultToBool <$> c_proven_string_is_valid_utf8 ptr len

-- | Escape HTML special characters to prevent XSS.
-- Delegates to @proven_string_escape_html@ in libproven.
escapeHtml :: String -> IO (Maybe String)
escapeHtml str = withCStringLen' str $ \ptr len -> do
  sr <- c_proven_string_escape_html ptr len
  stringResultToMaybe c_proven_free_string sr

-- | Escape single quotes for SQL strings.
-- Delegates to @proven_string_escape_sql@ in libproven.
escapeSql :: String -> IO (Maybe String)
escapeSql str = withCStringLen' str $ \ptr len -> do
  sr <- c_proven_string_escape_sql ptr len
  stringResultToMaybe c_proven_free_string sr

-- | Escape JavaScript special characters.
-- Delegates to @proven_string_escape_js@ in libproven.
escapeJs :: String -> IO (Maybe String)
escapeJs str = withCStringLen' str $ \ptr len -> do
  sr <- c_proven_string_escape_js ptr len
  stringResultToMaybe c_proven_free_string sr
