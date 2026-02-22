-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafeString - FFI bindings to libproven string escaping operations
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafeString
  ( escapeHtml
  , escapeSql
  , escapeJs
  , isValidUtf8
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))

-- | Escape HTML special characters to prevent XSS (delegates to Idris 2).
-- | Converts <, >, &, ", and ' to their HTML entity equivalents.
foreign import escapeHtmlImpl :: String -> String

escapeHtml :: String -> String
escapeHtml = escapeHtmlImpl

-- | Escape SQL single quotes to prevent SQL injection (delegates to Idris 2).
-- | Doubles single quotes per SQL standard.
foreign import escapeSqlImpl :: String -> String

escapeSql :: String -> String
escapeSql = escapeSqlImpl

-- | Escape JavaScript special characters for safe embedding (delegates to Idris 2).
-- | Handles backslash, quotes, and control characters.
foreign import escapeJsImpl :: String -> String

escapeJs :: String -> String
escapeJs = escapeJsImpl

-- | Check if a string is valid UTF-8 (delegates to Idris 2).
foreign import isValidUtf8Impl :: String -> Boolean

isValidUtf8 :: String -> Boolean
isValidUtf8 = isValidUtf8Impl
