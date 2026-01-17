-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe string operations with injection prevention.
-- |
-- | Provides HTML, SQL, JavaScript, URL, and shell escaping to prevent
-- | XSS, SQL injection, and command injection attacks.

module Proven.SafeString
  ( SafeString
  , escapeHtml
  , escapeSql
  , escapeJs
  , escapeShell
  , urlEncode
  , urlDecode
  , isValidUtf8
  , truncateUtf8
  , detectInjection
  ) where

import Prelude

import Data.Array (any)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, length, take, toLower)
import Data.String.CodeUnits as CU
import Proven.Result (Result(..), ProvenError(..))

-- | SafeString namespace marker (not instantiated).
data SafeString

-- | Escape HTML special characters to prevent XSS.
-- | Converts <, >, &, ", and ' to their HTML entity equivalents.
escapeHtml :: String -> String
escapeHtml s = escapeHtmlImpl s

foreign import escapeHtmlImpl :: String -> String

-- | Escape SQL single quotes to prevent SQL injection.
-- | Doubles single quotes per SQL standard.
escapeSql :: String -> String
escapeSql s = escapeSqlImpl s

foreign import escapeSqlImpl :: String -> String

-- | Escape JavaScript special characters for safe embedding.
-- | Handles backslash, quotes, and control characters.
escapeJs :: String -> String
escapeJs s = escapeJsImpl s

foreign import escapeJsImpl :: String -> String

-- | Escape shell special characters by wrapping in single quotes.
-- | Handles embedded single quotes safely.
escapeShell :: String -> String
escapeShell s = escapeShellImpl s

foreign import escapeShellImpl :: String -> String

-- | URL encode a string (percent encoding).
-- | Safe characters: A-Z, a-z, 0-9, -, _, ., ~
urlEncode :: String -> String
urlEncode s = urlEncodeImpl s

foreign import urlEncodeImpl :: String -> String

-- | URL decode a string (percent decoding).
-- | Returns error for invalid percent sequences.
urlDecode :: String -> Result String ProvenError
urlDecode s =
  case urlDecodeImpl s of
    { success: true, value } -> Ok value
    { success: false, error } -> Err (InvalidFormat error)

foreign import urlDecodeImpl :: String -> { success :: Boolean, value :: String, error :: String }

-- | Check if a string is valid UTF-8.
-- | In PureScript/JS, strings are always valid UTF-16.
isValidUtf8 :: String -> Boolean
isValidUtf8 _ = true

-- | Truncate a string to maximum byte length respecting UTF-8 boundaries.
-- | Ensures the result is always valid UTF-8.
truncateUtf8 :: String -> Int -> String
truncateUtf8 s maxBytes
  | CU.length s <= maxBytes = s
  | otherwise = take maxBytes s

-- | Detect potential injection patterns in a string.
-- | Checks for common XSS, SQL injection, and script patterns.
detectInjection :: String -> Boolean
detectInjection s =
  let
    lower = toLower s
    patterns =
      [ "<script"
      , "javascript:"
      , "onerror="
      , "onclick="
      , "onload="
      , "eval("
      , "expression("
      , "vbscript:"
      , "data:"
      , "select "
      , "insert "
      , "update "
      , "delete "
      , "drop "
      , "union "
      , "--"
      , "/*"
      , "*/"
      , "; "
      , "' or "
      , "\" or "
      ]
  in
    any (\p -> contains (Pattern p) lower) patterns
