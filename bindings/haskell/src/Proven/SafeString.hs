{- SPDX-License-Identifier: Apache-2.0 -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Safe string operations for security-sensitive contexts.
module Proven.SafeString
  ( escapeHtml
  , escapeSql
  , escapeJs
  , escapeUrl
  , truncateSafe
  ) where

import Data.Char (ord)
import Numeric (showHex)

-- | Escape HTML special characters to prevent XSS attacks.
escapeHtml :: String -> String
escapeHtml = concatMap escapeChar
  where
    escapeChar '&'  = "&amp;"
    escapeChar '<'  = "&lt;"
    escapeChar '>'  = "&gt;"
    escapeChar '"'  = "&quot;"
    escapeChar '\'' = "&#x27;"
    escapeChar c    = [c]

-- | Escape single quotes for SQL strings.
escapeSql :: String -> String
escapeSql = concatMap escapeChar
  where
    escapeChar '\'' = "''"
    escapeChar c    = [c]

-- | Escape JavaScript special characters.
escapeJs :: String -> String
escapeJs = concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar '\'' = "\\'"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c    = [c]

-- | URL-encode a string.
escapeUrl :: String -> String
escapeUrl = concatMap escapeChar
  where
    escapeChar c
      | isUnreserved c = [c]
      | otherwise      = '%' : pad2 (showHex (ord c) "")
    isUnreserved c = c `elem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_.~")
    pad2 [x] = ['0', x]
    pad2 xs  = xs

-- | Safely truncate a string with a suffix.
truncateSafe :: String -> Int -> String -> String
truncateSafe value maxLen suffix
  | length value <= maxLen = value
  | maxLen <= length suffix = take maxLen suffix
  | otherwise = take (maxLen - length suffix) value ++ suffix
