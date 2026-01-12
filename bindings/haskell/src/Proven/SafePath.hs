{- SPDX-License-Identifier: PMPL-1.0 -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Safe filesystem path operations with traversal attack prevention.
module Proven.SafePath
  ( hasTraversal
  , isSafe
  , sanitizeFilename
  , safeJoin
  ) where

import Data.List (isInfixOf, intercalate)

-- | Check if a path contains directory traversal sequences.
hasTraversal :: String -> Bool
hasTraversal path = ".." `isInfixOf` path || '~' `elem` path

-- | Check if a path is safe (no traversal attacks).
isSafe :: String -> Bool
isSafe = not . hasTraversal

-- | Sanitize a filename by removing dangerous characters.
sanitizeFilename :: String -> String
sanitizeFilename = map sanitizeChar . replaceDotDot
  where
    sanitizeChar c
      | c `elem` "/<>:\"|?*\0\\" = '_'
      | otherwise = c
    replaceDotDot [] = []
    replaceDotDot ('.':'.':xs) = '_':'_': replaceDotDot xs
    replaceDotDot (x:xs) = x : replaceDotDot xs

-- | Safely join path components, rejecting traversal attempts.
safeJoin :: String -> [String] -> Maybe String
safeJoin base parts
  | any hasTraversal parts = Nothing
  | otherwise = Just $ intercalate "/" (base : map sanitizeFilename parts)
