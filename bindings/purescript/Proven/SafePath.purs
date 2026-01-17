-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe file path operations with traversal prevention.
-- |
-- | Validates paths to prevent directory traversal attacks and ensures
-- | paths stay within allowed boundaries.

module Proven.SafePath
  ( SafePath
  , isSafePath
  , hasTraversal
  , normalize
  , join
  , isAbsolute
  , isRelative
  , getExtension
  , getBasename
  , getDirname
  , requireSafePath
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, lastIndexOf, drop, take, length)
import Data.String as S
import Proven.Result (Result(..), ProvenError(..))

-- | SafePath namespace marker (not instantiated).
data SafePath

-- | Check if a path is safe (no traversal attempts).
-- | Returns false if path contains ".." or other traversal patterns.
isSafePath :: String -> Boolean
isSafePath path = not (hasTraversal path)

-- | Check if path contains traversal patterns.
-- | Detects "..", null bytes, and other attack patterns.
hasTraversal :: String -> Boolean
hasTraversal path =
  contains (Pattern "..") path ||
  contains (Pattern "\x00") path ||
  contains (Pattern "//") path && contains (Pattern ":") path

-- | Normalize a path by resolving . and removing redundant separators.
-- | Does not resolve .. for safety - those should be rejected.
normalize :: String -> String
normalize path = normalizeImpl path

foreign import normalizeImpl :: String -> String

-- | Join two path components safely.
-- | Ensures the second component cannot escape the first via traversal.
join :: String -> String -> Result String ProvenError
join base child
  | hasTraversal child = Err PathTraversal
  | otherwise = Ok (joinImpl base child)

foreign import joinImpl :: String -> String -> String

-- | Check if path is absolute.
isAbsolute :: String -> Boolean
isAbsolute path = isAbsoluteImpl path

foreign import isAbsoluteImpl :: String -> Boolean

-- | Check if path is relative.
isRelative :: String -> Boolean
isRelative path = not (isAbsolute path)

-- | Get the file extension from a path.
-- | Returns Nothing if no extension is present.
getExtension :: String -> Maybe String
getExtension path =
  case lastIndexOf (Pattern ".") path of
    Nothing -> Nothing
    Just idx ->
      let ext = drop (idx + 1) path
      in if length ext > 0 && not (contains (Pattern "/") ext)
         then Just ext
         else Nothing

-- | Get the basename (filename) from a path.
getBasename :: String -> String
getBasename path = basenameImpl path

foreign import basenameImpl :: String -> String

-- | Get the directory name from a path.
getDirname :: String -> String
getDirname path = dirnameImpl path

foreign import dirnameImpl :: String -> String

-- | Require a safe path or return error.
requireSafePath :: String -> Result String ProvenError
requireSafePath path
  | isSafePath path = Ok path
  | otherwise = Err PathTraversal
