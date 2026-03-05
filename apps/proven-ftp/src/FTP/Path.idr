-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- FTP Path Validation
--
-- Validates and normalises file paths to prevent directory traversal
-- attacks. A path that escapes the FTP root is rejected at parse time
-- rather than causing undefined behaviour at the filesystem level.

module FTP.Path

%default total

-- ============================================================================
-- Path validation errors
-- ============================================================================

||| Reasons a path can be rejected.
public export
data PathError : Type where
  ||| The path is empty.
  EmptyPath        : PathError
  ||| The path contains a null byte.
  NullByte         : PathError
  ||| The path attempts to escape the root via "..".
  TraversalAttempt : PathError
  ||| The path exceeds the maximum length.
  PathTooLong      : (len : Nat) -> PathError

public export
Show PathError where
  show EmptyPath          = "Empty path"
  show NullByte           = "Path contains null byte"
  show TraversalAttempt   = "Path traversal attempt (..)"
  show (PathTooLong n)    = "Path too long: " ++ show n ++ " chars"

-- ============================================================================
-- Path components
-- ============================================================================

||| Split a path into components on '/'.
public export
splitPath : String -> List String
splitPath s = filter (\x => length x > 0) (splitOn '/' s)
  where
    splitOn : Char -> String -> List String
    splitOn sep str = map pack (go (unpack str) [] [])
      where
        go : List Char -> List Char -> List (List Char) -> List (List Char)
        go []        acc parts = reverse (reverse acc :: parts)
        go (c :: cs) acc parts =
          if c == sep
            then go cs [] (reverse acc :: parts)
            else go cs (c :: acc) parts

||| Check whether a path component is the traversal marker "..".
public export
isTraversal : String -> Bool
isTraversal ".." = True
isTraversal _    = False

||| Check whether a string contains a null byte.
public export
containsNull : String -> Bool
containsNull s = any (\c => c == '\0') (unpack s)

-- ============================================================================
-- Depth tracking for traversal prevention
-- ============================================================================

||| Walk a list of path components, tracking depth below root.
||| Returns Nothing if the path ever goes above root (depth < 0).
public export
checkDepth : List String -> Nat -> Bool
checkDepth []          _     = True
checkDepth ("." :: cs) depth = checkDepth cs depth
checkDepth (".." :: _) Z     = False  -- would escape root
checkDepth (".." :: cs) (S d) = checkDepth cs d
checkDepth (_ :: cs)   depth = checkDepth cs (S depth)

-- ============================================================================
-- Validated path
-- ============================================================================

||| A validated FTP path that is known to be safe.
public export
record SafePath where
  constructor MkSafePath
  ||| The original path string.
  original   : String
  ||| The normalised components (no empty segments, no . or ..).
  components : List String
  ||| Whether the path is absolute (starts with /).
  isAbsolute : Bool

public export
Show SafePath where
  show p =
    let prefix = if p.isAbsolute then "/" else ""
    in prefix ++ joinWith "/" p.components
    where
      joinWith : String -> List String -> String
      joinWith _   []        = ""
      joinWith _   [x]       = x
      joinWith sep (x :: xs) = x ++ sep ++ joinWith sep xs

||| The maximum path length we accept.
maxPath : Nat
maxPath = 4096

||| Validate and normalise an FTP path.
||| Rejects empty paths, null bytes, traversal attempts, and overlong paths.
public export
validatePath : String -> Either PathError SafePath
validatePath s =
  if length s == 0 then Left EmptyPath
  else if length s > maxPath then Left (PathTooLong (length s))
  else if containsNull s then Left NullByte
  else
    let parts = splitPath s
        abs   = case strUncons s of
                  Just ('/', _) => True
                  _             => False
        clean = filter (\p => p /= ".") parts
    in if not (checkDepth parts 0)
         then Left TraversalAttempt
         else Right (MkSafePath s (filter (\p => p /= "..") clean) abs)

||| Resolve a path relative to a working directory.
||| If the path is absolute, the working directory is ignored.
public export
resolvePath : SafePath -> SafePath -> SafePath
resolvePath _   path@(MkSafePath _ _ True)  = path
resolvePath cwd path@(MkSafePath _ _ False) =
  MkSafePath
    (show cwd ++ "/" ++ show path)
    (cwd.components ++ path.components)
    cwd.isAbsolute

||| The root path.
public export
rootPath : SafePath
rootPath = MkSafePath "/" [] True
