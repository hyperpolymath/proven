-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafePath - File path operations that cannot crash or allow traversal attacks
|||
||| This module provides:
||| - Safe path parsing and manipulation
||| - Path traversal attack prevention
||| - Cross-platform path handling
||| - Path normalization and validation
module Proven.SafePath

import public Proven.Core
import public Proven.SafePath.Types
import public Proven.SafePath.Operations

import Data.List
import Data.List1
import Data.String

%default total

--------------------------------------------------------------------------------
-- Path Types
--------------------------------------------------------------------------------

||| Path separator for current platform
public export
pathSeparator : Char
pathSeparator = '/'  -- Unix-style default, Windows uses '\\'

||| Extension separator
public export
extSeparator : Char
extSeparator = '.'

||| A path segment (single directory or file name)
public export
Segment : Type
Segment = String

||| A file system path represented as segments
public export
record Path where
  constructor MkPath
  isAbsolute : Bool
  segments : List Segment

--------------------------------------------------------------------------------
-- Path Construction
--------------------------------------------------------------------------------

||| Create an empty relative path
public export
emptyPath : Path
emptyPath = MkPath False []

||| Create a root path
public export
rootPath : Path
rootPath = MkPath True []

||| Create path from single segment
public export
fromSegment : Segment -> Path
fromSegment seg = MkPath False [seg]

||| Parse string to path
public export
partial
parsePath : String -> Path
parsePath s =
  let isAbs = isPrefixOf "/" s || isWindowsAbsolute s
      cleaned = if isAbs then assert_total (strTail s) else s
      parts = filter (not . (== "")) (forget (split (== pathSeparator) cleaned))
  in MkPath isAbs parts
  where
    isWindowsAbsolute : String -> Bool
    isWindowsAbsolute str = length str >= 2 &&
                            isAlpha (assert_total $ prim__strHead str) &&
                            prim__strIndex str 1 == ':'

||| Create path from segments
public export
fromSegments : Bool -> List Segment -> Path
fromSegments isAbs segs = MkPath isAbs (filter (not . (== "")) segs)

--------------------------------------------------------------------------------
-- Path Display
--------------------------------------------------------------------------------

||| Convert path to string
public export
partial
toString : Path -> String
toString (MkPath abs segs) =
  (if abs then "/" else "") ++ fastConcat (intersperse (singleton pathSeparator) segs)

public export
Show Path where
  show = toString

public export
Eq Path where
  p1 == p2 = isAbsolute p1 == isAbsolute p2 && segments p1 == segments p2

--------------------------------------------------------------------------------
-- Path Components
--------------------------------------------------------------------------------

||| Get file name (last segment)
public export
fileName : Path -> Maybe String
fileName path = case segments path of
  [] => Nothing
  (s :: ss) => Just (last (s :: ss))

||| Get parent directory
public export
parent : Path -> Path
parent path = case segments path of
  [] => path
  (s :: ss) => MkPath (isAbsolute path) (init (s :: ss))

||| Get file extension
public export
extension : Path -> Maybe String
extension path = do
  name <- fileName path
  let parts = forget (split (== extSeparator) name)
  case parts of
    [] => Nothing
    [_] => Nothing  -- No extension
    (p :: ps) => case ps of
                   [] => Nothing
                   _ => Just (last (p :: ps))

||| Get file stem (name without extension)
public export
stem : Path -> Maybe String
stem path = do
  name <- fileName path
  let parts = forget (split (== extSeparator) name)
  case parts of
    [] => Nothing
    [x] => Just x
    (p :: ps) => case ps of
                   [] => Just p
                   _ => Just (fastConcat (intersperse (singleton extSeparator) (init (p :: ps))))

||| Get all ancestors of a path
public export
partial
ancestors : Path -> List Path
ancestors path = go (segments path) []
  where
    go : List Segment -> List Path -> List Path
    go [] acc = reverse acc
    go (s :: ss) acc = case ss of
                         [] => reverse acc
                         _ => go (init (s :: ss)) (MkPath (isAbsolute path) (init (s :: ss)) :: acc)

--------------------------------------------------------------------------------
-- Path Manipulation
--------------------------------------------------------------------------------

infixr 5 </>

||| Join two paths
public export
(</>) : Path -> Path -> Path
(</>) base rel =
  if isAbsolute rel
    then rel  -- Absolute path replaces base
    else MkPath (isAbsolute base) (segments base ++ segments rel)

||| Append segment to path
public export
appendSegment : Segment -> Path -> Path
appendSegment seg path = MkPath (isAbsolute path) (segments path ++ [seg])

||| Change file extension
public export
withExtension : String -> Path -> Path
withExtension ext path =
  case segments path of
    [] => path
    (s :: ss) =>
      let name = last (s :: ss)
          newName = case stem path of
                      Nothing => name ++ "." ++ ext
                      Just s => s ++ "." ++ ext
      in MkPath (isAbsolute path) (init (s :: ss) ++ [newName])

||| Remove file extension
public export
stripExtension : Path -> Path
stripExtension path =
  case (segments path, stem path) of
    ([], _) => path
    ((seg :: segs), Just s) => MkPath (isAbsolute path) (init (seg :: segs) ++ [s])
    (_, Nothing) => path

--------------------------------------------------------------------------------
-- Path Normalization
--------------------------------------------------------------------------------

||| Normalize path (resolve . and .., remove redundant separators)
public export
normalize : Path -> Path
normalize path = MkPath (isAbsolute path) (normalizeSegments (segments path) [])
  where
    normalizeSegments : List Segment -> List Segment -> List Segment
    normalizeSegments [] acc = reverse acc
    normalizeSegments ("" :: rest) acc = normalizeSegments rest acc
    normalizeSegments ("." :: rest) acc = normalizeSegments rest acc
    normalizeSegments (".." :: rest) acc =
      case acc of
        [] => normalizeSegments rest (if isAbsolute path then [] else [".."])
        (".." :: _) => normalizeSegments rest (".." :: acc)
        (_ :: accRest) => normalizeSegments rest accRest
    normalizeSegments (seg :: rest) acc = normalizeSegments rest (seg :: acc)

||| Canonicalize path (normalize + resolve symlinks would require IO)
||| For pure version, just normalizes
public export
canonicalize : Path -> Path
canonicalize = normalize

--------------------------------------------------------------------------------
-- Path Safety
--------------------------------------------------------------------------------

||| Check if path isInfixOf traversal sequences
public export
containsTraversal : Path -> Bool
containsTraversal path = any isTraversal (segments path)
  where
    isTraversal : Segment -> Bool
    isTraversal ".." = True
    isTraversal s = isInfixOf ".." s || isInfixOf "/" s || isInfixOf "\\" s

||| Check if path is contained within a base path (no escape)
public export
isContainedIn : (base : Path) -> (path : Path) -> Bool
isContainedIn base path =
  let normBase = normalize base
      normPath = normalize path
      resolved = normalize (base </> path)
  in isPrefixOf (segments normBase) (segments resolved) &&
     not (containsTraversal resolved)

||| Safe path joining that prevents traversal
public export
safeJoin : (base : Path) -> (rel : Path) -> Maybe Path
safeJoin base rel =
  let result = normalize (base </> rel)
  in if isContainedIn base result
       then Just result
       else Nothing

||| Check if path is safe (no null bytes, traversal, etc.)
public export
isSafePath : Path -> Bool
isSafePath path =
  not (containsTraversal path) &&
  all isSafeSegment (segments path)
  where
    isSafeSegment : Segment -> Bool
    isSafeSegment seg =
      not (null seg) &&
      not (any (\c => c == '\0' || c == '\n' || c == '\r') (unpack seg))

||| Sanitize a path segment (remove dangerous characters)
public export
sanitizeSegment : Segment -> Segment
sanitizeSegment seg = pack (filter isSafe (unpack seg))
  where
    isSafe : Char -> Bool
    isSafe c = isAlphaNum c || elem c (unpack "-_.~")

||| Sanitize entire path
public export
sanitize : Path -> Path
sanitize path =
  let cleaned = filter (not . (== "")) (map Proven.SafePath.sanitizeSegment (segments path))
      noTraversal = filter (\s => s /= ".." && s /= ".") cleaned
  in MkPath (isAbsolute path) noTraversal

--------------------------------------------------------------------------------
-- Path Validation
--------------------------------------------------------------------------------

||| Path validation errors
public export
data PathError
  = EmptyPath
  | TraversalDetected
  | InvalidCharacter Char
  | SegmentTooLong Nat
  | PathTooLong Nat
  | NullByteDetected

public export
Show PathError where
  show EmptyPath = "Empty path"
  show TraversalDetected = "Path traversal detected"
  show (InvalidCharacter c) = "Invalid character: " ++ show c
  show (SegmentTooLong n) = "Segment too long: " ++ show n ++ " bytes"
  show (PathTooLong n) = "Path too long: " ++ show n ++ " bytes"
  show NullByteDetected = "Null byte detected in path"

||| Maximum segment length (most filesystems)
public export
maxSegmentLength : Nat
maxSegmentLength = 255

||| Maximum path length (common limit)
public export
maxPathLength : Nat
maxPathLength = 4096

||| Validate path
public export
validate : Path -> Either PathError Path
validate path =
  if null (segments path) && not (isAbsolute path)
    then Left EmptyPath
    else if containsTraversal path
      then Left TraversalDetected
      else if any hasNullByte (segments path)
        then Left NullByteDetected
        else if any (\s => length s > maxSegmentLength) (segments path)
          then Left (SegmentTooLong maxSegmentLength)
          else if length (toString path) > maxPathLength
            then Left (PathTooLong maxPathLength)
            else Right path
  where
    hasNullByte : Segment -> Bool
    hasNullByte seg = '\0' `elem` unpack seg

--------------------------------------------------------------------------------
-- Path Comparison
--------------------------------------------------------------------------------

||| Check if path starts with prefix
public export
startsWith : Path -> Path -> Bool
startsWith pfx path =
  isAbsolute pfx == isAbsolute path &&
  isPrefixOf (segments pfx) (segments path)

||| Check if path ends with suffix
public export
endsWith : Path -> Path -> Bool
endsWith suffix path = isSuffixOf (segments suffix) (segments path)

||| Get relative path from base to target
public export
relativeTo : (base : Path) -> (target : Path) -> Maybe Path
relativeTo base target =
  if isAbsolute base /= isAbsolute target
    then Nothing
    else let normBase = segments (normalize base)
             normTarget = segments (normalize target)
         in Just (MkPath False (stripPrefix normBase normTarget))
  where
    stripPrefix : List Segment -> List Segment -> List Segment
    stripPrefix [] ys = ys
    stripPrefix _ [] = []
    stripPrefix (x :: xs) (y :: ys) =
      if x == y then stripPrefix xs ys else ".." :: (y :: ys)

--------------------------------------------------------------------------------
-- Common Paths
--------------------------------------------------------------------------------

||| Current directory
public export
currentDir : Path
currentDir = MkPath False ["."]

||| Parent directory
public export
parentDir : Path
parentDir = MkPath False [".."]

||| Home directory placeholder
public export
homeDir : Path
homeDir = MkPath False ["~"]

||| Temporary directory
public export
tempDir : Path
tempDir = MkPath True ["tmp"]
