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
parsePath : String -> Path
parsePath s =
  let isAbs = isPrefixOf "/" s || isWindowsAbsolute s
      cleaned = if isAbs then drop 1 s else s
      parts = filter (not . (== "")) (split pathSeparator cleaned)
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
toString : Path -> String
toString path =
  let prefix = if path.isAbsolute then "/" else ""
      body = join (singleton pathSeparator) path.segments
  in prefix ++ body

public export
Show Path where
  show = toString

public export
Eq Path where
  p1 == p2 = p1.isAbsolute == p2.isAbsolute && p1.segments == p2.segments

--------------------------------------------------------------------------------
-- Path Components
--------------------------------------------------------------------------------

||| Get file name (last segment)
public export
fileName : Path -> Maybe String
fileName path = case path.segments of
  [] => Nothing
  segs => Just (last segs)

||| Get parent directory
public export
parent : Path -> Path
parent path = case path.segments of
  [] => path
  segs => MkPath path.isAbsolute (init segs)

||| Get file extension
public export
extension : Path -> Maybe String
extension path = do
  name <- fileName path
  let parts = split extSeparator name
  case parts of
    [] => Nothing
    [_] => Nothing  -- No extension
    _ => Just (last parts)

||| Get file stem (name without extension)
public export
stem : Path -> Maybe String
stem path = do
  name <- fileName path
  let parts = split extSeparator name
  case parts of
    [] => Nothing
    [x] => Just x
    _ => Just (join (singleton extSeparator) (init parts))

||| Get all ancestors of a path
public export
ancestors : Path -> List Path
ancestors path = go path.segments []
  where
    go : List Segment -> List Path -> List Path
    go [] acc = reverse acc
    go segs acc = go (init segs) (MkPath path.isAbsolute (init segs) :: acc)

--------------------------------------------------------------------------------
-- Path Manipulation
--------------------------------------------------------------------------------

||| Join two paths
public export
(</>) : Path -> Path -> Path
(</>) base rel =
  if rel.isAbsolute
    then rel  -- Absolute path replaces base
    else MkPath base.isAbsolute (base.segments ++ rel.segments)

||| Append segment to path
public export
appendSegment : Segment -> Path -> Path
appendSegment seg path = MkPath path.isAbsolute (path.segments ++ [seg])

||| Change file extension
public export
withExtension : String -> Path -> Path
withExtension ext path =
  case path.segments of
    [] => path
    segs =>
      let name = last segs
          newName = case stem path of
                      Nothing => name ++ "." ++ ext
                      Just s => s ++ "." ++ ext
      in MkPath path.isAbsolute (init segs ++ [newName])

||| Remove file extension
public export
stripExtension : Path -> Path
stripExtension path =
  case (path.segments, stem path) of
    ([], _) => path
    (segs, Just s) => MkPath path.isAbsolute (init segs ++ [s])
    (_, Nothing) => path

--------------------------------------------------------------------------------
-- Path Normalization
--------------------------------------------------------------------------------

||| Normalize path (resolve . and .., remove redundant separators)
public export
normalize : Path -> Path
normalize path = MkPath path.isAbsolute (normalizeSegments path.segments [])
  where
    normalizeSegments : List Segment -> List Segment -> List Segment
    normalizeSegments [] acc = reverse acc
    normalizeSegments ("" :: rest) acc = normalizeSegments rest acc
    normalizeSegments ("." :: rest) acc = normalizeSegments rest acc
    normalizeSegments (".." :: rest) acc =
      case acc of
        [] => normalizeSegments rest (if path.isAbsolute then [] else [".."])
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

||| Check if path contains traversal sequences
public export
containsTraversal : Path -> Bool
containsTraversal path = any isTraversal path.segments
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
  in isPrefixOf normBase.segments resolved.segments &&
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
  all isSafeSegment path.segments
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
    isSafe c = isAlphaNum c || c `elem` unpack "-_.~"

||| Sanitize entire path
public export
sanitize : Path -> Path
sanitize path =
  let cleaned = filter (not . (== "")) (map sanitizeSegment path.segments)
      noTraversal = filter (\s => s /= ".." && s /= ".") cleaned
  in MkPath path.isAbsolute noTraversal

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
  if null path.segments && not path.isAbsolute
    then Left EmptyPath
    else if containsTraversal path
      then Left TraversalDetected
      else if any hasNullByte path.segments
        then Left NullByteDetected
        else if any (\s => length s > maxSegmentLength) path.segments
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
startsWith prefix path =
  prefix.isAbsolute == path.isAbsolute &&
  isPrefixOf prefix.segments path.segments

||| Check if path ends with suffix
public export
endsWith : Path -> Path -> Bool
endsWith suffix path = isSuffixOf suffix.segments path.segments

||| Get relative path from base to target
public export
relativeTo : (base : Path) -> (target : Path) -> Maybe Path
relativeTo base target =
  if base.isAbsolute /= target.isAbsolute
    then Nothing
    else let normBase = (normalize base).segments
             normTarget = (normalize target).segments
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
