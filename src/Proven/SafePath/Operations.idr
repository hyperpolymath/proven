-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Safe Path Operations
|||
||| Operations for manipulating file paths safely.
module Proven.SafePath.Operations

import Proven.Core
import Proven.SafePath.Types
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Segment Operations
--------------------------------------------------------------------------------

||| Check if segment is safe (no dangerous characters)
public export
isSafeSegment : String -> Bool
isSafeSegment "" = False
isSafeSegment seg =
  not (seg == ".") &&
  not (seg == "..") &&
  all isSafeChar (unpack seg)
  where
    isSafeChar : Char -> Bool
    isSafeChar c = c /= '\0' && c /= '/' && c /= '\\' && c /= '\n' && c /= '\r'

||| Sanitize a single segment
public export
sanitizeSegment : String -> String
sanitizeSegment seg =
  let cleaned = filter isSafe (unpack seg)
  in if null cleaned then "_" else pack cleaned
  where
    isSafe : Char -> Bool
    isSafe c = isAlphaNum c || c `elem` unpack "-_.~"

||| Split path string into segments
public export
splitPath : String -> List String
splitPath s =
  let chars = unpack s
      -- Handle both Unix and Windows separators
      normalized = map (\c => if c == '\\' then '/' else c) chars
  in filter (not . (== "")) (split '/' (pack normalized))

||| Join segments into path string
public export
joinSegments : List String -> String
joinSegments [] = ""
joinSegments segs = join "/" segs

--------------------------------------------------------------------------------
-- Path Normalization Operations
--------------------------------------------------------------------------------

||| Remove redundant separators
public export
removeDuplicateSeparators : String -> String
removeDuplicateSeparators s = pack (go (unpack s) False)
  where
    isSep : Char -> Bool
    isSep c = c == '/' || c == '\\'

    go : List Char -> Bool -> List Char
    go [] _ = []
    go (c :: cs) lastWasSep =
      if isSep c
        then if lastWasSep then go cs True else '/' :: go cs True
        else c :: go cs False

||| Resolve . and .. in segment list
public export
resolveDotsInSegments : List String -> Bool -> List String
resolveDotsInSegments segs isAbs = reverse (go segs [])
  where
    go : List String -> List String -> List String
    go [] acc = acc
    go ("." :: rest) acc = go rest acc
    go (".." :: rest) acc =
      case acc of
        [] => if isAbs then go rest [] else go rest [".."]
        (".." :: _) => go rest (".." :: acc)
        (_ :: accRest) => go rest accRest
    go (seg :: rest) acc = go rest (seg :: acc)

||| Full path normalization
public export
normalizePath : String -> String
normalizePath s =
  let isAbs = isPrefixOf "/" s || isWindowsAbs s
      segs = splitPath s
      resolved = resolveDotsInSegments segs isAbs
      prefix = if isAbs then "/" else ""
  in prefix ++ joinSegments resolved
  where
    isWindowsAbs : String -> Bool
    isWindowsAbs str =
      length str >= 2 && isAlpha (assert_total $ prim__strHead str) && prim__strIndex str 1 == ':'

--------------------------------------------------------------------------------
-- Path Comparison Operations
--------------------------------------------------------------------------------

||| Compare paths case-sensitively (Unix)
public export
pathEqSensitive : String -> String -> Bool
pathEqSensitive p1 p2 = normalizePath p1 == normalizePath p2

||| Compare paths case-insensitively (Windows)
public export
pathEqInsensitive : String -> String -> Bool
pathEqInsensitive p1 p2 = toLower (normalizePath p1) == toLower (normalizePath p2)

||| Check if path1 is parent of path2
public export
isParentOf : String -> String -> Bool
isParentOf parent child =
  let normParent = splitPath (normalizePath parent)
      normChild = splitPath (normalizePath child)
  in isPrefixOf normParent normChild && length normParent < length normChild

||| Check if path1 is ancestor of path2
public export
isAncestorOf : String -> String -> Bool
isAncestorOf ancestor descendant =
  let normAncestor = splitPath (normalizePath ancestor)
      normDescendant = splitPath (normalizePath descendant)
  in isPrefixOf normAncestor normDescendant

--------------------------------------------------------------------------------
-- Extension Operations
--------------------------------------------------------------------------------

||| Get extension from file name
public export
getExtension : String -> Maybe String
getExtension s =
  let name = last (splitPath s)
      parts = split '.' name
  in case parts of
       [] => Nothing
       [_] => Nothing
       _ => Just (last parts)

||| Get all extensions (for .tar.gz etc.)
public export
getAllExtensions : String -> List String
getAllExtensions s =
  let name = last (splitPath s)
      parts = split '.' name
  in case parts of
       [] => []
       [_] => []
       (_ :: exts) => exts

||| Change extension
public export
changeExtension : String -> String -> String
changeExtension path newExt =
  let segs = splitPath path
  in case segs of
       [] => ""
       _ =>
         let name = last segs
             base = case split '.' name of
                      [] => name
                      [x] => x
                      (x :: _) => x
             newName = if newExt == "" then base else base ++ "." ++ newExt
         in joinSegments (init segs ++ [newName])

||| Add extension (doesn't replace existing)
public export
addExtension : String -> String -> String
addExtension path ext =
  let segs = splitPath path
  in case segs of
       [] => ""
       _ =>
         let name = last segs
             newName = name ++ "." ++ ext
         in joinSegments (init segs ++ [newName])

||| Strip extension
public export
stripExtension : String -> String
stripExtension path =
  let segs = splitPath path
  in case segs of
       [] => ""
       _ =>
         let name = last segs
             base = case split '.' name of
                      [] => name
                      [x] => x
                      parts => join "." (init parts)
         in joinSegments (init segs ++ [base])

--------------------------------------------------------------------------------
-- Safe Path Construction
--------------------------------------------------------------------------------

||| Safely join two paths, preventing traversal
public export
safeJoinPaths : String -> String -> Maybe String
safeJoinPaths base rel =
  let normBase = normalizePath base
      combined = normBase ++ "/" ++ rel
      normCombined = normalizePath combined
      baseSegs = splitPath normBase
      combinedSegs = splitPath normCombined
  in if isPrefixOf baseSegs combinedSegs
       then Just normCombined
       else Nothing

||| Create a contained path
public export
makeContainedPath : (base : String) -> (rel : String) -> Maybe (ContainedPath base)
makeContainedPath base rel =
  case safeJoinPaths base rel of
    Nothing => Nothing
    Just full => Just (MkContainedPath base rel full)

||| Validate path string
public export
validatePath : String -> Either String ValidatedPath
validatePath "" = Left "Empty path"
validatePath s =
  let isAbs = isPrefixOf "/" s
      segs = splitPath s
  in if any hasNullByte segs
       then Left "Null byte in path"
       else if any (\seg => length seg > 255) segs
         then Left "Segment too long"
         else if length s > 4096
           then Left "Path too long"
           else Right (MkValidatedPath (normalizePath s) isAbs segs)
  where
    hasNullByte : String -> Bool
    hasNullByte seg = '\0' `elem` unpack seg

--------------------------------------------------------------------------------
-- Glob Matching
--------------------------------------------------------------------------------

||| Check if string matches a simple glob pattern
public export
matchGlob : String -> String -> Bool
matchGlob pattern str = matchGlobChars (unpack pattern) (unpack str)
  where
    matchGlobChars : List Char -> List Char -> Bool
    matchGlobChars [] [] = True
    matchGlobChars [] _ = False
    matchGlobChars ('*' :: ps) ss = matchStar ps ss
    matchGlobChars ('?' :: ps) (_ :: ss) = matchGlobChars ps ss
    matchGlobChars ('?' :: _) [] = False
    matchGlobChars (p :: ps) (s :: ss) =
      if p == s then matchGlobChars ps ss else False
    matchGlobChars _ [] = False

    matchStar : List Char -> List Char -> Bool
    matchStar [] _ = True
    matchStar ps [] = matchGlobChars ps []
    matchStar ps (s :: ss) =
      matchGlobChars ps (s :: ss) || matchStar ps ss

||| Match path against glob pattern (handles **)
public export
matchPathGlob : String -> String -> Bool
matchPathGlob pattern path =
  let patSegs = splitPath pattern
      pathSegs = splitPath path
  in matchSegments patSegs pathSegs
  where
    matchSegments : List String -> List String -> Bool
    matchSegments [] [] = True
    matchSegments [] _ = False
    matchSegments ("**" :: []) _ = True
    matchSegments ("**" :: ps) ss = any (matchSegments ps) (tails ss)
    matchSegments (p :: ps) (s :: ss) =
      matchGlob p s && matchSegments ps ss
    matchSegments _ [] = False

    tails : List a -> List (List a)
    tails [] = [[]]
    tails xs@(_ :: rest) = xs :: tails rest

--------------------------------------------------------------------------------
-- Common File Extensions
--------------------------------------------------------------------------------

||| Check if file has executable extension
public export
isExecutableExtension : String -> Bool
isExecutableExtension ext =
  toLower ext `elem` ["exe", "bat", "cmd", "com", "msi", "sh", "bash", "zsh"]

||| Check if file has archive extension
public export
isArchiveExtension : String -> Bool
isArchiveExtension ext =
  toLower ext `elem` ["zip", "tar", "gz", "bz2", "xz", "7z", "rar", "tgz"]

||| Check if file has image extension
public export
isImageExtension : String -> Bool
isImageExtension ext =
  toLower ext `elem` ["jpg", "jpeg", "png", "gif", "bmp", "svg", "webp", "ico"]

||| Check if file has document extension
public export
isDocumentExtension : String -> Bool
isDocumentExtension ext =
  toLower ext `elem` ["pdf", "doc", "docx", "txt", "rtf", "odt", "md", "adoc"]

||| Get MIME type from extension (simplified)
public export
getMimeType : String -> Maybe String
getMimeType ext = lookup (toLower ext) mimeTypes
  where
    mimeTypes : List (String, String)
    mimeTypes =
      [ ("html", "text/html"), ("htm", "text/html")
      , ("css", "text/css"), ("js", "application/javascript")
      , ("json", "application/json"), ("xml", "application/xml")
      , ("txt", "text/plain"), ("md", "text/markdown")
      , ("png", "image/png"), ("jpg", "image/jpeg"), ("jpeg", "image/jpeg")
      , ("gif", "image/gif"), ("svg", "image/svg+xml")
      , ("pdf", "application/pdf")
      , ("zip", "application/zip")
      ]
