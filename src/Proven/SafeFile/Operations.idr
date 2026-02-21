-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Safe file operations
|||
||| This module provides safe file operations including:
||| - Path validation
||| - Bounded reads
||| - Size-limited writes
||| - Directory operations
module Proven.SafeFile.Operations

import Proven.Core
import Proven.SafeFile.Types
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Path Validation
--------------------------------------------------------------------------------

||| Validate path string
export
validatePath : FileOptions -> String -> FileResult SafePath
validatePath opts path =
  let pathLen = length (unpack path)
  in if pathLen > maxPathLength
       then Err (PathTooLong path pathLen maxPathLength)
       else if not opts.allowTraversal && hasDangerousPattern path
         then Err (PathTraversal path)
         else if not (isInAllowedDir opts.allowedDirs path)
           then Err (PermissionDenied path "not in allowed directories")
           else if isBlockedPath opts.blockedPaths path
             then Err (PermissionDenied path "path is blocked")
             else case safePath path of
               Just sp => Ok sp
               Nothing => Err (InvalidPath path "failed validation")

||| Validate path with default options
export
validatePathDefault : String -> FileResult SafePath
validatePathDefault = validatePath defaultOptions

||| Normalize path (remove redundant slashes, etc.)
export
normalizePath : String -> String
normalizePath path =
  let parts = filter (not . null) (split (== '/') path)
      normalized = "/" ++ joinBy "/" parts
  in if isPrefixOf "/" path then normalized else joinBy "/" parts

||| Split path into directory and filename
export
splitPath : String -> (String, String)
splitPath path =
  case break (== '/') (reverse path) of
    (revName, revDir) =>
      (reverse (drop 1 revDir), reverse revName)

||| Get filename from path
export
filename : String -> String
filename path = snd (splitPath path)

||| Get directory from path
export
dirname : String -> String
dirname path = fst (splitPath path)

||| Get file extension
export
extension : String -> Maybe String
extension path =
  let name = filename path
  in case break (== '.') (reverse name) of
       (revExt, rest) =>
         if null rest then Nothing
         else Just (reverse revExt)

||| Join path components
export
joinPath : List String -> String
joinPath = joinBy "/"

||| Combine two paths
export
combinePath : String -> String -> String
combinePath base rel =
  if isPrefixOf "/" rel
    then rel  -- Absolute path
    else if null base
      then rel
      else if isSuffixOf "/" base
        then base ++ rel
        else base ++ "/" ++ rel

--------------------------------------------------------------------------------
-- Read Limit Checking
--------------------------------------------------------------------------------

||| Check if read size is within limits
export
checkReadSize : FileOptions -> Nat -> FileResult ()
checkReadSize opts requested =
  if requested > opts.maxReadSize
    then Err (ReadLimitExceeded requested opts.maxReadSize)
    else Ok ()

||| Check if total read is within limits
export
checkTotalRead : FileOptions -> SafeHandle -> Nat -> FileResult ()
checkTotalRead opts handle additional =
  let total = handle.bytesRead + additional
  in if total > opts.maxTotalRead
       then Err (ReadLimitExceeded total opts.maxTotalRead)
       else Ok ()

||| Check if write size is within limits
export
checkWriteSize : FileOptions -> Nat -> FileResult ()
checkWriteSize opts requested =
  if requested > opts.maxWriteSize
    then Err (WriteLimitExceeded requested opts.maxWriteSize)
    else Ok ()

||| Check if total write is within limits
export
checkTotalWrite : FileOptions -> SafeHandle -> Nat -> FileResult ()
checkTotalWrite opts handle additional =
  let total = handle.bytesWritten + additional
  in if total > opts.maxTotalWrite
       then Err (WriteLimitExceeded total opts.maxTotalWrite)
       else Ok ()

--------------------------------------------------------------------------------
-- Handle Operations
--------------------------------------------------------------------------------

||| Check if handle can be read
export
checkReadable : SafeHandle -> FileResult ()
checkReadable h =
  if isReadable h
    then Ok ()
    else Err (InvalidOperation "read" h.mode)

||| Check if handle can be written
export
checkWritable : SafeHandle -> FileResult ()
checkWritable h =
  if isWritable h
    then Ok ()
    else Err (InvalidOperation "write" h.mode)

||| Update handle after read
export
updateAfterRead : SafeHandle -> Nat -> SafeHandle
updateAfterRead h bytes = { bytesRead := h.bytesRead + bytes } h

||| Update handle after write
export
updateAfterWrite : SafeHandle -> Nat -> SafeHandle
updateAfterWrite h bytes = { bytesWritten := h.bytesWritten + bytes } h

||| Create new handle (for simulation/testing)
export
newHandle : Nat -> FileMode -> SafePath -> SafeHandle
newHandle id mode path = MkSafeHandle id mode path 0 0

--------------------------------------------------------------------------------
-- Simulated File Operations (Pure)
--------------------------------------------------------------------------------

||| Simulated file system entry
public export
record FSEntry where
  constructor MkFSEntry
  entryPath : String
  entryType : FileType
  entrySize : Nat
  contents : String

||| Simulated file system
public export
FileSystem : Type
FileSystem = List FSEntry

||| Find entry in file system
export
findEntry : FileSystem -> String -> Maybe FSEntry
findEntry fs path = find (\e => e.entryPath == path) fs

||| Check if file exists
export
fileExists : FileSystem -> String -> Bool
fileExists fs path = isJust (findEntry fs path)

||| Get file info (pure)
export
getFileInfoPure : FileSystem -> FileOptions -> String -> FileResult FileInfo
getFileInfoPure fs opts path = do
  sp <- validatePath opts path
  case findEntry fs path of
    Nothing => Err (NotFound path)
    Just entry => Ok (MkFileInfo
      { path = sp
      , fileType = entry.entryType
      , size = entry.entrySize
      , readable = True
      , writable = True
      , executable = False
      })

||| Read file contents (pure, bounded)
export
readFilePure : FileSystem -> FileOptions -> String -> FileResult String
readFilePure fs opts path = do
  sp <- validatePath opts path
  case findEntry fs path of
    Nothing => Err (NotFound path)
    Just entry =>
      if entry.entrySize > opts.maxReadSize
        then Err (FileTooLarge path entry.entrySize opts.maxReadSize)
        else Ok entry.contents

||| Read file with size limit (pure)
export
readBoundedPure : FileSystem -> FileOptions -> String -> Nat -> FileResult String
readBoundedPure fs opts path limit = do
  sp <- validatePath opts path
  checkReadSize opts limit
  case findEntry fs path of
    Nothing => Err (NotFound path)
    Just entry =>
      let content = entry.contents
          actualSize = min limit (length (unpack content))
      in Ok (pack (take actualSize (unpack content)))

||| List directory contents (pure)
export
listDirPure : FileSystem -> FileOptions -> String -> FileResult (List String)
listDirPure fs opts path = do
  sp <- validatePath opts path
  case findEntry fs path of
    Nothing => Err (NotFound path)
    Just entry =>
      if entry.entryType /= Directory
        then Err (InvalidPath path "not a directory")
        else let prefix = if isSuffixOf "/" path then path else path ++ "/"
                 entries = filter (\e => isPrefixOf prefix e.entryPath &&
                                         not (isInfixOf "/" (drop (length (unpack prefix)) e.entryPath)))
                                  fs
             in Ok (map (\e => filename e.entryPath) entries)

--------------------------------------------------------------------------------
-- Line-Based Operations
--------------------------------------------------------------------------------

||| Read lines with limit
export
readLinesPure : FileSystem -> FileOptions -> String -> Nat -> FileResult (List String)
readLinesPure fs opts path maxLines = do
  content <- readFilePure fs opts path
  let allLines = lines content
      limitedLines = take maxLines allLines
  Ok limitedLines

||| Count lines (bounded read)
export
countLinesPure : FileSystem -> FileOptions -> String -> FileResult Nat
countLinesPure fs opts path = do
  content <- readFilePure fs opts path
  Ok (length (lines content))

--------------------------------------------------------------------------------
-- Content Validation
--------------------------------------------------------------------------------

||| Check if content is text (no null bytes)
export
isTextContent : String -> Bool
isTextContent s = not (isInfixOf "\0" s)

||| Check if content is valid UTF-8 (simplified)
export
isValidUtf8 : String -> Bool
isValidUtf8 s = all (\c => ord c >= 0 && ord c <= 1114111) (unpack s)

||| Sanitize content (remove null bytes)
export
sanitizeContent : String -> String
sanitizeContent = pack . filter (/= '\0') . unpack

--------------------------------------------------------------------------------
-- Path Security Helpers
--------------------------------------------------------------------------------

||| Check if path is absolute
export
isAbsolute : String -> Bool
isAbsolute s = isPrefixOf "/" s

||| Check if path is relative
export
isRelative : String -> Bool
isRelative = not . isAbsolute

||| Resolve relative path against base
export
resolvePath : String -> String -> String
resolvePath base rel =
  if isAbsolute rel
    then rel
    else normalizePath (combinePath base rel)

||| Check if path escapes base directory
export
escapesBase : String -> String -> Bool
escapesBase base path =
  let resolved = resolvePath base path
  in not (isPrefixOf base resolved)

||| Safe path join (prevents traversal)
export
safeJoin : FileOptions -> String -> String -> FileResult String
safeJoin opts base rel = do
  let combined = combinePath base rel
  validated <- validatePath opts combined
  if escapesBase base combined && not opts.allowTraversal
    then Err (PathTraversal combined)
    else Ok combined

--------------------------------------------------------------------------------
-- Temporary File Helpers
--------------------------------------------------------------------------------

||| Generate temp filename (pure - needs seed)
export
tempFilename : Nat -> String -> String
tempFilename seed prefix = prefix ++ "-" ++ show seed ++ ".tmp"

||| Check if path looks like temp file
export
isTempFile : String -> Bool
isTempFile path = isSuffixOf ".tmp" path || isInfixOf "/tmp/" path

--------------------------------------------------------------------------------
-- File Type Detection
--------------------------------------------------------------------------------

||| Common text file extensions
public export
textExtensions : List String
textExtensions =
  [ "txt", "md", "rst", "adoc"
  , "json", "yaml", "yml", "toml", "xml"
  , "html", "css", "js", "ts"
  , "py", "rb", "rs", "go", "java"
  , "c", "h", "cpp", "hpp"
  , "sh", "bash", "zsh"
  , "idr", "hs", "ml", "fs"
  ]

||| Check if extension indicates text file
export
isTextExtension : String -> Bool
isTextExtension ext = toLower ext `elem` textExtensions

||| Common binary file extensions
public export
binaryExtensions : List String
binaryExtensions =
  [ "exe", "dll", "so", "dylib"
  , "zip", "tar", "gz", "bz2", "xz"
  , "png", "jpg", "jpeg", "gif", "bmp", "webp"
  , "mp3", "mp4", "wav", "avi", "mkv"
  , "pdf", "doc", "docx", "xls", "xlsx"
  , "bin", "dat", "db", "sqlite"
  ]

||| Check if extension indicates binary file
export
isBinaryExtension : String -> Bool
isBinaryExtension ext = toLower ext `elem` binaryExtensions

