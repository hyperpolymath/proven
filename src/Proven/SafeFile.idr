-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeFile - Safe file operations
|||
||| This module provides safe file operations including:
||| - Bounded reads with size limits
||| - Safe path validation (no traversal)
||| - Mode-enforced handles
||| - Resource tracking
|||
||| Example usage:
||| ```idris
||| -- Validate a path
||| case validatePath "/home/user/data.txt" of
|||   Ok safePath => readFile safePath
|||   Err (PathTraversal _) => putStrLn "Nice try"
|||   Err e => putStrLn (friendlyError e)
|||
||| -- Read with size limit
||| case readBounded fs opts "/data/input.txt" 1024 of
|||   Ok content => processContent content
|||   Err (FileTooLarge _ _ _) => putStrLn "File exceeds limit"
|||   Err e => handleError e
||| ```
module Proven.SafeFile

import public Proven.Core
import public Proven.SafeFile.Types
import public Proven.SafeFile.Operations
import public Proven.SafeFile.Proofs

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- High-Level Path API
--------------------------------------------------------------------------------

||| Validate path with default options
public export
validate : String -> FileResult SafePath
validate = validatePathDefault

||| Validate path with custom options
public export
validateWith : FileOptions -> String -> FileResult SafePath
validateWith = validatePath

||| Check if path is safe (no result details)
public export
isSafe : String -> Bool
isSafe path = isOk (validatePathDefault path)
  where
    isOk : FileResult a -> Bool
    isOk (Ok _) = True
    isOk (Err _) = False

||| Normalize and validate path
public export
normalizeAndValidate : String -> FileResult SafePath
normalizeAndValidate path = validatePathDefault (normalizePath path)

--------------------------------------------------------------------------------
-- Path Component API
--------------------------------------------------------------------------------

||| Get filename from path
public export
getFilename : String -> String
getFilename = filename

||| Get directory from path
public export
getDirectory : String -> String
getDirectory = dirname

||| Get file extension
public export
getExtension : String -> Maybe String
getExtension = extension

||| Join path components safely
public export
join : List String -> FileResult SafePath
join parts = validatePathDefault (joinPath parts)

||| Combine base and relative path safely
public export
combine : String -> String -> FileResult SafePath
combine base rel = validatePathDefault (combinePath base rel)

||| Safe join with traversal check
public export
safelyJoin : String -> String -> FileResult String
safelyJoin = safeJoin defaultOptions

--------------------------------------------------------------------------------
-- File Reading API (Pure)
--------------------------------------------------------------------------------

||| Read entire file (bounded)
public export
readFile : FileSystem -> String -> FileResult String
readFile fs = readFilePure fs defaultOptions

||| Read file with custom options
public export
readFileWith : FileSystem -> FileOptions -> String -> FileResult String
readFileWith = readFilePure

||| Read file with explicit size limit
public export
readBounded : FileSystem -> String -> Nat -> FileResult String
readBounded fs = readBoundedPure fs defaultOptions

||| Read file strictly (1MB limit)
public export
readStrict : FileSystem -> String -> FileResult String
readStrict fs = readFilePure fs strictOptions

||| Read first N bytes
public export
readHead : FileSystem -> String -> Nat -> FileResult String
readHead fs path n = readBoundedPure fs defaultOptions path n

--------------------------------------------------------------------------------
-- Line-Based API
--------------------------------------------------------------------------------

||| Read lines from file
public export
readLines : FileSystem -> String -> FileResult (List String)
readLines fs path = readLinesPure fs defaultOptions path 1000000

||| Read first N lines
public export
readFirstLines : FileSystem -> String -> Nat -> FileResult (List String)
readFirstLines fs = readLinesPure fs defaultOptions

||| Count lines in file
public export
countLines : FileSystem -> String -> FileResult Nat
countLines fs = countLinesPure fs defaultOptions

--------------------------------------------------------------------------------
-- File Info API
--------------------------------------------------------------------------------

||| Get file information
public export
getInfo : FileSystem -> String -> FileResult FileInfo
getInfo fs = getFileInfoPure fs defaultOptions

||| Check if file exists
public export
exists : FileSystem -> String -> Bool
exists = fileExists

||| Check if path is file
public export
isFile : FileSystem -> String -> Bool
isFile fs path = case findEntry fs path of
  Just entry => entry.entryType == RegularFile
  Nothing => False

||| Check if path is directory
public export
isDir : FileSystem -> String -> Bool
isDir fs path = case findEntry fs path of
  Just entry => entry.entryType == Directory
  Nothing => False

--------------------------------------------------------------------------------
-- Directory API
--------------------------------------------------------------------------------

||| List directory contents
public export
listDir : FileSystem -> String -> FileResult (List String)
listDir fs = listDirPure fs defaultOptions

||| List directory with options
public export
listDirWith : FileSystem -> FileOptions -> String -> FileResult (List String)
listDirWith = listDirPure

--------------------------------------------------------------------------------
-- Content Validation
--------------------------------------------------------------------------------

||| Check if content is text (no null bytes)
public export
isText : String -> Bool
isText = isTextContent

||| Check if file likely isInfixOf text
public export
isTextFile : String -> Bool
isTextFile path = case extension path of
  Just ext => isTextExtension ext
  Nothing => False

||| Check if file likely isInfixOf binary
public export
isBinaryFile : String -> Bool
isBinaryFile path = case extension path of
  Just ext => isBinaryExtension ext
  Nothing => False

||| Sanitize file content (remove null bytes)
public export
sanitize : String -> String
sanitize = sanitizeContent

--------------------------------------------------------------------------------
-- Handle API
--------------------------------------------------------------------------------

||| Create read-only handle
public export
openForRead : Nat -> SafePath -> SafeHandle
openForRead id path = newHandle id ReadOnly path

||| Create write-only handle
public export
openForWrite : Nat -> SafePath -> SafeHandle
openForWrite id path = newHandle id WriteOnly path

||| Create append handle
public export
openForAppend : Nat -> SafePath -> SafeHandle
openForAppend id path = newHandle id AppendOnly path

||| Create read-write handle
public export
openForReadWrite : Nat -> SafePath -> SafeHandle
openForReadWrite id path = newHandle id ReadWrite path

||| Check handle can read
public export
canRead : SafeHandle -> Bool
canRead = isReadable

||| Check handle can write
public export
canWrite : SafeHandle -> Bool
canWrite = isWritable

||| Get bytes read from handle
public export
bytesRead : SafeHandle -> Nat
bytesRead h = h.bytesRead

||| Get bytes written to handle
public export
bytesWritten : SafeHandle -> Nat
bytesWritten h = h.bytesWritten

--------------------------------------------------------------------------------
-- Limit Checking
--------------------------------------------------------------------------------

||| Check if read is allowed
public export
canReadSize : Nat -> Bool
canReadSize n = n <= defaultOptions.maxReadSize

||| Check if write is allowed
public export
canWriteSize : Nat -> Bool
canWriteSize n = n <= defaultOptions.maxWriteSize

||| Get remaining read allowance for handle
public export
remainingReadAllowance : SafeHandle -> Nat
remainingReadAllowance h =
  if h.bytesRead >= defaultOptions.maxTotalRead
    then 0
    else defaultOptions.maxTotalRead `minus` h.bytesRead

||| Get remaining write allowance for handle
public export
remainingWriteAllowance : SafeHandle -> Nat
remainingWriteAllowance h =
  if h.bytesWritten >= defaultOptions.maxTotalWrite
    then 0
    else defaultOptions.maxTotalWrite `minus` h.bytesWritten

--------------------------------------------------------------------------------
-- Buffer API
--------------------------------------------------------------------------------

||| Create buffer with default max
public export
newBuffer : String -> Maybe ReadBuffer
newBuffer = mkReadBuffer defaultReadLimit

||| Create buffer with custom max
public export
newBufferWith : Nat -> String -> Maybe ReadBuffer
newBufferWith = mkReadBuffer

||| Create empty buffer
public export
emptyReadBuffer : ReadBuffer
emptyReadBuffer = emptyBuffer defaultReadLimit

||| Get buffer contents
public export
bufferContents : ReadBuffer -> String
bufferContents buf = buf.contents

||| Get buffer size
public export
bufferSize : ReadBuffer -> Nat
bufferSize buf = buf.size

--------------------------------------------------------------------------------
-- Preset Options
--------------------------------------------------------------------------------

||| Default options (64MB limits)
public export
defaults : FileOptions
defaults = defaultOptions

||| Strict options (1MB limits, no symlinks)
public export
strict : FileOptions
strict = strictOptions

||| Permissive options (640MB limits)
public export
permissive : FileOptions
permissive = permissiveOptions

--------------------------------------------------------------------------------
-- Error Helpers
--------------------------------------------------------------------------------

||| Check if error is path related
public export
isPathError : FileError -> Bool
isPathError (PathTooLong _ _ _) = True
isPathError (InvalidPath _ _) = True
isPathError (PathTraversal _) = True
isPathError _ = False

||| Check if error is permission related
public export
isPermissionError : FileError -> Bool
isPermissionError (PermissionDenied _ _) = True
isPermissionError _ = False

||| Check if error is limit related
public export
isLimitError : FileError -> Bool
isLimitError (ReadLimitExceeded _ _) = True
isLimitError (WriteLimitExceeded _ _) = True
isLimitError (FileTooLarge _ _ _) = True
isLimitError _ = False

||| Check if error is not found
public export
isNotFoundError : FileError -> Bool
isNotFoundError (NotFound _) = True
isNotFoundError _ = False

||| Get user-friendly error message
public export
friendlyError : FileError -> String
friendlyError (PathTooLong path len max) =
  "Path too long: " ++ show len ++ " characters (maximum " ++ show max ++ ")"
friendlyError (InvalidPath path reason) =
  "Invalid path '" ++ path ++ "': " ++ reason
friendlyError (PathTraversal path) =
  "Path traversal blocked: '" ++ path ++ "' isInfixOf '..' or dangerous characters"
friendlyError (NotFound path) =
  "File not found: " ++ path
friendlyError (PermissionDenied path op) =
  "Permission denied: cannot " ++ op ++ " '" ++ path ++ "'"
friendlyError (ReadLimitExceeded req limit) =
  "Read limit exceeded: requested " ++ show req ++ " bytes (limit " ++ show limit ++ ")"
friendlyError (WriteLimitExceeded req limit) =
  "Write limit exceeded: requested " ++ show req ++ " bytes (limit " ++ show limit ++ ")"
friendlyError (FileTooLarge path size limit) =
  "File too large: " ++ path ++ " is " ++ show size ++ " bytes (limit " ++ show limit ++ ")"
friendlyError HandleClosed =
  "Operation failed: file handle is closed"
friendlyError (InvalidOperation op mode) =
  "Invalid operation: cannot " ++ op ++ " in " ++ show mode ++ " mode"
friendlyError (IOError msg) =
  "I/O error: " ++ msg

--------------------------------------------------------------------------------
-- Common Patterns
--------------------------------------------------------------------------------

||| Read config file (strict limits)
public export
readConfig : FileSystem -> String -> FileResult String
readConfig fs path = readFilePure fs strictOptions path

||| Read log file (permissive)
public export
readLog : FileSystem -> String -> FileResult String
readLog fs path = readFilePure fs permissiveOptions path

||| Read small file (64KB limit)
public export
readSmall : FileSystem -> String -> FileResult String
readSmall fs path = readBoundedPure fs defaultOptions path 65536

||| Check path is safe for logging
public export
safeForLog : String -> String
safeForLog path =
  if length (unpack path) > 100
    then take 100 path ++ "..."
    else path

--------------------------------------------------------------------------------
-- Temporary File Helpers
--------------------------------------------------------------------------------

||| Generate temp filename
public export
tempFile : Nat -> String -> String
tempFile = tempFilename

||| Check if temp file
public export
isTemp : String -> Bool
isTemp = isTempFile

