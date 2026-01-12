-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Safe file types and constraints
|||
||| This module defines types for safe file operations including:
||| - Bounded file paths
||| - Safe file handles
||| - Read/write limits
||| - Path validation
module Proven.SafeFile.Types

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Path Types
--------------------------------------------------------------------------------

||| Maximum path length (4KB - standard limit)
public export
maxPathLength : Nat
maxPathLength = 4096

||| Maximum filename length (255 - POSIX limit)
public export
maxFilenameLength : Nat
maxFilenameLength = 255

||| Validated file path
public export
record SafePath where
  constructor MkSafePath
  ||| The path string
  path : String
  ||| Proof path is bounded
  0 bounded : length (unpack path) <= maxPathLength = True

||| Try to create a safe path
public export
safePath : (p : String) -> Maybe SafePath
safePath p =
  if length (unpack p) <= maxPathLength
    then Just (MkSafePath p (believe_me Refl))
    else Nothing

||| Get raw path string
public export
(.rawPath) : SafePath -> String
(.rawPath) sp = sp.path

||| Path component (single directory or filename)
public export
record PathComponent where
  constructor MkPathComponent
  ||| The component string
  component : String
  ||| Proof component is bounded
  0 bounded : length (unpack component) <= maxFilenameLength = True

||| Try to create path component
public export
pathComponent : (c : String) -> Maybe PathComponent
pathComponent c =
  if length (unpack c) <= maxFilenameLength
    then Just (MkPathComponent c (believe_me Refl))
    else Nothing

--------------------------------------------------------------------------------
-- File Handle Types
--------------------------------------------------------------------------------

||| File open mode
public export
data FileMode : Type where
  ||| Read only
  ReadOnly : FileMode
  ||| Write only (creates/truncates)
  WriteOnly : FileMode
  ||| Append only (creates if needed)
  AppendOnly : FileMode
  ||| Read and write
  ReadWrite : FileMode

public export
Show FileMode where
  show ReadOnly = "r"
  show WriteOnly = "w"
  show AppendOnly = "a"
  show ReadWrite = "rw"

public export
Eq FileMode where
  ReadOnly == ReadOnly = True
  WriteOnly == WriteOnly = True
  AppendOnly == AppendOnly = True
  ReadWrite == ReadWrite = True
  _ == _ = False

||| Safe file handle with mode tracking
public export
record SafeHandle where
  constructor MkSafeHandle
  ||| Underlying handle identifier
  handleId : Nat
  ||| Open mode
  mode : FileMode
  ||| Associated path
  path : SafePath
  ||| Bytes read so far
  bytesRead : Nat
  ||| Bytes written so far
  bytesWritten : Nat

||| Check if handle is readable
public export
isReadable : SafeHandle -> Bool
isReadable h = h.mode == ReadOnly || h.mode == ReadWrite

||| Check if handle is writable
public export
isWritable : SafeHandle -> Bool
isWritable h = h.mode == WriteOnly || h.mode == AppendOnly || h.mode == ReadWrite

--------------------------------------------------------------------------------
-- Size Types
--------------------------------------------------------------------------------

||| Bounded size value
public export
record BoundedSize where
  constructor MkBoundedSize
  ||| The size value
  size : Nat
  ||| Maximum allowed
  maxSize : Nat
  ||| Proof size is bounded
  0 bounded : size <= maxSize = True

||| Create bounded size
public export
boundedSize : (max : Nat) -> (n : Nat) -> Maybe BoundedSize
boundedSize max n =
  if n <= max
    then Just (MkBoundedSize n max (believe_me Refl))
    else Nothing

||| Default read limit (64MB)
public export
defaultReadLimit : Nat
defaultReadLimit = 67108864

||| Default write limit (64MB)
public export
defaultWriteLimit : Nat
defaultWriteLimit = 67108864

||| Strict read limit (1MB)
public export
strictReadLimit : Nat
strictReadLimit = 1048576

||| Strict write limit (1MB)
public export
strictWriteLimit : Nat
strictWriteLimit = 1048576

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

||| File operation errors
public export
data FileError : Type where
  ||| Path too long
  PathTooLong : (path : String) -> (len : Nat) -> (max : Nat) -> FileError

  ||| Invalid path characters
  InvalidPath : (path : String) -> (reason : String) -> FileError

  ||| Path traversal attempt
  PathTraversal : (path : String) -> FileError

  ||| File not found
  NotFound : (path : String) -> FileError

  ||| Permission denied
  PermissionDenied : (path : String) -> (operation : String) -> FileError

  ||| Read limit exceeded
  ReadLimitExceeded : (requested : Nat) -> (limit : Nat) -> FileError

  ||| Write limit exceeded
  WriteLimitExceeded : (requested : Nat) -> (limit : Nat) -> FileError

  ||| File too large
  FileTooLarge : (path : String) -> (size : Nat) -> (limit : Nat) -> FileError

  ||| Handle closed
  HandleClosed : FileError

  ||| Invalid operation for mode
  InvalidOperation : (operation : String) -> (mode : FileMode) -> FileError

  ||| IO error
  IOError : (message : String) -> FileError

public export
Show FileError where
  show (PathTooLong path len max) =
    "Path too long: " ++ show len ++ " chars (max " ++ show max ++ ")"
  show (InvalidPath path reason) =
    "Invalid path '" ++ path ++ "': " ++ reason
  show (PathTraversal path) =
    "Path traversal detected: " ++ path
  show (NotFound path) =
    "File not found: " ++ path
  show (PermissionDenied path op) =
    "Permission denied for " ++ op ++ " on " ++ path
  show (ReadLimitExceeded requested limit) =
    "Read limit exceeded: " ++ show requested ++ " bytes (limit " ++ show limit ++ ")"
  show (WriteLimitExceeded requested limit) =
    "Write limit exceeded: " ++ show requested ++ " bytes (limit " ++ show limit ++ ")"
  show (FileTooLarge path size limit) =
    "File too large: " ++ path ++ " is " ++ show size ++ " bytes (limit " ++ show limit ++ ")"
  show HandleClosed =
    "Handle is closed"
  show (InvalidOperation op mode) =
    "Invalid operation " ++ op ++ " for mode " ++ show mode
  show (IOError msg) =
    "IO error: " ++ msg

public export
Eq FileError where
  PathTooLong p1 l1 m1 == PathTooLong p2 l2 m2 = p1 == p2 && l1 == l2 && m1 == m2
  InvalidPath p1 r1 == InvalidPath p2 r2 = p1 == p2 && r1 == r2
  PathTraversal p1 == PathTraversal p2 = p1 == p2
  NotFound p1 == NotFound p2 = p1 == p2
  PermissionDenied p1 o1 == PermissionDenied p2 o2 = p1 == p2 && o1 == o2
  ReadLimitExceeded r1 l1 == ReadLimitExceeded r2 l2 = r1 == r2 && l1 == l2
  WriteLimitExceeded r1 l1 == WriteLimitExceeded r2 l2 = r1 == r2 && l1 == l2
  FileTooLarge p1 s1 l1 == FileTooLarge p2 s2 l2 = p1 == p2 && s1 == s2 && l1 == l2
  HandleClosed == HandleClosed = True
  InvalidOperation o1 m1 == InvalidOperation o2 m2 = o1 == o2 && m1 == m2
  IOError m1 == IOError m2 = m1 == m2
  _ == _ = False

||| File operation result
public export
FileResult : Type -> Type
FileResult = Result FileError

--------------------------------------------------------------------------------
-- Options Types
--------------------------------------------------------------------------------

||| File operation options
public export
record FileOptions where
  constructor MkFileOptions
  ||| Maximum bytes to read per operation
  maxReadSize : Nat
  ||| Maximum bytes to write per operation
  maxWriteSize : Nat
  ||| Maximum total bytes read per handle
  maxTotalRead : Nat
  ||| Maximum total bytes written per handle
  maxTotalWrite : Nat
  ||| Allow path traversal (../)
  allowTraversal : Bool
  ||| Allowed base directories (empty = any)
  allowedDirs : List String
  ||| Blocked paths/patterns
  blockedPaths : List String
  ||| Follow symlinks
  followSymlinks : Bool

||| Default file options
public export
defaultOptions : FileOptions
defaultOptions = MkFileOptions
  { maxReadSize = defaultReadLimit
  , maxWriteSize = defaultWriteLimit
  , maxTotalRead = defaultReadLimit * 10
  , maxTotalWrite = defaultWriteLimit * 10
  , allowTraversal = False
  , allowedDirs = []
  , blockedPaths = ["/etc/passwd", "/etc/shadow", "~/.ssh"]
  , followSymlinks = True
  }

||| Strict file options
public export
strictOptions : FileOptions
strictOptions = MkFileOptions
  { maxReadSize = strictReadLimit
  , maxWriteSize = strictWriteLimit
  , maxTotalRead = strictReadLimit * 5
  , maxTotalWrite = strictWriteLimit * 5
  , allowTraversal = False
  , allowedDirs = []
  , blockedPaths = ["/etc/passwd", "/etc/shadow", "~/.ssh", "/dev", "/proc", "/sys"]
  , followSymlinks = False
  }

||| Permissive file options
public export
permissiveOptions : FileOptions
permissiveOptions = MkFileOptions
  { maxReadSize = defaultReadLimit * 10
  , maxWriteSize = defaultWriteLimit * 10
  , maxTotalRead = defaultReadLimit * 100
  , maxTotalWrite = defaultWriteLimit * 100
  , allowTraversal = True
  , allowedDirs = []
  , blockedPaths = []
  , followSymlinks = True
  }

--------------------------------------------------------------------------------
-- File Info Types
--------------------------------------------------------------------------------

||| File type enumeration
public export
data FileType : Type where
  ||| Regular file
  RegularFile : FileType
  ||| Directory
  Directory : FileType
  ||| Symbolic link
  SymLink : FileType
  ||| Other (device, socket, etc.)
  OtherType : FileType

public export
Show FileType where
  show RegularFile = "file"
  show Directory = "directory"
  show SymLink = "symlink"
  show OtherType = "other"

public export
Eq FileType where
  RegularFile == RegularFile = True
  Directory == Directory = True
  SymLink == SymLink = True
  OtherType == OtherType = True
  _ == _ = False

||| File information
public export
record FileInfo where
  constructor MkFileInfo
  ||| File path
  path : SafePath
  ||| File type
  fileType : FileType
  ||| File size in bytes
  size : Nat
  ||| Is readable
  readable : Bool
  ||| Is writable
  writable : Bool
  ||| Is executable
  executable : Bool

--------------------------------------------------------------------------------
-- Path Security
--------------------------------------------------------------------------------

||| Dangerous path patterns
public export
dangerousPatterns : List String
dangerousPatterns =
  [ ".."           -- Parent traversal
  , "~"            -- Home expansion (shell)
  , "$"            -- Variable expansion
  , "|"            -- Pipe
  , ";"            -- Command separator
  , "&"            -- Background/AND
  , "`"            -- Command substitution
  , "\\"           -- Escape character
  , "\n"           -- Newline injection
  , "\r"           -- Carriage return injection
  , "\0"           -- Null byte injection
  ]

||| Check if path contains dangerous patterns
public export
hasDangerousPattern : String -> Bool
hasDangerousPattern path =
  any (\pat => isInfixOf pat path) dangerousPatterns

||| Check if path is within allowed directories
public export
isInAllowedDir : List String -> String -> Bool
isInAllowedDir [] _ = True  -- Empty list = allow all
isInAllowedDir dirs path = any (\d => isPrefixOf d path) dirs

||| Check if path is blocked
public export
isBlockedPath : List String -> String -> Bool
isBlockedPath blocked path = any (\b => isInfixOf b path) blocked

--------------------------------------------------------------------------------
-- Buffer Types
--------------------------------------------------------------------------------

||| Bounded buffer for reading
public export
record ReadBuffer where
  constructor MkReadBuffer
  ||| Buffer contents
  contents : String
  ||| Buffer size
  size : Nat
  ||| Maximum allowed
  maxSize : Nat
  ||| Proof size bounded
  0 bounded : size <= maxSize = True

||| Create read buffer
public export
mkReadBuffer : (max : Nat) -> (content : String) -> Maybe ReadBuffer
mkReadBuffer max content =
  let sz = length (unpack content)
  in if sz <= max
       then Just (MkReadBuffer content sz max (believe_me Refl))
       else Nothing

||| Empty read buffer
public export
emptyBuffer : (max : Nat) -> ReadBuffer
emptyBuffer max = MkReadBuffer "" 0 max Refl

