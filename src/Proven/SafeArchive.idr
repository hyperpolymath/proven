-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2024-2025 Hyperpolymath

||| Safe archive handling with path traversal and decompression bomb prevention
|||
||| This module provides type-safe archive handling:
||| - Archive format detection (tar, zip, gzip, etc.)
||| - Entry validation and filtering
||| - Size limits and ratio checks
||| - Permission validation
|||
||| Security features:
||| - Path traversal prevention (Zip Slip)
||| - Decompression bomb detection
||| - Symlink attack prevention
||| - Permission stripping
||| - Size limit enforcement
||| - File type filtering
module Proven.SafeArchive

import Data.List
import Data.String
import Data.Either
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

||| Archive format types
public export
data ArchiveFormat
  = TarFormat
  | TarGzFormat
  | TarBz2Format
  | TarXzFormat
  | TarZstFormat
  | ZipFormat
  | SevenZipFormat
  | RarFormat
  | GzipFormat      -- Single file
  | Bzip2Format     -- Single file
  | XzFormat        -- Single file
  | ZstdFormat      -- Single file

||| Compression level
public export
data CompressionLevel
  = NoCompression
  | FastCompression
  | DefaultCompression
  | MaxCompression
  | CustomLevel Nat

||| File permissions (Unix-style)
public export
record FilePermissions where
  constructor MkFilePermissions
  ownerRead : Bool
  ownerWrite : Bool
  ownerExecute : Bool
  groupRead : Bool
  groupWrite : Bool
  groupExecute : Bool
  otherRead : Bool
  otherWrite : Bool
  otherExecute : Bool
  setuid : Bool
  setgid : Bool
  sticky : Bool

||| Entry type in archive
public export
data EntryType
  = RegularFile
  | Directory
  | Symlink String       -- Target path
  | Hardlink String      -- Target path
  | CharDevice Nat Nat   -- Major, minor
  | BlockDevice Nat Nat
  | Fifo
  | Socket

||| Archive entry metadata
public export
record EntryMeta where
  constructor MkEntryMeta
  path : String
  entryType : EntryType
  size : Nat              -- Uncompressed size
  compressedSize : Maybe Nat
  modTime : Maybe String  -- ISO 8601
  permissions : Maybe FilePermissions
  owner : Maybe String
  group : Maybe String
  comment : Maybe String

||| Validated archive entry
public export
record SafeEntry where
  constructor MkSafeEntry
  meta : EntryMeta
  normalizedPath : String   -- Normalized, validated path
  withinBounds : Bool       -- Size within limits

||| Archive metadata
public export
record ArchiveMeta where
  constructor MkArchiveMeta
  format : ArchiveFormat
  entryCount : Nat
  totalSize : Nat           -- Uncompressed
  compressedSize : Nat
  compressionRatio : Double
  hasEncryption : Bool

||| Archive extraction options
public export
record ExtractOptions where
  constructor MkExtractOptions
  targetDir : String
  overwrite : Bool
  preservePermissions : Bool
  preserveOwnership : Bool
  preserveModTime : Bool
  extractSymlinks : Bool
  followSymlinks : Bool
  stripComponents : Nat     -- Remove leading path components
  includePatterns : List String
  excludePatterns : List String

||| Archive creation options
public export
record CreateOptions where
  constructor MkCreateOptions
  format : ArchiveFormat
  compression : CompressionLevel
  includeHidden : Bool
  followSymlinks : Bool
  preservePermissions : Bool
  excludePatterns : List String

||| Security limits for extraction
public export
record SecurityLimits where
  constructor MkSecurityLimits
  maxTotalSize : Nat        -- Max uncompressed size
  maxEntrySize : Nat        -- Max single entry size
  maxEntryCount : Nat       -- Max number of entries
  maxCompressionRatio : Double
  maxPathLength : Nat
  maxNestingDepth : Nat     -- For nested archives

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

||| Errors that can occur during archive operations
public export
data ArchiveError
  = UnsupportedFormat String
  | PathTraversal String
  | DecompressionBomb Double
  | EntrySizeExceeded String Nat Nat  -- path, actual, max
  | TotalSizeExceeded Nat Nat         -- actual, max
  | EntryCountExceeded Nat Nat        -- actual, max
  | DangerousSymlink String String    -- path, target
  | DangerousHardlink String String
  | DangerousPermissions String Nat   -- path, mode
  | InvalidEntryPath String
  | PathTooLong String Nat
  | NestingTooDeep Nat
  | EncryptedArchive
  | CorruptedArchive String
  | EmptyArchive
  | InvalidMagicNumber String
  | UnsupportedCompression String
  | ExtractionFailed String

public export
Show ArchiveError where
  show (UnsupportedFormat f) = "Archive error: unsupported format '" ++ f ++ "'"
  show (PathTraversal p) = "Archive security: path traversal detected '" ++ p ++ "'"
  show (DecompressionBomb ratio) = "Archive security: decompression bomb detected (ratio: " ++ show ratio ++ ")"
  show (EntrySizeExceeded path actual max) =
    "Archive error: entry '" ++ path ++ "' size " ++ show actual ++ " exceeds limit " ++ show max
  show (TotalSizeExceeded actual max) =
    "Archive error: total size " ++ show actual ++ " exceeds limit " ++ show max
  show (EntryCountExceeded actual max) =
    "Archive error: entry count " ++ show actual ++ " exceeds limit " ++ show max
  show (DangerousSymlink path target) =
    "Archive security: dangerous symlink '" ++ path ++ "' -> '" ++ target ++ "'"
  show (DangerousHardlink path target) =
    "Archive security: dangerous hardlink '" ++ path ++ "' -> '" ++ target ++ "'"
  show (DangerousPermissions path mode) =
    "Archive security: dangerous permissions on '" ++ path ++ "' (mode: " ++ show mode ++ ")"
  show (InvalidEntryPath p) = "Archive error: invalid entry path '" ++ p ++ "'"
  show (PathTooLong path len) =
    "Archive error: path too long '" ++ path ++ "' (" ++ show len ++ " chars)"
  show (NestingTooDeep depth) =
    "Archive error: nesting too deep (" ++ show depth ++ " levels)"
  show EncryptedArchive = "Archive error: encrypted archives not supported"
  show (CorruptedArchive msg) = "Archive error: corrupted archive - " ++ msg
  show EmptyArchive = "Archive error: empty archive"
  show (InvalidMagicNumber magic) = "Archive error: invalid magic number '" ++ magic ++ "'"
  show (UnsupportedCompression c) = "Archive error: unsupported compression '" ++ c ++ "'"
  show (ExtractionFailed msg) = "Archive error: extraction failed - " ++ msg

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

||| Default security limits
defaultLimits : SecurityLimits
defaultLimits = MkSecurityLimits
  (1024 * 1024 * 1024)  -- 1 GB max total
  (100 * 1024 * 1024)   -- 100 MB max per entry
  10000                  -- 10k max entries
  100.0                  -- 100:1 max compression ratio
  4096                   -- Max path length
  5                      -- Max nesting depth

||| Strict security limits
strictLimits : SecurityLimits
strictLimits = MkSecurityLimits
  (100 * 1024 * 1024)   -- 100 MB max total
  (10 * 1024 * 1024)    -- 10 MB max per entry
  1000                   -- 1k max entries
  20.0                   -- 20:1 max compression ratio
  256                    -- Max path length
  2                      -- Max nesting depth

||| Path traversal patterns
pathTraversalPatterns : List String
pathTraversalPatterns =
  [ "..", "..\\", "../", "/..", "\\.."
  , "%2e%2e", "%252e%252e"
  ]

||| Dangerous file extensions
dangerousExtensions : List String
dangerousExtensions =
  [ ".exe", ".dll", ".so", ".dylib"
  , ".bat", ".cmd", ".ps1", ".sh"
  , ".jar", ".war", ".class"
  , ".php", ".phar", ".asp", ".aspx", ".jsp"
  ]

||| Archive magic numbers (hex)
archiveMagicNumbers : List (String, ArchiveFormat)
archiveMagicNumbers =
  [ ("504B0304", ZipFormat)      -- PK\x03\x04
  , ("504B0506", ZipFormat)      -- Empty zip
  , ("504B0708", ZipFormat)      -- Spanned zip
  , ("1F8B", GzipFormat)         -- Gzip
  , ("425A68", Bzip2Format)      -- BZh
  , ("FD377A585A00", XzFormat)   -- XZ
  , ("28B52FFD", ZstdFormat)     -- Zstd
  , ("377ABCAF271C", SevenZipFormat)
  , ("526172211A07", RarFormat)  -- Rar 5.0
  ]

--------------------------------------------------------------------------------
-- Permission helpers
--------------------------------------------------------------------------------

||| Default safe permissions (0644)
public export
defaultFilePermissions : FilePermissions
defaultFilePermissions = MkFilePermissions
  True True False   -- owner
  True False False  -- group
  True False False  -- other
  False False False -- special

||| Default directory permissions (0755)
public export
defaultDirPermissions : FilePermissions
defaultDirPermissions = MkFilePermissions
  True True True    -- owner
  True False True   -- group
  True False True   -- other
  False False False -- special

||| Convert permissions to octal mode
public export
permissionsToMode : FilePermissions -> Nat
permissionsToMode p =
  let owner = (if p.ownerRead then 4 else 0) +
              (if p.ownerWrite then 2 else 0) +
              (if p.ownerExecute then 1 else 0)
      group = (if p.groupRead then 4 else 0) +
              (if p.groupWrite then 2 else 0) +
              (if p.groupExecute then 1 else 0)
      other = (if p.otherRead then 4 else 0) +
              (if p.otherWrite then 2 else 0) +
              (if p.otherExecute then 1 else 0)
      special = (if p.setuid then 4 else 0) +
                (if p.setgid then 2 else 0) +
                (if p.sticky then 1 else 0)
  in special * 512 + owner * 64 + group * 8 + other

||| Check if permissions are dangerous
public export
isDangerousPermissions : FilePermissions -> Bool
isDangerousPermissions p = p.setuid || p.setgid || p.otherWrite

||| Strip dangerous permission bits
public export
sanitizePermissions : FilePermissions -> FilePermissions
sanitizePermissions p = { setuid := False, setgid := False, otherWrite := False } p

--------------------------------------------------------------------------------
-- Path validation
--------------------------------------------------------------------------------

||| Normalize path (remove redundant components)
public export
normalizePath : String -> String
normalizePath path =
  let parts = split (\c => c == '/' || c == '\\') path
      filtered = filter (\p => p /= "" && p /= ".") parts
  in concat (intersperse "/" filtered)

||| Check for path traversal
public export
hasPathTraversal : String -> Bool
hasPathTraversal path =
  any (\p => isInfixOf p path) pathTraversalPatterns

||| Check if path is absolute
public export
isAbsolutePath : String -> Bool
isAbsolutePath path =
  case unpack path of
    ('/' :: _) => True
    (c :: ':' :: _ ) => isAlpha c  -- Windows drive letter
    _ => False

||| Check if path escapes root
public export
pathEscapesRoot : String -> Bool
pathEscapesRoot path =
  let normalized = normalizePath path
      parts = split (== '/') normalized
  in go 0 parts
  where
    go : Integer -> List String -> Bool
    go depth [] = depth < 0
    go depth (".." :: rest) = go (depth - 1) rest
    go depth ("" :: rest) = go depth rest
    go depth (_ :: rest) = go (depth + 1) rest

||| Validate entry path
public export
validatePath : SecurityLimits -> String -> Either ArchiveError String
validatePath limits path =
  if length path > limits.maxPathLength
    then Left (PathTooLong path (length path))
    else if hasPathTraversal path
      then Left (PathTraversal path)
      else if isAbsolutePath path
        then Left (PathTraversal ("absolute path: " ++ path))
        else if pathEscapesRoot path
          then Left (PathTraversal path)
          else Right (normalizePath path)

--------------------------------------------------------------------------------
-- Symlink validation
--------------------------------------------------------------------------------

||| Check if symlink target is safe
public export
isSymlinkSafe : String -> String -> Bool
isSymlinkSafe entryPath target =
  let targetNorm = normalizePath target
  in not (hasPathTraversal target) &&
     not (isAbsolutePath target) &&
     not (pathEscapesRoot (entryPath ++ "/../" ++ target))

||| Validate symlink
public export
validateSymlink : String -> String -> Either ArchiveError ()
validateSymlink path target =
  if isSymlinkSafe path target
    then Right ()
    else Left (DangerousSymlink path target)

--------------------------------------------------------------------------------
-- Size validation
--------------------------------------------------------------------------------

||| Calculate compression ratio
public export
compressionRatio : Nat -> Nat -> Double
compressionRatio compressed uncompressed =
  if compressed == 0
    then 0.0
    else cast uncompressed / cast compressed

||| Check for decompression bomb
public export
isDecompressionBomb : SecurityLimits -> Nat -> Nat -> Bool
isDecompressionBomb limits compressed uncompressed =
  compressionRatio compressed uncompressed > limits.maxCompressionRatio

||| Validate entry size
public export
validateEntrySize : SecurityLimits -> String -> Nat -> Either ArchiveError ()
validateEntrySize limits path size =
  if size > limits.maxEntrySize
    then Left (EntrySizeExceeded path size limits.maxEntrySize)
    else Right ()

||| Validate total size
public export
validateTotalSize : SecurityLimits -> Nat -> Either ArchiveError ()
validateTotalSize limits total =
  if total > limits.maxTotalSize
    then Left (TotalSizeExceeded total limits.maxTotalSize)
    else Right ()

--------------------------------------------------------------------------------
-- Entry validation
--------------------------------------------------------------------------------

||| Validate a single archive entry
public export
validateEntry : SecurityLimits -> EntryMeta -> Either ArchiveError SafeEntry
validateEntry limits entry = do
  -- Validate path
  normalizedPath <- validatePath limits entry.path

  -- Validate size
  _ <- validateEntrySize limits entry.path entry.size

  -- Validate symlinks
  case entry.entryType of
    Symlink target => validateSymlink entry.path target
    Hardlink target =>
      if hasPathTraversal target
        then Left (DangerousHardlink entry.path target)
        else Right ()
    _ => Right ()

  -- Validate permissions
  case entry.permissions of
    Just perms =>
      if isDangerousPermissions perms
        then Left (DangerousPermissions entry.path (permissionsToMode perms))
        else Right ()
    Nothing => Right ()

  pure (MkSafeEntry entry normalizedPath True)

||| Validate all entries in archive
public export
validateArchive : SecurityLimits -> List EntryMeta -> Either ArchiveError (List SafeEntry)
validateArchive limits entries = do
  -- Check entry count
  let entryCount = length entries
  if entryCount > limits.maxEntryCount
    then Left (EntryCountExceeded entryCount limits.maxEntryCount)
    else do
      -- Validate each entry
      validEntries <- traverse (validateEntry limits) entries

      -- Check total size
      let totalSize = sum (map (\e => e.meta.size) validEntries)
      _ <- validateTotalSize limits totalSize

      pure validEntries

--------------------------------------------------------------------------------
-- Format detection
--------------------------------------------------------------------------------

||| Show archive format
public export
showArchiveFormat : ArchiveFormat -> String
showArchiveFormat TarFormat = "tar"
showArchiveFormat TarGzFormat = "tar.gz"
showArchiveFormat TarBz2Format = "tar.bz2"
showArchiveFormat TarXzFormat = "tar.xz"
showArchiveFormat TarZstFormat = "tar.zst"
showArchiveFormat ZipFormat = "zip"
showArchiveFormat SevenZipFormat = "7z"
showArchiveFormat RarFormat = "rar"
showArchiveFormat GzipFormat = "gz"
showArchiveFormat Bzip2Format = "bz2"
showArchiveFormat XzFormat = "xz"
showArchiveFormat ZstdFormat = "zst"

||| Get file extension for format
public export
formatExtension : ArchiveFormat -> String
formatExtension TarFormat = ".tar"
formatExtension TarGzFormat = ".tar.gz"
formatExtension TarBz2Format = ".tar.bz2"
formatExtension TarXzFormat = ".tar.xz"
formatExtension TarZstFormat = ".tar.zst"
formatExtension ZipFormat = ".zip"
formatExtension SevenZipFormat = ".7z"
formatExtension RarFormat = ".rar"
formatExtension GzipFormat = ".gz"
formatExtension Bzip2Format = ".bz2"
formatExtension XzFormat = ".xz"
formatExtension ZstdFormat = ".zst"

||| Detect format from filename
public export
detectFormatFromName : String -> Maybe ArchiveFormat
detectFormatFromName name =
  let lower = toLower name in
  if isSuffixOf ".tar.gz" lower || isSuffixOf ".tgz" lower
    then Just TarGzFormat
    else if isSuffixOf ".tar.bz2" lower || isSuffixOf ".tbz2" lower
      then Just TarBz2Format
      else if isSuffixOf ".tar.xz" lower || isSuffixOf ".txz" lower
        then Just TarXzFormat
        else if isSuffixOf ".tar.zst" lower
          then Just TarZstFormat
          else if isSuffixOf ".tar" lower
            then Just TarFormat
            else if isSuffixOf ".zip" lower
              then Just ZipFormat
              else if isSuffixOf ".7z" lower
                then Just SevenZipFormat
                else if isSuffixOf ".rar" lower
                  then Just RarFormat
                  else if isSuffixOf ".gz" lower
                    then Just GzipFormat
                    else if isSuffixOf ".bz2" lower
                      then Just Bzip2Format
                      else if isSuffixOf ".xz" lower
                        then Just XzFormat
                        else if isSuffixOf ".zst" lower
                          then Just ZstdFormat
                          else Nothing

--------------------------------------------------------------------------------
-- Safe extraction options
--------------------------------------------------------------------------------

||| Create safe extraction options
public export
safeExtractOptions : String -> ExtractOptions
safeExtractOptions targetDir = MkExtractOptions
  targetDir
  False         -- Don't overwrite
  False         -- Don't preserve dangerous permissions
  False         -- Don't preserve ownership
  True          -- Preserve modification time
  False         -- Don't extract symlinks
  False         -- Don't follow symlinks
  0             -- Don't strip components
  []            -- No include patterns
  dangerousExtensions  -- Exclude dangerous extensions

||| Create strict extraction options
public export
strictExtractOptions : String -> ExtractOptions
strictExtractOptions targetDir = MkExtractOptions
  targetDir
  False
  False
  False
  False
  False
  False
  0
  []
  dangerousExtensions

--------------------------------------------------------------------------------
-- Safe creation options
--------------------------------------------------------------------------------

||| Create safe archive creation options
public export
safeCreateOptions : ArchiveFormat -> CreateOptions
safeCreateOptions format = MkCreateOptions
  format
  DefaultCompression
  False         -- Exclude hidden files
  False         -- Don't follow symlinks
  False         -- Don't preserve special permissions
  [".git", ".svn", ".hg", "node_modules", "__pycache__"]
