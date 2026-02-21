-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeArchive - Archive handling with Zip Slip prevention
|||
||| Provides type-safe archive member validation that prevents:
||| - Zip Slip (path traversal via "../" in archive entries)
||| - Symlink attacks
||| - Zip bombs (excessive compression ratios)
||| - Path injection via null bytes
module Proven.SafeArchive

import Data.String
import Data.List
import Data.Nat
import Data.Maybe

%default total

||| Archive entry type
public export
data EntryType = RegularFile | Directory | Symlink | HardLink

public export
Eq EntryType where
  RegularFile == RegularFile = True
  Directory == Directory = True
  Symlink == Symlink = True
  HardLink == HardLink = True
  _ == _ = False

||| An archive entry (before extraction)
public export
record ArchiveEntry where
  constructor MkArchiveEntry
  entryPath       : String
  entryType       : EntryType
  compressedSize  : Nat
  uncompressedSize : Nat
  symlinkTarget   : Maybe String

||| Zip Slip detection: check for path traversal
public export
hasPathTraversal : String -> Bool
hasPathTraversal path =
  isInfixOf ".." path ||
  isPrefixOf "/" path ||
  isPrefixOf "\\" path ||
  isInfixOf "\x00" path ||
  isInfixOf "/./" path ||
  isInfixOf "\\./" path

||| Check if path is safe for extraction within a target directory
public export
isSafeExtractionPath : String -> String -> Bool
isSafeExtractionPath targetDir entryPath =
  not (hasPathTraversal entryPath) &&
  not (isPrefixOf "/" entryPath) &&
  not (isInfixOf ":" entryPath)

||| Maximum compression ratio to detect zip bombs
public export
maxCompressionRatio : Nat
maxCompressionRatio = 1000

||| Check for zip bomb (excessive compression ratio)
public export
isZipBomb : ArchiveEntry -> Bool
isZipBomb entry =
  case compressedSize entry of
    Z => uncompressedSize entry > 0
    n => div (uncompressedSize entry) n > maxCompressionRatio

||| Maximum total uncompressed size (1 GB)
public export
maxTotalSize : Nat
maxTotalSize = 1073741824

||| Check if total extraction size is safe
public export
isTotalSizeSafe : List ArchiveEntry -> Bool
isTotalSizeSafe entries =
  foldl (\acc, e => acc + uncompressedSize e) 0 entries <= maxTotalSize

||| Check for dangerous symlink targets
public export
isDangerousSymlink : ArchiveEntry -> Bool
isDangerousSymlink entry = case entryType entry of
  Symlink => case symlinkTarget entry of
    Nothing => False
    Just target => hasPathTraversal target || isPrefixOf "/" target
  _ => False

||| Validate a single archive entry
public export
data EntryValidation =
    EntryOK
  | ZipSlipDetected String
  | ZipBombDetected Nat Nat     -- compressed, uncompressed
  | DangerousSymlink String
  | NullByteInPath
  | AbsolutePathDetected

public export
Eq EntryValidation where
  EntryOK == EntryOK = True
  ZipSlipDetected _ == ZipSlipDetected _ = True
  ZipBombDetected _ _ == ZipBombDetected _ _ = True
  DangerousSymlink _ == DangerousSymlink _ = True
  NullByteInPath == NullByteInPath = True
  AbsolutePathDetected == AbsolutePathDetected = True
  _ == _ = False

||| Validate an archive entry
public export
validateEntry : String -> ArchiveEntry -> EntryValidation
validateEntry targetDir entry =
  if isInfixOf "\x00" (entryPath entry)
    then NullByteInPath
    else if isPrefixOf "/" (entryPath entry)
      then AbsolutePathDetected
      else if hasPathTraversal (entryPath entry)
        then ZipSlipDetected (entryPath entry)
        else if isZipBomb entry
          then ZipBombDetected (compressedSize entry) (uncompressedSize entry)
          else if isDangerousSymlink entry
            then DangerousSymlink (entryPath entry)
            else EntryOK

||| Validate all entries in an archive
public export
validateArchive : String -> List ArchiveEntry -> List (ArchiveEntry, EntryValidation)
validateArchive targetDir entries =
  let validated = map (\e => (e, validateEntry targetDir e)) entries
  in filter (\(_, v) => v /= EntryOK) validated

||| Check if an archive is completely safe
public export
isArchiveSafe : String -> List ArchiveEntry -> Bool
isArchiveSafe targetDir entries =
  all (\e => validateEntry targetDir e == EntryOK) entries &&
  isTotalSizeSafe entries

||| Safe extraction path: resolve entry path relative to target directory
public export
safeExtractPath : String -> ArchiveEntry -> Maybe String
safeExtractPath targetDir entry =
  if isSafeExtractionPath targetDir (entryPath entry)
    then Just (targetDir ++ "/" ++ entryPath entry)
    else Nothing

-- ----------------------------------------------------------------
-- Proof types
-- ----------------------------------------------------------------

||| Proof that an entry has no path traversal
public export
data NoTraversal : ArchiveEntry -> Type where
  MkNoTraversal : hasPathTraversal (entryPath e) = False -> NoTraversal e

||| Proof that an entry is not a zip bomb
public export
data NotZipBomb : ArchiveEntry -> Type where
  MkNotZipBomb : isZipBomb e = False -> NotZipBomb e

||| Proof that an archive is safe for extraction
public export
data SafeArchive : String -> List ArchiveEntry -> Type where
  MkSafeArchive : isArchiveSafe dir entries = True -> SafeArchive dir entries

||| Proof that symlinks are safe
public export
data SafeSymlinks : List ArchiveEntry -> Type where
  MkSafeSymlinks : all (\e => not (isDangerousSymlink e)) entries = True ->
                   SafeSymlinks entries
