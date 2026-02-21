-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeArchive operations
|||
||| This module exports archive safety operations to the C ABI via Idris2's RefC backend.
||| All functions are proven total and prevent Zip Slip, zip bombs, and symlink attacks.
|||
||| Return conventions:
||| - Path validation -> (status: Int, value/error: String)
|||   - status = 0: Safe
|||   - status = 1: Unsafe, value contains reason
||| - Bool checks -> Int (0 = false, 1 = true)
module Proven.FFI.SafeArchive

import Proven.SafeArchive

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode EntryValidation as (status, message)
encodeValidation : EntryValidation -> (Int, String)
encodeValidation EntryOK = (0, "OK")
encodeValidation (ZipSlipDetected path) = (1, "Zip Slip: " ++ path)
encodeValidation (ZipBombDetected c u) = (1, "Zip bomb: " ++ show c ++ " -> " ++ show u)
encodeValidation (DangerousSymlink path) = (1, "Dangerous symlink: " ++ path)
encodeValidation NullByteInPath = (1, "Null byte in path")
encodeValidation AbsolutePathDetected = (1, "Absolute path detected")

--------------------------------------------------------------------------------
-- Path Traversal Detection
--------------------------------------------------------------------------------

export
proven_idris_archive_has_path_traversal : String -> Int
proven_idris_archive_has_path_traversal = encodeBool . hasPathTraversal

export
proven_idris_archive_is_safe_extraction_path : String -> String -> Int
proven_idris_archive_is_safe_extraction_path targetDir entryPath =
  encodeBool (isSafeExtractionPath targetDir entryPath)

--------------------------------------------------------------------------------
-- Zip Bomb Detection
--------------------------------------------------------------------------------

export
proven_idris_archive_is_zip_bomb : Int -> Int -> Int
proven_idris_archive_is_zip_bomb compressed uncompressed =
  let entry = MkArchiveEntry "" RegularFile (cast compressed) (cast uncompressed) Nothing
  in encodeBool (isZipBomb entry)

export
proven_idris_archive_max_compression_ratio : Int
proven_idris_archive_max_compression_ratio = cast maxCompressionRatio

export
proven_idris_archive_max_total_size : Int
proven_idris_archive_max_total_size = cast maxTotalSize

--------------------------------------------------------------------------------
-- Entry Validation
--------------------------------------------------------------------------------

export
proven_idris_archive_validate_entry : String -> String -> Int -> Int -> Int -> (Int, String)
proven_idris_archive_validate_entry targetDir path compressed uncompressed isSymlink =
  let etype = if isSymlink == 1 then Symlink else RegularFile
      entry = MkArchiveEntry path etype (cast compressed) (cast uncompressed) Nothing
  in encodeValidation (validateEntry targetDir entry)

--------------------------------------------------------------------------------
-- Safe Extraction Path
--------------------------------------------------------------------------------

export
proven_idris_archive_safe_extract_path : String -> String -> (Int, String)
proven_idris_archive_safe_extract_path targetDir path =
  let entry = MkArchiveEntry path RegularFile 0 0 Nothing
  in case safeExtractPath targetDir entry of
    Nothing => (1, "Unsafe extraction path")
    Just p => (0, p)

--------------------------------------------------------------------------------
-- Symlink Safety
--------------------------------------------------------------------------------

export
proven_idris_archive_is_dangerous_symlink : String -> String -> Int
proven_idris_archive_is_dangerous_symlink path target =
  let entry = MkArchiveEntry path Symlink 0 0 (Just target)
  in encodeBool (isDangerousSymlink entry)
