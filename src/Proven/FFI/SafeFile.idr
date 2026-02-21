-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeFile operations
|||
||| This module exports safe file utilities to the C ABI via Idris2's RefC backend.
||| All functions are proven total and protect against path traversal and resource exhaustion.
|||
||| Return conventions:
||| - Path validation → (status: Int, result: String)
|||   - status = 0: Valid path, result contains normalized path
|||   - status = 1: Invalid path, result contains error message
||| - Path utilities → String (always succeed, return processed path)
||| - Extension detection → (status: Int, extension: String)
|||   - status = 0: Has extension, extension contains value
|||   - status = 1: No extension
||| - Bool → Int (0 = false, 1 = true)
|||
||| CRITICAL: Use strict defaults for untrusted paths. Never allow path traversal (../).
|||
||| NOTE: Actual file I/O operations (readFile, writeFile) not exposed as they require
||| IO effects. This module exports path validation and manipulation utilities.
module Proven.FFI.SafeFile

import Proven.SafeFile
import Proven.SafeFile.Types
import Proven.SafeFile.Operations
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode FileResult as (status, result/error)
encodeFileResult : Show a => FileResult a -> (Int, String)
encodeFileResult (Err err) = (1, show err)
encodeFileResult (Ok val) = (0, show val)

||| Encode Maybe String as (status, value)
encodeMaybeString : Maybe String -> (Int, String)
encodeMaybeString Nothing = (1, "")
encodeMaybeString (Just s) = (0, s)

||| Encode FileMode as Int
encodeFileMode : FileMode -> Int
encodeFileMode ReadOnly = 0
encodeFileMode WriteOnly = 1
encodeFileMode AppendOnly = 2
encodeFileMode ReadWrite = 3

||| Encode FileType as Int
encodeFileType : FileType -> Int
encodeFileType RegularFile = 0
encodeFileType Directory = 1
encodeFileType SymLink = 2
encodeFileType OtherType = 3

--------------------------------------------------------------------------------
-- Path Validation
--------------------------------------------------------------------------------

export
proven_idris_file_is_safe_path : String -> Int
proven_idris_file_is_safe_path path =
  case validatePathDefault path of
    Ok _ => 1  -- Safe
    Err _ => 0  -- Unsafe

export
proven_idris_file_validate_path : String -> (Int, String)
proven_idris_file_validate_path path =
  case validatePathDefault path of
    Ok sp => (0, sp.rawPath)
    Err err => (1, show err)

export
proven_idris_file_has_dangerous_pattern : String -> Int
proven_idris_file_has_dangerous_pattern path =
  encodeBool (hasDangerousPattern path)

export
proven_idris_file_is_in_allowed_dir : String -> String -> Int
proven_idris_file_is_in_allowed_dir allowedDirs path =
  let dirs = split (== ',') allowedDirs
  in encodeBool (isInAllowedDir dirs path)

export
proven_idris_file_is_blocked_path : String -> String -> Int
proven_idris_file_is_blocked_path blockedPaths path =
  let paths = split (== ',') blockedPaths
  in encodeBool (isBlockedPath paths path)

--------------------------------------------------------------------------------
-- Path Utilities
--------------------------------------------------------------------------------

export
proven_idris_file_normalize_path : String -> String
proven_idris_file_normalize_path path = normalizePath path

export
proven_idris_file_filename : String -> String
proven_idris_file_filename path = filename path

export
proven_idris_file_dirname : String -> String
proven_idris_file_dirname path = dirname path

export
proven_idris_file_extension : String -> (Int, String)
proven_idris_file_extension path = encodeMaybeString (extension path)

export
proven_idris_file_join_path : String -> String
proven_idris_file_join_path componentsStr =
  let components = split (== ',') componentsStr
  in joinPath components

export
proven_idris_file_combine_path : String -> String -> String
proven_idris_file_combine_path base rel = combinePath base rel

--------------------------------------------------------------------------------
-- Path Limits Constants
--------------------------------------------------------------------------------

export
proven_idris_file_max_path_length : Int
proven_idris_file_max_path_length = cast maxPathLength

export
proven_idris_file_max_filename_length : Int
proven_idris_file_max_filename_length = cast maxFilenameLength

export
proven_idris_file_default_read_limit : Int
proven_idris_file_default_read_limit = cast defaultReadLimit

export
proven_idris_file_default_write_limit : Int
proven_idris_file_default_write_limit = cast defaultWriteLimit

export
proven_idris_file_strict_read_limit : Int
proven_idris_file_strict_read_limit = cast strictReadLimit

export
proven_idris_file_strict_write_limit : Int
proven_idris_file_strict_write_limit = cast strictWriteLimit

--------------------------------------------------------------------------------
-- Options Constants
--------------------------------------------------------------------------------

export
proven_idris_file_default_allow_traversal : Int
proven_idris_file_default_allow_traversal = encodeBool defaultOptions.allowTraversal

export
proven_idris_file_default_follow_symlinks : Int
proven_idris_file_default_follow_symlinks = encodeBool defaultOptions.followSymlinks

export
proven_idris_file_strict_allow_traversal : Int
proven_idris_file_strict_allow_traversal = encodeBool strictOptions.allowTraversal

export
proven_idris_file_strict_follow_symlinks : Int
proven_idris_file_strict_follow_symlinks = encodeBool strictOptions.followSymlinks

export
proven_idris_file_permissive_allow_traversal : Int
proven_idris_file_permissive_allow_traversal = encodeBool permissiveOptions.allowTraversal

export
proven_idris_file_permissive_follow_symlinks : Int
proven_idris_file_permissive_follow_symlinks = encodeBool permissiveOptions.followSymlinks

--------------------------------------------------------------------------------
-- Limit Checking
--------------------------------------------------------------------------------

export
proven_idris_file_check_path_length : String -> Int
proven_idris_file_check_path_length path =
  if length (unpack path) <= maxPathLength
    then 1  -- Valid
    else 0  -- Too long

export
proven_idris_file_check_filename_length : String -> Int
proven_idris_file_check_filename_length name =
  if length (unpack name) <= maxFilenameLength
    then 1  -- Valid
    else 0  -- Too long

export
proven_idris_file_check_read_size : Int -> Int -> Int
proven_idris_file_check_read_size maxSize requested =
  if requested <= maxSize
    then 1  -- Within limit
    else 0  -- Exceeds limit

--------------------------------------------------------------------------------
-- Error Classification
--------------------------------------------------------------------------------

export
proven_idris_file_is_path_too_long_error : String -> Int
proven_idris_file_is_path_too_long_error errorMsg =
  if isInfixOf "path too long" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_file_is_invalid_path_error : String -> Int
proven_idris_file_is_invalid_path_error errorMsg =
  if isInfixOf "invalid path" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_file_is_path_traversal_error : String -> Int
proven_idris_file_is_path_traversal_error errorMsg =
  if isInfixOf "traversal" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_file_is_not_found_error : String -> Int
proven_idris_file_is_not_found_error errorMsg =
  if isInfixOf "not found" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_file_is_permission_denied_error : String -> Int
proven_idris_file_is_permission_denied_error errorMsg =
  if isInfixOf "permission denied" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_file_is_read_limit_exceeded_error : String -> Int
proven_idris_file_is_read_limit_exceeded_error errorMsg =
  if isInfixOf "read limit exceeded" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_file_is_write_limit_exceeded_error : String -> Int
proven_idris_file_is_write_limit_exceeded_error errorMsg =
  if isInfixOf "write limit exceeded" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_file_is_file_too_large_error : String -> Int
proven_idris_file_is_file_too_large_error errorMsg =
  if isInfixOf "too large" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_file_is_handle_closed_error : String -> Int
proven_idris_file_is_handle_closed_error errorMsg =
  if isInfixOf "closed" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_file_is_invalid_operation_error : String -> Int
proven_idris_file_is_invalid_operation_error errorMsg =
  if isInfixOf "invalid operation" (toLower errorMsg)
    then 1
    else 0

--------------------------------------------------------------------------------
-- Friendly Error Messages
--------------------------------------------------------------------------------

export
proven_idris_file_friendly_error : String -> String
proven_idris_file_friendly_error errorMsg =
  if isInfixOf "traversal" (toLower errorMsg)
    then "Path traversal attack detected (../ or dangerous patterns)"
  else if isInfixOf "path too long" (toLower errorMsg)
    then "Path exceeds maximum length (possible buffer overflow)"
  else if isInfixOf "too large" (toLower errorMsg)
    then "File size exceeds limit (possible resource exhaustion)"
  else if isInfixOf "read limit exceeded" (toLower errorMsg)
    then "Read operation exceeds size limit"
  else if isInfixOf "write limit exceeded" (toLower errorMsg)
    then "Write operation exceeds size limit"
  else if isInfixOf "not found" (toLower errorMsg)
    then "File or directory not found"
  else if isInfixOf "permission denied" (toLower errorMsg)
    then "Access denied by security policy"
  else if isInfixOf "invalid path" (toLower errorMsg)
    then "Path contains invalid characters or format"
  else if isInfixOf "closed" (toLower errorMsg)
    then "File handle has been closed"
  else if isInfixOf "invalid operation" (toLower errorMsg)
    then "Operation not allowed for current file mode"
  else
    "File operation error"

--------------------------------------------------------------------------------
-- Dangerous Patterns Count
--------------------------------------------------------------------------------

export
proven_idris_file_dangerous_patterns_count : Int
proven_idris_file_dangerous_patterns_count = cast (length dangerousPatterns)
