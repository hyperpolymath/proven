-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeDocker (container) operations
|||
||| This module exports container image validation to the C ABI via Idris2's RefC backend.
||| All functions are proven total and validate image references.
|||
||| Return conventions:
||| - Image parsing -> (status: Int, value: String)
|||   - status = 0: Success, value contains result
|||   - status = 1: Error (invalid reference)
||| - Bool checks -> Int (0 = false, 1 = true)
module Proven.FFI.SafeDocker

import Proven.SafeDocker

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

--------------------------------------------------------------------------------
-- Image Reference Parsing
--------------------------------------------------------------------------------

export
proven_idris_docker_parse_image : String -> (Int, String)
proven_idris_docker_parse_image s = case parseImageRef s of
  Nothing => (1, "Invalid image reference")
  Just ref => (0, show ref)

export
proven_idris_docker_normalize_image : String -> (Int, String)
proven_idris_docker_normalize_image s = case parseImageRef s of
  Nothing => (1, "Invalid image reference")
  Just ref => (0, show (normalize ref))

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

export
proven_idris_docker_is_valid_tag : String -> Int
proven_idris_docker_is_valid_tag = encodeBool . isValidTag

export
proven_idris_docker_is_valid_digest : String -> Int
proven_idris_docker_is_valid_digest = encodeBool . isValidDigest

export
proven_idris_docker_is_valid_repo : String -> Int
proven_idris_docker_is_valid_repo = encodeBool . isValidRepo

--------------------------------------------------------------------------------
-- Security Checks
--------------------------------------------------------------------------------

export
proven_idris_docker_is_secure_base : String -> Int
proven_idris_docker_is_secure_base s = case parseImageRef s of
  Nothing => 0
  Just ref => encodeBool (isSecureBase ref)

export
proven_idris_docker_is_pinned_by_digest : String -> Int
proven_idris_docker_is_pinned_by_digest s = case parseImageRef s of
  Nothing => 0
  Just ref => encodeBool (isPinnedByDigest ref)

export
proven_idris_docker_uses_latest_tag : String -> Int
proven_idris_docker_uses_latest_tag s = case parseImageRef s of
  Nothing => 1  -- No tag defaults to latest
  Just ref => encodeBool (usesLatestTag ref)

--------------------------------------------------------------------------------
-- Image Components
--------------------------------------------------------------------------------

export
proven_idris_docker_get_registry : String -> (Int, String)
proven_idris_docker_get_registry s = case parseImageRef s of
  Nothing => (1, "Invalid image reference")
  Just ref => case registry ref of
    Nothing => (1, "No registry specified")
    Just reg => (0, show reg)

export
proven_idris_docker_get_repository : String -> (Int, String)
proven_idris_docker_get_repository s = case parseImageRef s of
  Nothing => (1, "Invalid image reference")
  Just ref => (0, show (repository ref))

export
proven_idris_docker_get_tag : String -> (Int, String)
proven_idris_docker_get_tag s = case parseImageRef s of
  Nothing => (1, "Invalid image reference")
  Just ref => case tag ref of
    Nothing => (1, "No tag specified")
    Just t => (0, show t)

export
proven_idris_docker_get_digest : String -> (Int, String)
proven_idris_docker_get_digest s = case parseImageRef s of
  Nothing => (1, "Invalid image reference")
  Just ref => case digest ref of
    Nothing => (1, "No digest specified")
    Just d => (0, show d)
