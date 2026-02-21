-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeGit operations
|||
||| This module exports git safety operations to the C ABI via Idris2's RefC backend.
||| All functions are proven total and prevent command injection.
|||
||| Return conventions:
||| - Validation -> (status: Int, value: String)
|||   - status = 0: Success/Valid
|||   - status = 1: Error/Invalid
||| - Bool checks -> Int (0 = false, 1 = true)
module Proven.FFI.SafeGit

import Proven.SafeGit

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

--------------------------------------------------------------------------------
-- Ref Name Validation
--------------------------------------------------------------------------------

export
proven_idris_git_is_valid_ref : String -> Int
proven_idris_git_is_valid_ref s = encodeBool (isValidRefName s)

export
proven_idris_git_mk_ref : String -> (Int, String)
proven_idris_git_mk_ref s = case mkGitRef s of
  Nothing => (1, "Invalid git ref name")
  Just ref => (0, refName ref)

export
proven_idris_git_is_valid_ref_char : Int -> Int
proven_idris_git_is_valid_ref_char c = encodeBool (isValidRefChar (chr c))

--------------------------------------------------------------------------------
-- Commit Message Handling
--------------------------------------------------------------------------------

export
proven_idris_git_sanitize_commit_message : String -> String
proven_idris_git_sanitize_commit_message = sanitizeCommitMessage

export
proven_idris_git_mk_commit_message : String -> String
proven_idris_git_mk_commit_message s = messageText (mkCommitMessage s)

--------------------------------------------------------------------------------
-- Branch Protection
--------------------------------------------------------------------------------

export
proven_idris_git_is_protected : String -> Int
proven_idris_git_is_protected s = case mkGitRef s of
  Nothing => 0
  Just ref => encodeBool (isProtected ref)

--------------------------------------------------------------------------------
-- Shell Escaping
--------------------------------------------------------------------------------

export
proven_idris_git_shell_escape : String -> String
proven_idris_git_shell_escape = shellEscape

--------------------------------------------------------------------------------
-- Path Sanitization
--------------------------------------------------------------------------------

export
proven_idris_git_sanitize_path : String -> (Int, String)
proven_idris_git_sanitize_path s = case sanitizePath s of
  Nothing => (1, "Unsafe path: contains traversal or absolute reference")
  Just p => (0, p)

--------------------------------------------------------------------------------
-- Clone URL Validation
--------------------------------------------------------------------------------

export
proven_idris_git_is_safe_clone_url : String -> Int
proven_idris_git_is_safe_clone_url = encodeBool . isSafeCloneUrl

--------------------------------------------------------------------------------
-- Command Building
--------------------------------------------------------------------------------

export
proven_idris_git_build_init : String
proven_idris_git_build_init = show (buildCommand GitInit)

export
proven_idris_git_build_status : String
proven_idris_git_build_status = show (buildCommand GitStatus)

export
proven_idris_git_build_diff : String
proven_idris_git_build_diff = show (buildCommand GitDiff)

export
proven_idris_git_build_stash : String
proven_idris_git_build_stash = show (buildCommand GitStash)

export
proven_idris_git_build_stash_pop : String
proven_idris_git_build_stash_pop = show (buildCommand GitStashPop)

export
proven_idris_git_build_log : Int -> String
proven_idris_git_build_log n = show (buildCommand (GitLog (cast n)))

export
proven_idris_git_build_clone : String -> (Int, String)
proven_idris_git_build_clone url =
  if isSafeCloneUrl url
    then (0, show (buildCommand (GitClone url)))
    else (1, "Unsafe clone URL")

export
proven_idris_git_build_commit : String -> String
proven_idris_git_build_commit msg =
  show (buildCommand (GitCommit (mkCommitMessage msg)))
