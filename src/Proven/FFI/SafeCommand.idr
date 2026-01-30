-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeCommand operations
|||
||| This module exports command injection prevention to the C ABI via Idris2's RefC backend.
||| All functions are proven total and prevent shell injection via proper escaping.
|||
||| Return conventions:
||| - Validation → (status: Int, error: String)
|||   - status = 0: Valid
|||   - status = 1: Invalid, error contains reason
||| - Escaping → String (always returns escaped string)
||| - Bool checks → Int (0 = false, 1 = true)
|||
||| CRITICAL: Always escape user input before passing to shell. Never concatenate raw user input into commands.
module Proven.FFI.SafeCommand

import Proven.SafeCommand
import Proven.SafeCommand.Escape
import Proven.SafeCommand.Builder
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode validation result as (status, error)
encodeValidation : Bool -> String -> (Int, String)
encodeValidation True _ = (0, "")
encodeValidation False reason = (1, reason)

||| Encode Maybe SafeCommand as (status, rendered/error)
encodeMaybeCommand : Maybe SafeCommand -> (Int, String)
encodeMaybeCommand Nothing = (1, "Invalid command name")
encodeMaybeCommand (Just cmd) = (0, render cmd)

--------------------------------------------------------------------------------
-- Command Name Validation
--------------------------------------------------------------------------------

%export
proven_idris_command_is_valid_name : String -> Int
proven_idris_command_is_valid_name name =
  encodeBool (isValidCommandName name)

%export
proven_idris_command_validate_name : String -> (Int, String)
proven_idris_command_validate_name name =
  encodeValidation (isValidCommandName name) ("Invalid command name: " ++ name)

%export
proven_idris_command_is_safe_path : String -> Int
proven_idris_command_is_safe_path path =
  encodeBool (isSafePath path)

--------------------------------------------------------------------------------
-- Argument Validation
--------------------------------------------------------------------------------

%export
proven_idris_command_is_flag : String -> Int
proven_idris_command_is_flag arg =
  encodeBool (isFlag arg)

%export
proven_idris_command_is_valid_flag : String -> Int
proven_idris_command_is_valid_flag flag =
  encodeBool (isValidFlag flag)

%export
proven_idris_command_contains_shell_meta : String -> Int
proven_idris_command_contains_shell_meta arg =
  encodeBool (containsShellMeta arg)

--------------------------------------------------------------------------------
-- Escaping Functions
--------------------------------------------------------------------------------

%export
proven_idris_command_escape_shell_arg : String -> String
proven_idris_command_escape_shell_arg = escapeShellArg

%export
proven_idris_command_escape_double_quoted : String -> String
proven_idris_command_escape_double_quoted = escapeDoubleQuoted

%export
proven_idris_command_escape_backslash : String -> String
proven_idris_command_escape_backslash = escapeBackslash

%export
proven_idris_command_escape_cmd_arg : String -> String
proven_idris_command_escape_cmd_arg = escapeCmdArg

%export
proven_idris_command_escape_cmd_bare : String -> String
proven_idris_command_escape_cmd_bare = escapeCmdBare

%export
proven_idris_command_escape_powershell : String -> String
proven_idris_command_escape_powershell = escapePowerShell

%export
proven_idris_command_escape_powershell_double : String -> String
proven_idris_command_escape_powershell_double = escapePowerShellDouble

%export
proven_idris_command_escape_path : String -> String
proven_idris_command_escape_path = escapePath

%export
proven_idris_command_escape_windows_path : String -> String
proven_idris_command_escape_windows_path = escapeWindowsPath

%export
proven_idris_command_escape_url_arg : String -> String
proven_idris_command_escape_url_arg = escapeUrlArg

--------------------------------------------------------------------------------
-- Environment Variable Escaping
--------------------------------------------------------------------------------

%export
proven_idris_command_escape_env_name : String -> (Int, String)
proven_idris_command_escape_env_name name =
  case escapeEnvName name of
    Nothing => (1, "Invalid environment variable name")
    Just n => (0, n)

%export
proven_idris_command_escape_env_value : String -> String
proven_idris_command_escape_env_value = escapeEnvValue

--------------------------------------------------------------------------------
-- Dangerous Pattern Detection
--------------------------------------------------------------------------------

%export
proven_idris_command_contains_dangerous : String -> Int
proven_idris_command_contains_dangerous arg =
  encodeBool (containsDangerousPattern arg)

%export
proven_idris_command_looks_like_substitution : String -> Int
proven_idris_command_looks_like_substitution arg =
  encodeBool (looksLikeSubstitution arg)

%export
proven_idris_command_looks_like_glob : String -> Int
proven_idris_command_looks_like_glob arg =
  encodeBool (looksLikeGlob arg)

%export
proven_idris_command_url_needs_escaping : String -> Int
proven_idris_command_url_needs_escaping url =
  encodeBool (urlNeedsEscaping url)

--------------------------------------------------------------------------------
-- Command Building (Simple Cases)
--------------------------------------------------------------------------------

%export
proven_idris_command_make : String -> (Int, String)
proven_idris_command_make name =
  encodeMaybeCommand (cmd name)

%export
proven_idris_command_echo : String -> String
proven_idris_command_echo msg = render (echo msg)

%export
proven_idris_command_cat : String -> String
proven_idris_command_cat file = render (cat [file])

%export
proven_idris_command_ls : String -> String
proven_idris_command_ls path = render (ls [path])

%export
proven_idris_command_mkdir : String -> String
proven_idris_command_mkdir path = render (mkdir path)

%export
proven_idris_command_mkdir_p : String -> String
proven_idris_command_mkdir_p path = render (mkdirP path)

%export
proven_idris_command_cp : String -> String -> String
proven_idris_command_cp src dst = render (cp src dst)

%export
proven_idris_command_mv : String -> String -> String
proven_idris_command_mv src dst = render (mv src dst)

%export
proven_idris_command_rm : String -> String
proven_idris_command_rm path = render (rm [path])

%export
proven_idris_command_grep : String -> String -> String
proven_idris_command_grep pattern file = render (grep pattern [file])

%export
proven_idris_command_find : String -> String -> String
proven_idris_command_find path pattern = render (find path pattern)

%export
proven_idris_command_curl : String -> String
proven_idris_command_curl url = render (curl url)

%export
proven_idris_command_wget : String -> String
proven_idris_command_wget url = render (wget url)

--------------------------------------------------------------------------------
-- Display/Debugging
--------------------------------------------------------------------------------

%export
proven_idris_command_unescape_display : String -> String
proven_idris_command_unescape_display = unescapeForDisplay

--------------------------------------------------------------------------------
-- Error Checking
--------------------------------------------------------------------------------

%export
proven_idris_command_is_injection_error : String -> Int
proven_idris_command_is_injection_error errorMsg =
  if isInfixOf "injection" (toLower errorMsg) || isInfixOf "dangerous" (toLower errorMsg)
    then 1
    else 0

%export
proven_idris_command_is_validation_error : String -> Int
proven_idris_command_is_validation_error errorMsg =
  if isInfixOf "invalid" (toLower errorMsg) || isInfixOf "validation" (toLower errorMsg)
    then 1
    else 0
