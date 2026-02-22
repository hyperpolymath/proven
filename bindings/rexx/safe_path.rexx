/* SPDX-License-Identifier: PMPL-1.0-or-later                       */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)           */
/* <jonathan.jewell@open.ac.uk>                                      */
/*                                                                   */
/* Proven SafePath - REXX binding for path validation and            */
/* directory traversal prevention.                                   */
/*                                                                   */
/* All computation delegates to proven-cli (Idris 2 + Zig).         */
/* This file does NOT reimplement any path logic.                   */
/*                                                                   */
/* Compatible with:                                                  */
/*   - Regina REXX 3.x+                                             */
/*   - ooRexx 4.x+                                                  */
/*   - IBM z/OS TSO/E REXX                                          */
/*   - OS/2 Classic REXX                                            */
/*                                                                   */
/* Usage:                                                            */
/*   call safe_path.rexx                                             */
/*   if validate_path('../etc/passwd') = 'true' then                */
/*     say 'WARNING: directory traversal detected!'                 */

/* -------------------------------------------------------------------*/
/* CLI resolution                                                     */
/* -------------------------------------------------------------------*/

proven_cli: procedure
  cli = value('PROVEN_CLI',, 'ENVIRONMENT')
  if cli = '' then cli = 'proven-cli'
  return cli

/* Internal: call proven-cli path subcommand.                        */
/* Returns trimmed output on success, empty string on failure.       */
path_call: procedure
  parse arg func, input
  cli = proven_cli()
  cmd = cli 'path' func '"'input'"'
  address SYSTEM cmd WITH OUTPUT STEM out.
  if rc \= 0 then return ''
  if out.0 > 0 then return strip(out.1)
  return ''

/* -------------------------------------------------------------------*/
/* Public API                                                         */
/* -------------------------------------------------------------------*/

/* Check whether a path contains directory traversal patterns.       */
/* Returns 'true' if traversal is detected, 'false' if safe,        */
/* or empty string on error.                                         */
/* Usage: result = validate_path(path)                               */
validate_path: procedure
  parse arg path
  return path_call('has-traversal', path)

/* Sanitize a filename by removing dangerous characters.             */
/* Returns the sanitized name or empty string on error.              */
/* Usage: result = sanitize_filename(name)                           */
sanitize_filename: procedure
  parse arg name
  return path_call('sanitize-filename', name)

/* Safely join two path components.                                  */
/* Prevents traversal out of the base directory.                     */
/* Returns the joined path or empty string if unsafe.                */
/* Usage: result = safe_join(base, child)                            */
safe_join: procedure
  parse arg base, child
  return path_call('safe-join', base child)

/* Normalize a path (resolve . and redundant separators).            */
/* Does NOT resolve .. components for safety; use has-traversal      */
/* first.  Returns the normalized path or empty string on error.     */
/* Usage: result = normalize_path(path)                              */
normalize_path: procedure
  parse arg path
  return path_call('normalize', path)

/* Extract the file extension from a path.                           */
/* Returns the extension (without dot) or empty string.              */
/* Usage: result = get_extension(path)                               */
get_extension: procedure
  parse arg path
  return path_call('get-extension', path)
