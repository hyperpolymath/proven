/* SPDX-License-Identifier: PMPL-1.0-or-later                       */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)           */
/* <jonathan.jewell@open.ac.uk>                                      */
/*                                                                   */
/* Proven SafeJson - REXX binding for JSON validation.              */
/*                                                                   */
/* All computation delegates to proven-cli (Idris 2 + Zig).         */
/* This file does NOT reimplement any JSON parsing logic.           */
/*                                                                   */
/* Compatible with:                                                  */
/*   - Regina REXX 3.x+                                             */
/*   - ooRexx 4.x+                                                  */
/*   - IBM z/OS TSO/E REXX                                          */
/*   - OS/2 Classic REXX                                            */
/*                                                                   */
/* Usage:                                                            */
/*   call safe_json.rexx                                             */
/*   if validate_json('{"key": "value"}') \= '' then                */
/*     say 'Valid JSON'                                              */

/* -------------------------------------------------------------------*/
/* CLI resolution                                                     */
/* -------------------------------------------------------------------*/

proven_cli: procedure
  cli = value('PROVEN_CLI',, 'ENVIRONMENT')
  if cli = '' then cli = 'proven-cli'
  return cli

/* Internal: call proven-cli json subcommand.                        */
/* Returns trimmed output on success, empty string on failure.       */
json_call: procedure
  parse arg func, input
  cli = proven_cli()
  cmd = cli 'json' func "'"input"'"
  address SYSTEM cmd WITH OUTPUT STEM out.
  if rc \= 0 then return ''
  if out.0 > 0 then return strip(out.1)
  return ''

/* -------------------------------------------------------------------*/
/* Public API                                                         */
/* -------------------------------------------------------------------*/

/* Check whether a string is valid JSON.                             */
/* Returns 'true' if valid, 'false' otherwise, '' on error.          */
/* Usage: result = validate_json(str)                                */
validate_json: procedure
  parse arg str
  return json_call('is-valid', str)

/* Get the type of a JSON value.                                     */
/* Returns one of: object, array, string, number, boolean, null.     */
/* Returns empty string on parse error.                              */
/* Usage: result = json_type(str)                                    */
json_type: procedure
  parse arg str
  return json_call('get-type', str)

/* Pretty-print a JSON string with indentation.                      */
/* Returns the formatted string or empty string on error.            */
/* Usage: result = json_pretty(str)                                  */
json_pretty: procedure
  parse arg str
  return json_call('pretty', str)

/* Minify a JSON string by removing whitespace.                      */
/* Returns the compact string or empty string on error.              */
/* Usage: result = json_minify(str)                                  */
json_minify: procedure
  parse arg str
  return json_call('minify', str)

/* Validate JSON against a size limit (bytes).                       */
/* Returns 'true' if within limit, 'false' if too large, '' on err.  */
/* Usage: result = json_check_size(str, maxbytes)                    */
json_check_size: procedure
  parse arg str, maxbytes
  if maxbytes = '' then maxbytes = 1048576
  return json_call('check-size', str maxbytes)

/* Count the depth of nested JSON structures.                        */
/* Returns the depth as a number, or empty string on error.          */
/* Usage: result = json_depth(str)                                   */
json_depth: procedure
  parse arg str
  return json_call('depth', str)
