/* SPDX-License-Identifier: PMPL-1.0-or-later                       */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)           */
/* <jonathan.jewell@open.ac.uk>                                      */
/*                                                                   */
/* Proven SafeString - REXX binding for safe string operations.     */
/*                                                                   */
/* All computation delegates to proven-cli (Idris 2 + Zig).         */
/* This file does NOT reimplement any string logic.                 */
/*                                                                   */
/* Compatible with:                                                  */
/*   - Regina REXX 3.x+                                             */
/*   - ooRexx 4.x+                                                  */
/*   - IBM z/OS TSO/E REXX                                          */
/*   - OS/2 Classic REXX                                            */
/*                                                                   */
/* Usage:                                                            */
/*   call safe_string.rexx                                           */
/*   safe = sanitize_string('<script>alert(1)</script>')             */
/*   say safe                                                        */

/* -------------------------------------------------------------------*/
/* CLI resolution                                                     */
/* -------------------------------------------------------------------*/

proven_cli: procedure
  cli = value('PROVEN_CLI',, 'ENVIRONMENT')
  if cli = '' then cli = 'proven-cli'
  return cli

/* Internal: call proven-cli string subcommand.                      */
/* Returns trimmed output on success, empty string on failure.       */
string_call: procedure
  parse arg func, input
  cli = proven_cli()
  cmd = cli 'string' func '"'input'"'
  address SYSTEM cmd WITH OUTPUT STEM out.
  if rc \= 0 then return ''
  if out.0 > 0 then return strip(out.1)
  return ''

/* -------------------------------------------------------------------*/
/* Public API                                                         */
/* -------------------------------------------------------------------*/

/* Sanitize a string by removing unsafe characters.                  */
/* Retains only alphanumeric, underscore, and hyphen.               */
/* Usage: result = sanitize_string(input)                            */
sanitize_string: procedure
  parse arg input
  return string_call('sanitize', input)

/* Escape string for HTML output (XSS prevention).                   */
/* Converts <, >, &, ", ' to HTML entities.                          */
/* Usage: result = escape_html(input)                                */
escape_html: procedure
  parse arg input
  return string_call('escape-html', input)

/* Escape string for SQL (injection prevention).                     */
/* Doubles single-quote characters.                                  */
/* Usage: result = escape_sql(input)                                 */
escape_sql: procedure
  parse arg input
  return string_call('escape-sql', input)

/* Escape string for JavaScript contexts.                            */
/* Usage: result = escape_js(input)                                  */
escape_js: procedure
  parse arg input
  return string_call('escape-js', input)

/* Check whether a string is valid UTF-8.                            */
/* Returns 'true' or 'false'.                                        */
/* Usage: result = is_valid_utf8(input)                              */
is_valid_utf8: procedure
  parse arg input
  return string_call('is-valid-utf8', input)

/* Convert a string to a URL-safe slug.                              */
/* Usage: result = slugify(input)                                    */
slugify: procedure
  parse arg input
  return string_call('slugify', input)

/* Truncate a string to a maximum byte length (UTF-8 safe).          */
/* Usage: result = safe_truncate(input, maxlen)                      */
safe_truncate: procedure
  parse arg input, maxlen
  return string_call('truncate', input maxlen)
