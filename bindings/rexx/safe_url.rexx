/* SPDX-License-Identifier: PMPL-1.0-or-later                       */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)           */
/* <jonathan.jewell@open.ac.uk>                                      */
/*                                                                   */
/* Proven SafeUrl - REXX binding for URL validation and parsing.    */
/*                                                                   */
/* All computation delegates to proven-cli (Idris 2 + Zig).         */
/* This file does NOT reimplement any URL logic.                    */
/*                                                                   */
/* Compatible with:                                                  */
/*   - Regina REXX 3.x+                                             */
/*   - ooRexx 4.x+                                                  */
/*   - IBM z/OS TSO/E REXX                                          */
/*   - OS/2 Classic REXX                                            */
/*                                                                   */
/* Usage:                                                            */
/*   call safe_url.rexx                                              */
/*   result = validate_url('https://example.com/path?q=1')          */
/*   if result \= '' then say 'Valid URL'                           */

/* -------------------------------------------------------------------*/
/* CLI resolution                                                     */
/* -------------------------------------------------------------------*/

proven_cli: procedure
  cli = value('PROVEN_CLI',, 'ENVIRONMENT')
  if cli = '' then cli = 'proven-cli'
  return cli

/* Internal: call proven-cli url subcommand.                         */
/* Returns trimmed output on success, empty string on failure.       */
url_call: procedure
  parse arg func, input
  cli = proven_cli()
  cmd = cli 'url' func '"'input'"'
  address SYSTEM cmd WITH OUTPUT STEM out.
  if rc \= 0 then return ''
  if out.0 > 0 then return strip(out.1)
  return ''

/* -------------------------------------------------------------------*/
/* Public API                                                         */
/* -------------------------------------------------------------------*/

/* Validate a URL and return parsed components.                      */
/* Returns parsed URL info on success, empty string on failure.      */
/* Usage: result = validate_url(url)                                 */
validate_url: procedure
  parse arg url
  return url_call('parse', url)

/* Check whether a URL uses HTTPS.                                   */
/* Returns 'true' or 'false', empty string on parse error.           */
/* Usage: result = is_https(url)                                     */
is_https: procedure
  parse arg url
  return url_call('is-https', url)

/* Extract the hostname from a URL.                                  */
/* Returns the hostname or empty string on failure.                  */
/* Usage: host = get_url_host(url)                                   */
get_url_host: procedure
  parse arg url
  return url_call('get-host', url)

/* Extract the path component from a URL.                            */
/* Returns the path or empty string on failure.                      */
/* Usage: path = get_url_path(url)                                   */
get_url_path: procedure
  parse arg url
  return url_call('get-path', url)

/* Encode a string for use in a URL query parameter.                 */
/* Returns the percent-encoded string or empty string on error.      */
/* Usage: result = url_encode(str)                                   */
url_encode: procedure
  parse arg str
  return url_call('encode', str)

/* Decode a percent-encoded URL string.                              */
/* Returns the decoded string or empty string on error.              */
/* Usage: result = url_decode(str)                                   */
url_decode: procedure
  parse arg str
  return url_call('decode', str)
