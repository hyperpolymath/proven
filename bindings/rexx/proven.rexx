/* SPDX-License-Identifier: PMPL-1.0-or-later                       */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)           */
/* <jonathan.jewell@open.ac.uk>                                      */
/*                                                                   */
/* Proven Safety Library - REXX binding (master module)              */
/*                                                                   */
/* Thin CLI wrapper: ALL computation delegates to libproven          */
/* (Idris 2 + Zig) via the proven-cli binary.  This file does NOT   */
/* reimplement any logic.                                            */
/*                                                                   */
/* Prerequisites:                                                    */
/*   - proven-cli must be on PATH (built from ffi/zig/)             */
/*   - Alternatively, set the PROVEN_CLI environment variable       */
/*                                                                   */
/* Compatible with:                                                  */
/*   - Regina REXX 3.x+                                             */
/*   - ooRexx 4.x+                                                  */
/*   - IBM z/OS TSO/E REXX                                          */
/*   - OS/2 Classic REXX                                            */
/*                                                                   */
/* Usage:                                                            */
/*   call proven.rexx                                                */
/*   result = proven_safe_add(10, 20)                                */
/*   say result                                                      */

/* -------------------------------------------------------------------*/
/* CLI binary resolution                                              */
/* -------------------------------------------------------------------*/

/* Locate the proven-cli binary.  Checks PROVEN_CLI environment      */
/* variable first, then falls back to the bare command name.         */
proven_resolve_cli: procedure
  cli = value('PROVEN_CLI',, 'ENVIRONMENT')
  if cli = '' then cli = 'proven-cli'
  return cli

/* -------------------------------------------------------------------*/
/* Internal helper: invoke proven-cli and capture output              */
/* Returns the trimmed stdout on success, empty string on failure.   */
/* -------------------------------------------------------------------*/
proven_call: procedure
  parse arg module, func, args
  cli = proven_resolve_cli()
  cmd = cli module func args
  /* Execute and capture output via a temporary queue */
  call_result = ''
  address SYSTEM cmd WITH OUTPUT STEM out.
  if rc \= 0 then return ''
  if out.0 > 0 then call_result = strip(out.1)
  return call_result

/* -------------------------------------------------------------------*/
/* SafeMath - Arithmetic that cannot crash                            */
/* -------------------------------------------------------------------*/

/* Safe addition with overflow checking.                             */
/* Usage: result = proven_safe_add(a, b)                             */
proven_safe_add: procedure
  parse arg a, b
  return proven_call('math', 'add', a b)

/* Safe subtraction with underflow checking.                         */
/* Usage: result = proven_safe_sub(a, b)                             */
proven_safe_sub: procedure
  parse arg a, b
  return proven_call('math', 'sub', a b)

/* Safe multiplication with overflow checking.                       */
/* Usage: result = proven_safe_mul(a, b)                             */
proven_safe_mul: procedure
  parse arg a, b
  return proven_call('math', 'mul', a b)

/* Safe division with divide-by-zero protection.                     */
/* Usage: result = proven_safe_div(a, b)                             */
proven_safe_div: procedure
  parse arg a, b
  return proven_call('math', 'div', a b)

/* Safe modulo with divide-by-zero protection.                       */
/* Usage: result = proven_safe_mod(a, b)                             */
proven_safe_mod: procedure
  parse arg a, b
  return proven_call('math', 'mod', a b)

/* Safe absolute value.                                              */
/* Usage: result = proven_safe_abs(n)                                */
proven_safe_abs: procedure
  parse arg n
  return proven_call('math', 'abs', n)

/* Clamp a value to the given range [lo, hi].                        */
/* Usage: result = proven_safe_clamp(lo, hi, val)                    */
proven_safe_clamp: procedure
  parse arg lo, hi, val
  return proven_call('math', 'clamp', lo hi val)

/* Safe exponentiation with overflow checking.                       */
/* Usage: result = proven_safe_pow(base, exp)                        */
proven_safe_pow: procedure
  parse arg base, exp
  return proven_call('math', 'pow', base exp)

/* -------------------------------------------------------------------*/
/* SafeString - Text operations that handle encoding safely           */
/* -------------------------------------------------------------------*/

/* Sanitize a string by removing unsafe characters.                  */
/* Usage: result = proven_sanitize_string(str)                       */
proven_sanitize_string: procedure
  parse arg str
  return proven_call('string', 'sanitize', str)

/* Escape string for HTML output (XSS prevention).                   */
/* Usage: result = proven_escape_html(str)                           */
proven_escape_html: procedure
  parse arg str
  return proven_call('string', 'escape-html', str)

/* Escape string for SQL (injection prevention).                     */
/* Usage: result = proven_escape_sql(str)                            */
proven_escape_sql: procedure
  parse arg str
  return proven_call('string', 'escape-sql', str)

/* Check if a string is valid UTF-8.                                 */
/* Usage: result = proven_is_valid_utf8(str)                         */
proven_is_valid_utf8: procedure
  parse arg str
  return proven_call('string', 'is-valid-utf8', str)

/* -------------------------------------------------------------------*/
/* SafeEmail - Email validation                                       */
/* -------------------------------------------------------------------*/

/* Validate an email address per RFC 5321.                           */
/* Usage: result = proven_validate_email(addr)                       */
proven_validate_email: procedure
  parse arg addr
  return proven_call('email', 'is-valid', addr)

/* Parse an email into local part and domain.                        */
/* Usage: result = proven_parse_email(addr)                          */
proven_parse_email: procedure
  parse arg addr
  return proven_call('email', 'parse', addr)

/* -------------------------------------------------------------------*/
/* SafeUrl - URL parsing and validation                               */
/* -------------------------------------------------------------------*/

/* Validate a URL.                                                   */
/* Usage: result = proven_validate_url(url)                          */
proven_validate_url: procedure
  parse arg url
  return proven_call('url', 'parse', url)

/* -------------------------------------------------------------------*/
/* SafeNetwork - IP address operations                                */
/* -------------------------------------------------------------------*/

/* Validate an IPv4 address.                                         */
/* Usage: result = proven_validate_ipv4(addr)                        */
proven_validate_ipv4: procedure
  parse arg addr
  return proven_call('network', 'parse-ipv4', addr)

/* Check whether an IPv4 address is private.                         */
/* Usage: result = proven_is_private_ip(addr)                        */
proven_is_private_ip: procedure
  parse arg addr
  return proven_call('network', 'ipv4-is-private', addr)

/* Check whether an IPv4 address is loopback.                        */
/* Usage: result = proven_is_loopback(addr)                          */
proven_is_loopback: procedure
  parse arg addr
  return proven_call('network', 'ipv4-is-loopback', addr)

/* -------------------------------------------------------------------*/
/* SafePath - Filesystem traversal prevention                         */
/* -------------------------------------------------------------------*/

/* Validate a filesystem path for traversal attacks.                 */
/* Usage: result = proven_validate_path(path)                        */
proven_validate_path: procedure
  parse arg path
  return proven_call('path', 'has-traversal', path)

/* Sanitize a filename, removing dangerous characters.               */
/* Usage: result = proven_sanitize_filename(name)                    */
proven_sanitize_filename: procedure
  parse arg name
  return proven_call('path', 'sanitize-filename', name)

/* -------------------------------------------------------------------*/
/* SafeCrypto - Cryptographic primitives                              */
/* -------------------------------------------------------------------*/

/* Compute SHA-256 hash of input.                                    */
/* Usage: result = proven_hash_sha256(data)                          */
proven_hash_sha256: procedure
  parse arg data
  return proven_call('crypto', 'sha256', data)

/* Constant-time comparison of two strings.                          */
/* Usage: result = proven_constant_time_eq(a, b)                     */
proven_constant_time_eq: procedure
  parse arg a, b
  return proven_call('crypto', 'constant-time-eq', a b)

/* Generate random bytes (hex-encoded).                              */
/* Usage: result = proven_random_hex(nbytes)                         */
proven_random_hex: procedure
  parse arg nbytes
  if nbytes = '' then nbytes = 32
  return proven_call('crypto', 'random-bytes', nbytes)

/* -------------------------------------------------------------------*/
/* SafeHex - Hexadecimal encoding/decoding                            */
/* -------------------------------------------------------------------*/

/* Encode a string to hexadecimal.                                   */
/* Usage: result = proven_hex_encode(data)                           */
proven_hex_encode: procedure
  parse arg data
  return proven_call('hex', 'encode', data)

/* Decode a hexadecimal string.                                      */
/* Usage: result = proven_hex_decode(hexstr)                         */
proven_hex_decode: procedure
  parse arg hexstr
  return proven_call('hex', 'decode', hexstr)

/* -------------------------------------------------------------------*/
/* SafeJson - JSON validation                                         */
/* -------------------------------------------------------------------*/

/* Check whether a string is valid JSON.                             */
/* Usage: result = proven_validate_json(str)                         */
proven_validate_json: procedure
  parse arg str
  return proven_call('json', 'is-valid', str)

/* Get the type of a JSON value (object, array, string, number, etc.)*/
/* Usage: result = proven_json_type(str)                             */
proven_json_type: procedure
  parse arg str
  return proven_call('json', 'get-type', str)

/* -------------------------------------------------------------------*/
/* SafeDateTime - ISO 8601 date/time operations                       */
/* -------------------------------------------------------------------*/

/* Format the current date/time as ISO 8601.                         */
/* Usage: result = proven_format_datetime(isostr)                    */
proven_format_datetime: procedure
  parse arg isostr
  return proven_call('datetime', 'parse', isostr)

/* Check whether a year is a leap year.                              */
/* Usage: result = proven_is_leap_year(year)                         */
proven_is_leap_year: procedure
  parse arg year
  return proven_call('datetime', 'is-leap-year', year)

/* -------------------------------------------------------------------*/
/* SafeUUID - UUID generation and validation                          */
/* -------------------------------------------------------------------*/

/* Generate a UUID v4.                                               */
/* Usage: result = proven_uuid_v4()                                  */
proven_uuid_v4: procedure
  return proven_call('uuid', 'v4', '')

/* Parse and validate a UUID string.                                 */
/* Usage: result = proven_parse_uuid(uuidstr)                        */
proven_parse_uuid: procedure
  parse arg uuidstr
  return proven_call('uuid', 'parse', uuidstr)

/* -------------------------------------------------------------------*/
/* Library information                                                */
/* -------------------------------------------------------------------*/

/* Return the library version string.                                */
/* Usage: ver = proven_version()                                     */
proven_version: procedure
  return proven_call('version', 'info', '')

/* Return the number of available modules.                           */
/* Usage: count = proven_module_count()                              */
proven_module_count: procedure
  return proven_call('version', 'module-count', '')
