/* SPDX-License-Identifier: PMPL-1.0-or-later                       */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)           */
/* <jonathan.jewell@open.ac.uk>                                      */
/*                                                                   */
/* Proven SafeCrypto - REXX binding for cryptographic primitives.   */
/*                                                                   */
/* All computation delegates to proven-cli (Idris 2 + Zig).         */
/* This file does NOT reimplement any cryptographic logic.          */
/*                                                                   */
/* Compatible with:                                                  */
/*   - Regina REXX 3.x+                                             */
/*   - ooRexx 4.x+                                                  */
/*   - IBM z/OS TSO/E REXX                                          */
/*   - OS/2 Classic REXX                                            */
/*                                                                   */
/* Usage:                                                            */
/*   call safe_crypto.rexx                                           */
/*   hash = hash_sha256('hello world')                               */
/*   say hash                                                        */

/* -------------------------------------------------------------------*/
/* CLI resolution                                                     */
/* -------------------------------------------------------------------*/

proven_cli: procedure
  cli = value('PROVEN_CLI',, 'ENVIRONMENT')
  if cli = '' then cli = 'proven-cli'
  return cli

/* Internal: call proven-cli with a given module and function.       */
/* Returns trimmed output on success, empty string on failure.       */
crypto_call: procedure
  parse arg module, func, args
  cli = proven_cli()
  cmd = cli module func args
  address SYSTEM cmd WITH OUTPUT STEM out.
  if rc \= 0 then return ''
  if out.0 > 0 then return strip(out.1)
  return ''

/* -------------------------------------------------------------------*/
/* Hashing                                                            */
/* -------------------------------------------------------------------*/

/* Compute SHA-256 hash of input data.                               */
/* Returns 64-character hex string, or empty string on error.        */
/* Usage: result = hash_sha256(data)                                 */
hash_sha256: procedure
  parse arg data
  return crypto_call('crypto', 'sha256', '"'data'"')

/* Compute a simple hash of input data (non-cryptographic).          */
/* Returns hex string, or empty string on error.                     */
/* Usage: result = simple_hash(data)                                 */
simple_hash: procedure
  parse arg data
  return crypto_call('crypto', 'simple-hash', '"'data'"')

/* -------------------------------------------------------------------*/
/* Comparison                                                         */
/* -------------------------------------------------------------------*/

/* Constant-time comparison of two strings.                          */
/* Prevents timing side-channel attacks.                             */
/* Returns 'true' if equal, 'false' otherwise, '' on error.          */
/* Usage: result = constant_time_eq(a, b)                            */
constant_time_eq: procedure
  parse arg a, b
  return crypto_call('crypto', 'constant-time-eq', '"'a'" "'b'"')

/* -------------------------------------------------------------------*/
/* Random generation                                                  */
/* -------------------------------------------------------------------*/

/* Generate cryptographically secure random bytes (hex-encoded).     */
/* Returns hex string of 2*nbytes characters, or '' on error.        */
/* Usage: result = random_hex(32)                                    */
random_hex: procedure
  parse arg nbytes
  if nbytes = '' then nbytes = 32
  return crypto_call('crypto', 'random-bytes', nbytes)

/* Generate a random token suitable for session IDs or API keys.     */
/* Returns hex-encoded token, or empty string on error.              */
/* Usage: result = generate_token(length)                            */
generate_token: procedure
  parse arg length
  if length = '' then length = 32
  return crypto_call('crypto', 'generate-token', length)

/* -------------------------------------------------------------------*/
/* Hex encoding/decoding                                              */
/* -------------------------------------------------------------------*/

/* Encode a string to hexadecimal.                                   */
/* Returns hex-encoded string, or empty string on error.             */
/* Usage: result = hex_encode(data)                                  */
hex_encode: procedure
  parse arg data
  return crypto_call('hex', 'encode', '"'data'"')

/* Decode a hexadecimal string to its byte representation.           */
/* Returns decoded string, or empty string on error.                 */
/* Usage: result = hex_decode(hexstr)                                */
hex_decode: procedure
  parse arg hexstr
  return crypto_call('hex', 'decode', hexstr)

/* -------------------------------------------------------------------*/
/* Memory safety                                                      */
/* -------------------------------------------------------------------*/

/* Securely wipe a REXX variable.                                    */
/* Note: REXX does not support direct memory wiping. This function   */
/* overwrites the variable with zeros and then drops it.             */
/* Returns '1' to indicate completion.                               */
/* Usage: call secure_wipe 'MYVAR'                                   */
secure_wipe: procedure expose (varname)
  parse arg varname
  /* Overwrite with zeros then drop - best effort in REXX */
  interpret varname "= copies('00'x, length("varname"))"
  interpret "drop" varname
  return '1'
