/* SPDX-License-Identifier: PMPL-1.0-or-later                       */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)           */
/* <jonathan.jewell@open.ac.uk>                                      */
/*                                                                   */
/* Proven SafeEmail - REXX binding for email validation.            */
/*                                                                   */
/* All computation delegates to proven-cli (Idris 2 + Zig).         */
/* This file does NOT reimplement any validation logic.             */
/*                                                                   */
/* Compatible with:                                                  */
/*   - Regina REXX 3.x+                                             */
/*   - ooRexx 4.x+                                                  */
/*   - IBM z/OS TSO/E REXX                                          */
/*   - OS/2 Classic REXX                                            */
/*                                                                   */
/* Usage:                                                            */
/*   call safe_email.rexx                                            */
/*   if validate_email('user@example.com') \= '' then               */
/*     say 'Valid email'                                             */

/* -------------------------------------------------------------------*/
/* CLI resolution                                                     */
/* -------------------------------------------------------------------*/

proven_cli: procedure
  cli = value('PROVEN_CLI',, 'ENVIRONMENT')
  if cli = '' then cli = 'proven-cli'
  return cli

/* Internal: call proven-cli email subcommand.                       */
/* Returns trimmed output on success, empty string on failure.       */
email_call: procedure
  parse arg func, input
  cli = proven_cli()
  cmd = cli 'email' func '"'input'"'
  address SYSTEM cmd WITH OUTPUT STEM out.
  if rc \= 0 then return ''
  if out.0 > 0 then return strip(out.1)
  return ''

/* -------------------------------------------------------------------*/
/* Public API                                                         */
/* -------------------------------------------------------------------*/

/* Validate an email address per RFC 5321.                           */
/* Returns 'true' if valid, empty string on invalid or error.        */
/* Usage: result = validate_email(addr)                              */
validate_email: procedure
  parse arg addr
  return email_call('is-valid', addr)

/* Parse an email address into local-part and domain.                */
/* Returns structured output on success, empty string on failure.    */
/* Usage: result = parse_email(addr)                                 */
parse_email: procedure
  parse arg addr
  return email_call('parse', addr)

/* Normalize an email address (lowercase domain, trim whitespace).   */
/* Returns the normalized form or empty string on error.             */
/* Usage: result = normalize_email(addr)                             */
normalize_email: procedure
  parse arg addr
  return email_call('normalize', addr)

/* Check whether an email uses a known disposable domain.            */
/* Returns 'true' if disposable, 'false' otherwise, '' on error.     */
/* Usage: result = is_disposable_email(addr)                         */
is_disposable_email: procedure
  parse arg addr
  return email_call('is-disposable', addr)
