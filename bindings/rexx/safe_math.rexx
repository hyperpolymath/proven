/* SPDX-License-Identifier: PMPL-1.0-or-later                       */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)           */
/* <jonathan.jewell@open.ac.uk>                                      */
/*                                                                   */
/* Proven SafeMath - REXX binding for safe arithmetic operations.    */
/*                                                                   */
/* All computation delegates to proven-cli (Idris 2 + Zig).         */
/* This file does NOT reimplement any arithmetic logic.             */
/*                                                                   */
/* Compatible with:                                                  */
/*   - Regina REXX 3.x+                                             */
/*   - ooRexx 4.x+                                                  */
/*   - IBM z/OS TSO/E REXX                                          */
/*   - OS/2 Classic REXX                                            */
/*                                                                   */
/* Usage:                                                            */
/*   call safe_math.rexx                                             */
/*   result = safe_add(100, 200)                                     */
/*   if result \= '' then say 'Sum:' result                         */
/*   else say 'Error: overflow or invalid input'                    */

/* -------------------------------------------------------------------*/
/* CLI resolution                                                     */
/* -------------------------------------------------------------------*/

proven_cli: procedure
  cli = value('PROVEN_CLI',, 'ENVIRONMENT')
  if cli = '' then cli = 'proven-cli'
  return cli

/* Internal: call proven-cli math subcommand.                        */
/* Returns trimmed output on success, empty string on failure.       */
math_call: procedure
  parse arg func, args
  cli = proven_cli()
  cmd = cli 'math' func args
  address SYSTEM cmd WITH OUTPUT STEM out.
  if rc \= 0 then return ''
  if out.0 > 0 then return strip(out.1)
  return ''

/* -------------------------------------------------------------------*/
/* Public API                                                         */
/* -------------------------------------------------------------------*/

/* Checked addition: returns sum or '' on overflow.                  */
/* Usage: result = safe_add(a, b)                                    */
safe_add: procedure
  parse arg a, b
  return math_call('add', a b)

/* Checked subtraction: returns difference or '' on underflow.       */
/* Usage: result = safe_sub(a, b)                                    */
safe_sub: procedure
  parse arg a, b
  return math_call('sub', a b)

/* Checked multiplication: returns product or '' on overflow.        */
/* Usage: result = safe_mul(a, b)                                    */
safe_mul: procedure
  parse arg a, b
  return math_call('mul', a b)

/* Checked division: returns quotient or '' on divide-by-zero.       */
/* Usage: result = safe_div(a, b)                                    */
safe_div: procedure
  parse arg a, b
  return math_call('div', a b)

/* Checked modulo: returns remainder or '' on divide-by-zero.        */
/* Usage: result = safe_mod(a, b)                                    */
safe_mod: procedure
  parse arg a, b
  return math_call('mod', a b)

/* Safe absolute value: returns |n| or '' on MIN_INT edge case.      */
/* Usage: result = safe_abs(n)                                       */
safe_abs: procedure
  parse arg n
  return math_call('abs', n)

/* Clamp value to range [lo, hi].                                    */
/* Usage: result = safe_clamp(lo, hi, val)                           */
safe_clamp: procedure
  parse arg lo, hi, val
  return math_call('clamp', lo hi val)

/* Safe exponentiation: returns base**exp or '' on overflow.         */
/* Usage: result = safe_pow(base, exp)                               */
safe_pow: procedure
  parse arg base, exp
  return math_call('pow', base exp)

/* Check whether a value is in range [lo, hi].                       */
/* Returns 'true', 'false', or '' on error.                          */
/* Usage: result = safe_in_range(val, lo, hi)                        */
safe_in_range: procedure
  parse arg val, lo, hi
  return math_call('in-range', val lo hi)
