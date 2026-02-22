# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe mathematical operations with overflow detection.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

proc safeDiv*(numerator, denominator: int64): Option[int64] =
  ## Safely divide two integers, returning None on division by zero.
  let res = provenMathDiv(numerator, denominator)
  if res.status == PROVEN_OK:
    return some(res.value)
  none(int64)

proc safeMod*(numerator, denominator: int64): Option[int64] =
  ## Safely compute modulo, returning None on division by zero.
  let res = provenMathMod(numerator, denominator)
  if res.status == PROVEN_OK:
    return some(res.value)
  none(int64)

proc safeAdd*(a, b: int64): Option[int64] =
  ## Safely add two integers with overflow detection.
  ## Returns None if overflow would occur.
  let res = provenMathAddChecked(a, b)
  if res.status == PROVEN_OK:
    return some(res.value)
  none(int64)

proc safeSub*(a, b: int64): Option[int64] =
  ## Safely subtract two integers with overflow detection.
  ## Returns None if overflow would occur.
  let res = provenMathSubChecked(a, b)
  if res.status == PROVEN_OK:
    return some(res.value)
  none(int64)

proc safeMul*(a, b: int64): Option[int64] =
  ## Safely multiply two integers with overflow detection.
  ## Returns None if overflow would occur.
  let res = provenMathMulChecked(a, b)
  if res.status == PROVEN_OK:
    return some(res.value)
  none(int64)

proc safeAbs*(n: int64): Option[int64] =
  ## Safely compute absolute value (handles int64.low).
  ## Returns None if result would overflow.
  let res = provenMathAbsSafe(n)
  if res.status == PROVEN_OK:
    return some(res.value)
  none(int64)

proc clamp*(lo, hi, value: int64): int64 =
  ## Clamp a value to the range [lo, hi].
  provenMathClamp(lo, hi, value)

proc safePow*(base: int64, exp: uint32): Option[int64] =
  ## Safely compute integer exponentiation with overflow detection.
  ## Returns None if overflow would occur.
  let res = provenMathPowChecked(base, exp)
  if res.status == PROVEN_OK:
    return some(res.value)
  none(int64)
