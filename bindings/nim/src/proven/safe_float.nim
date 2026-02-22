# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe floating point operations.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

proc isFinite*(x: float64): bool =
  ## Check if a float is finite (not NaN or Inf).
  provenFloatIsFinite(x)

proc isNan*(x: float64): bool =
  ## Check if a float is NaN.
  provenFloatIsNan(x)

proc safeDiv*(a, b: float64): Option[float64] =
  ## Safely divide two floats, returning None on division by zero
  ## or non-finite result.
  let res = provenFloatDiv(a, b)
  if res.status == PROVEN_OK:
    return some(res.value)
  none(float64)

proc safeSqrt*(x: float64): Option[float64] =
  ## Safely compute square root, returning None for negative numbers.
  let res = provenFloatSqrt(x)
  if res.status == PROVEN_OK:
    return some(res.value)
  none(float64)

proc safeLog*(x: float64): Option[float64] =
  ## Safely compute natural log, returning None for non-positive numbers.
  let res = provenFloatLn(x)
  if res.status == PROVEN_OK:
    return some(res.value)
  none(float64)
