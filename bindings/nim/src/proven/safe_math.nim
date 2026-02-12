# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe mathematical operations with overflow detection.

import std/options

proc safeDiv*(numerator, denominator: int64): Option[int64] =
  ## Safely divide two integers, returning None on division by zero.
  if denominator == 0:
    return none(int64)
  result = some(numerator div denominator)

proc safeMod*(numerator, denominator: int64): Option[int64] =
  ## Safely compute modulo, returning None on division by zero.
  if denominator == 0:
    return none(int64)
  result = some(numerator mod denominator)

proc safeAdd*(a, b: int64): Option[int64] =
  ## Safely add two integers with overflow detection.
  ## Returns None if overflow would occur.
  # Check for overflow
  if b > 0 and a > high(int64) - b:
    return none(int64)
  if b < 0 and a < low(int64) - b:
    return none(int64)
  result = some(a + b)

proc safeSub*(a, b: int64): Option[int64] =
  ## Safely subtract two integers with overflow detection.
  ## Returns None if overflow would occur.
  # Check for overflow
  if b < 0 and a > high(int64) + b:
    return none(int64)
  if b > 0 and a < low(int64) + b:
    return none(int64)
  result = some(a - b)

proc safeMul*(a, b: int64): Option[int64] =
  ## Safely multiply two integers with overflow detection.
  ## Returns None if overflow would occur.
  if a == 0 or b == 0:
    return some(0'i64)

  # Check for overflow using division
  if a > 0:
    if b > 0:
      if a > high(int64) div b:
        return none(int64)
    else:
      if b < low(int64) div a:
        return none(int64)
  else:
    if b > 0:
      if a < low(int64) div b:
        return none(int64)
    else:
      if a != 0 and b < high(int64) div a:
        return none(int64)

  result = some(a * b)
