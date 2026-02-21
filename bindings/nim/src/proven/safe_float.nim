# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe floating point operations.

import std/[options, math]

proc isFinite*(x: float64): bool =
  ## Check if a float is finite (not NaN or Inf).
  not (x.isNaN or x == Inf or x == NegInf)

proc safeAdd*(a, b: float64): Option[float64] =
  ## Safely add two floats, returning None if result is not finite.
  let result_val = a + b
  if not isFinite(result_val):
    return none(float64)
  result = some(result_val)

proc safeSub*(a, b: float64): Option[float64] =
  ## Safely subtract two floats, returning None if result is not finite.
  let result_val = a - b
  if not isFinite(result_val):
    return none(float64)
  result = some(result_val)

proc safeMul*(a, b: float64): Option[float64] =
  ## Safely multiply two floats, returning None if result is not finite.
  let result_val = a * b
  if not isFinite(result_val):
    return none(float64)
  result = some(result_val)

proc safeDiv*(a, b: float64): Option[float64] =
  ## Safely divide two floats, returning None on division by zero or non-finite result.
  if b == 0.0:
    return none(float64)
  let result_val = a / b
  if not isFinite(result_val):
    return none(float64)
  result = some(result_val)

proc safeSqrt*(x: float64): Option[float64] =
  ## Safely compute square root, returning None for negative numbers.
  if x < 0.0:
    return none(float64)
  let result_val = sqrt(x)
  if not isFinite(result_val):
    return none(float64)
  result = some(result_val)

proc safePow*(base, exp: float64): Option[float64] =
  ## Safely compute power, returning None if result is not finite.
  let result_val = pow(base, exp)
  if not isFinite(result_val):
    return none(float64)
  result = some(result_val)

proc safeLog*(x: float64): Option[float64] =
  ## Safely compute natural log, returning None for non-positive numbers.
  if x <= 0.0:
    return none(float64)
  let result_val = ln(x)
  if not isFinite(result_val):
    return none(float64)
  result = some(result_val)

proc safeLog10*(x: float64): Option[float64] =
  ## Safely compute log base 10, returning None for non-positive numbers.
  if x <= 0.0:
    return none(float64)
  let result_val = log10(x)
  if not isFinite(result_val):
    return none(float64)
  result = some(result_val)

proc safeExp*(x: float64): Option[float64] =
  ## Safely compute e^x, returning None if result is not finite.
  let result_val = exp(x)
  if not isFinite(result_val):
    return none(float64)
  result = some(result_val)

proc clamp*(x, minVal, maxVal: float64): float64 =
  ## Clamp a value to a range.
  if x < minVal: minVal
  elif x > maxVal: maxVal
  else: x

proc almostEqual*(a, b: float64, epsilon: float64 = 1e-10): bool =
  ## Compare two floats for approximate equality.
  if a.isNaN or b.isNaN:
    return false
  abs(a - b) < epsilon

proc lerp*(a, b, t: float64): float64 =
  ## Linear interpolation between a and b.
  a + (b - a) * t

proc inverseLerp*(a, b, value: float64): Option[float64] =
  ## Inverse linear interpolation (get t from value).
  if almostEqual(a, b):
    return none(float64)
  result = some((value - a) / (b - a))

proc remap*(value, fromMin, fromMax, toMin, toMax: float64): Option[float64] =
  ## Remap a value from one range to another.
  let t = inverseLerp(fromMin, fromMax, value)
  if t.isNone:
    return none(float64)
  result = some(lerp(toMin, toMax, t.get()))

proc roundTo*(x: float64, decimals: int): float64 =
  ## Round a float to a specific number of decimal places.
  let factor = pow(10.0, decimals.float64)
  round(x * factor) / factor

proc sign*(x: float64): float64 =
  ## Get the sign of a number (-1, 0, or 1).
  if x < 0.0: -1.0
  elif x > 0.0: 1.0
  else: 0.0
