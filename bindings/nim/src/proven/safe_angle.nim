# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe angle operations with degree/radian conversions.

import std/[options, math]

type
  Degrees* = object
    ## Angle in degrees.
    value*: float64

  Radians* = object
    ## Angle in radians.
    value*: float64

proc degrees*(value: float64): Degrees =
  ## Create an angle in degrees.
  Degrees(value: value)

proc radians*(value: float64): Radians =
  ## Create an angle in radians.
  Radians(value: value)

proc normalizeDegrees*(deg: float64): float64 =
  ## Normalize degrees to [0, 360).
  var result = deg mod 360.0
  if result < 0.0:
    result += 360.0
  result

proc normalizeRadians*(rad: float64): float64 =
  ## Normalize radians to [0, 2*PI).
  let twoPi = 2.0 * PI
  var result = rad mod twoPi
  if result < 0.0:
    result += twoPi
  result

proc normalize*(d: Degrees): Degrees =
  ## Normalize degrees to [0, 360).
  Degrees(value: normalizeDegrees(d.value))

proc normalize*(r: Radians): Radians =
  ## Normalize radians to [0, 2*PI).
  Radians(value: normalizeRadians(r.value))

proc normalizeSigned*(d: Degrees): Degrees =
  ## Normalize degrees to [-180, 180).
  var v = normalizeDegrees(d.value)
  if v >= 180.0:
    v -= 360.0
  Degrees(value: v)

proc toRadians*(d: Degrees): Radians =
  ## Convert degrees to radians.
  Radians(value: d.value * PI / 180.0)

proc toDegrees*(r: Radians): Degrees =
  ## Convert radians to degrees.
  Degrees(value: r.value * 180.0 / PI)

proc degToRad*(deg: float64): float64 =
  ## Convert degrees to radians.
  deg * PI / 180.0

proc radToDeg*(rad: float64): float64 =
  ## Convert radians to degrees.
  rad * 180.0 / PI

proc `+`*(a, b: Degrees): Degrees =
  ## Add two angles in degrees.
  Degrees(value: normalizeDegrees(a.value + b.value))

proc `-`*(a, b: Degrees): Degrees =
  ## Subtract two angles in degrees.
  Degrees(value: normalizeDegrees(a.value - b.value))

proc `+`*(a, b: Radians): Radians =
  ## Add two angles in radians.
  Radians(value: normalizeRadians(a.value + b.value))

proc `-`*(a, b: Radians): Radians =
  ## Subtract two angles in radians.
  Radians(value: normalizeRadians(a.value - b.value))

proc sin*(d: Degrees): float64 =
  ## Sine of angle in degrees.
  sin(d.toRadians().value)

proc cos*(d: Degrees): float64 =
  ## Cosine of angle in degrees.
  cos(d.toRadians().value)

proc tan*(d: Degrees): Option[float64] =
  ## Tangent of angle in degrees (None at 90, 270, etc.).
  let normalized = normalizeDegrees(d.value)
  if abs(normalized - 90.0) < 0.0001 or abs(normalized - 270.0) < 0.0001:
    return none(float64)
  result = some(tan(d.toRadians().value))

proc sin*(r: Radians): float64 =
  ## Sine of angle in radians.
  sin(r.value)

proc cos*(r: Radians): float64 =
  ## Cosine of angle in radians.
  cos(r.value)

proc tan*(r: Radians): Option[float64] =
  ## Tangent of angle in radians (None at PI/2, 3*PI/2, etc.).
  let normalized = normalizeRadians(r.value)
  if abs(normalized - PI/2) < 0.0001 or abs(normalized - 3*PI/2) < 0.0001:
    return none(float64)
  result = some(tan(r.value))

proc angleDiffDegrees*(a, b: float64): float64 =
  ## Calculate the shortest angle difference in degrees.
  let diff = normalizeDegrees(b - a)
  if diff > 180.0:
    diff - 360.0
  else:
    diff

proc angleDiffRadians*(a, b: float64): float64 =
  ## Calculate the shortest angle difference in radians.
  let diff = normalizeRadians(b - a)
  if diff > PI:
    diff - 2.0 * PI
  else:
    diff

proc lerpAngleDegrees*(a, b, t: float64): float64 =
  ## Linear interpolation between angles in degrees.
  let diff = angleDiffDegrees(a, b)
  normalizeDegrees(a + diff * t)

proc lerpAngleRadians*(a, b, t: float64): float64 =
  ## Linear interpolation between angles in radians.
  let diff = angleDiffRadians(a, b)
  normalizeRadians(a + diff * t)

proc arcsin*(x: float64): Option[Radians] =
  ## Safe arcsine, returning None for values outside [-1, 1].
  if x < -1.0 or x > 1.0:
    return none(Radians)
  result = some(Radians(value: arcsin(x)))

proc arccos*(x: float64): Option[Radians] =
  ## Safe arccosine, returning None for values outside [-1, 1].
  if x < -1.0 or x > 1.0:
    return none(Radians)
  result = some(Radians(value: arccos(x)))

proc arctan*(x: float64): Radians =
  ## Arctangent.
  Radians(value: arctan(x))

proc arctan2*(y, x: float64): Radians =
  ## Two-argument arctangent.
  Radians(value: arctan2(y, x))
