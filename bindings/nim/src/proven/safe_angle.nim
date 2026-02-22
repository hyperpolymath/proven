# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe angle operations with degree/radian conversions.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import lib_proven

proc degToRad*(degrees: float64): float64 =
  ## Convert degrees to radians.
  provenAngleDegToRad(degrees)

proc radToDeg*(radians: float64): float64 =
  ## Convert radians to degrees.
  provenAngleRadToDeg(radians)

proc normalizeDegrees*(degrees: float64): float64 =
  ## Normalize degrees to [0, 360).
  provenAngleNormalizeDegrees(degrees)

proc normalizeRadians*(radians: float64): float64 =
  ## Normalize radians to [0, 2*PI).
  provenAngleNormalizeRadians(radians)
