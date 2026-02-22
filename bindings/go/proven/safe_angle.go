// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeAngle provides angle conversion and normalization via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
import "C"

// AngleDegToRad converts degrees to radians.
func AngleDegToRad(degrees float64) float64 {
	return float64(C.proven_angle_deg_to_rad(C.double(degrees)))
}

// AngleRadToDeg converts radians to degrees.
func AngleRadToDeg(radians float64) float64 {
	return float64(C.proven_angle_rad_to_deg(C.double(radians)))
}

// AngleNormalizeDegrees normalizes an angle to the range [0, 360) degrees.
func AngleNormalizeDegrees(degrees float64) float64 {
	return float64(C.proven_angle_normalize_degrees(C.double(degrees)))
}

// AngleNormalizeRadians normalizes an angle to the range [0, 2*pi) radians.
func AngleNormalizeRadians(radians float64) float64 {
	return float64(C.proven_angle_normalize_radians(C.double(radians)))
}
