// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeAngle - JNA wrapper for proven_angle_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import groovy.transform.CompileStatic

/**
 * Safe angle conversion and normalization via libproven JNA FFI.
 *
 * Every method delegates to the Idris 2 verified implementation.
 * No angle logic is reimplemented in Groovy.
 */
@CompileStatic
class SafeAngle {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /** Convert degrees to radians. */
    static double degToRad(double degrees) {
        LIB.proven_angle_deg_to_rad(degrees)
    }

    /** Convert radians to degrees. */
    static double radToDeg(double radians) {
        LIB.proven_angle_rad_to_deg(radians)
    }

    /** Normalize an angle in degrees to [0, 360). */
    static double normalizeDegrees(double degrees) {
        LIB.proven_angle_normalize_degrees(degrees)
    }

    /** Normalize an angle in radians to [0, 2*pi). */
    static double normalizeRadians(double radians) {
        LIB.proven_angle_normalize_radians(radians)
    }
}
