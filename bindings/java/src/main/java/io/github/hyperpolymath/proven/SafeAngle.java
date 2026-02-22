// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

/**
 * Safe angle conversion and normalisation via libproven FFI.
 *
 * All logic delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No angle logic is reimplemented in Java.
 */
public final class SafeAngle {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private SafeAngle() {}

    /**
     * Convert degrees to radians.
     *
     * @param degrees the angle in degrees
     * @return the angle in radians
     */
    public static double degToRad(double degrees) {
        return LIB.proven_angle_deg_to_rad(degrees);
    }

    /**
     * Convert radians to degrees.
     *
     * @param radians the angle in radians
     * @return the angle in degrees
     */
    public static double radToDeg(double radians) {
        return LIB.proven_angle_rad_to_deg(radians);
    }

    /**
     * Normalize an angle in degrees to [0, 360).
     *
     * @param degrees the angle in degrees
     * @return the normalized angle
     */
    public static double normalizeDegrees(double degrees) {
        return LIB.proven_angle_normalize_degrees(degrees);
    }

    /**
     * Normalize an angle in radians to [0, 2*pi).
     *
     * @param radians the angle in radians
     * @return the normalized angle
     */
    public static double normalizeRadians(double radians) {
        return LIB.proven_angle_normalize_radians(radians);
    }
}
