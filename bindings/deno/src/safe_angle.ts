// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeAngle - Angle conversions and normalization.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib } from './ffi.ts';

/**
 * Safe angle operations backed by formally verified Idris 2 code.
 */
export class SafeAngle {
  /**
   * Convert degrees to radians.
   *
   * @param degrees - Angle in degrees.
   * @returns Angle in radians.
   */
  static degToRad(degrees: number): number {
    const symbols = getLib();
    return symbols.proven_angle_deg_to_rad(degrees);
  }

  /**
   * Convert radians to degrees.
   *
   * @param radians - Angle in radians.
   * @returns Angle in degrees.
   */
  static radToDeg(radians: number): number {
    const symbols = getLib();
    return symbols.proven_angle_rad_to_deg(radians);
  }

  /**
   * Normalize angle to 0-360 degrees.
   *
   * @param degrees - Angle in degrees.
   * @returns Normalized angle.
   */
  static normalizeDegrees(degrees: number): number {
    const symbols = getLib();
    return symbols.proven_angle_normalize_degrees(degrees);
  }

  /**
   * Normalize angle to 0-2pi radians.
   *
   * @param radians - Angle in radians.
   * @returns Normalized angle.
   */
  static normalizeRadians(radians: number): number {
    const symbols = getLib();
    return symbols.proven_angle_normalize_radians(radians);
  }
}
