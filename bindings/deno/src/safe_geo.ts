// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeGeo - Geographic coordinate operations.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.ts';
import { err, ok, type Result } from './result.ts';

/** Geographic coordinate. */
export interface Coordinate {
  readonly latitude: number;
  readonly longitude: number;
}

/**
 * Safe geographic operations backed by formally verified Idris 2 code.
 */
export class SafeGeo {
  /**
   * Validate and normalize geographic coordinates.
   *
   * @param lat - Latitude.
   * @param lon - Longitude.
   * @returns Result containing a Coordinate or an error string.
   */
  static validate(lat: number, lon: number): Result<Coordinate> {
    const symbols = getLib();
    const result = symbols.proven_geo_validate(lat, lon);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok({ latitude: result[1], longitude: result[2] });
  }
}
