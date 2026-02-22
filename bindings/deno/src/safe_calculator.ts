// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeCalculator - Mathematical expression evaluation.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.ts';
import { err, ok, type Result } from './result.ts';

/**
 * Safe expression calculator backed by formally verified Idris 2 code.
 */
export class SafeCalculator {
  /**
   * Evaluate a mathematical expression string.
   *
   * @param expression - The expression to evaluate (e.g. "2 + 3 * 4").
   * @returns Result containing the evaluated number or an error string.
   */
  static eval(expression: string): Result<number> {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(expression);
    const result = symbols.proven_calculator_eval(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }
}
