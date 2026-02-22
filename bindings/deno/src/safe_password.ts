// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafePassword - Password validation that cannot crash.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * Note: proven_password_validate returns a PasswordResult struct which
 * requires buffer-based marshaling. proven_password_is_common is directly
 * callable.
 *
 * @module
 */

import { getLib } from './ffi.ts';

/**
 * Password strength levels.
 */
export const PasswordStrength = {
  VERY_WEAK: 0,
  WEAK: 1,
  FAIR: 2,
  STRONG: 3,
  VERY_STRONG: 4,
} as const;

/**
 * Safe password operations backed by formally verified Idris 2 code.
 */
export class SafePassword {
  /**
   * Check if a password is in the common passwords list.
   *
   * @param password - The password to check.
   * @returns True if the password is common.
   */
  static isCommon(password: string): boolean {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(password);
    return symbols.proven_password_is_common(bytes, bytes.length);
  }
}
