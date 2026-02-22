// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeJson - Typed wrapper for JSON operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import { SafeJson as JsSafeJson } from '../../javascript/src/safe_json.js';

/** Result type for JSON operations. */
export type JsonResult<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/**
 * JSON value type.
 */
export type JsonValue =
  | null
  | boolean
  | number
  | string
  | JsonValue[]
  | { [key: string]: JsonValue };

/**
 * Safe JSON operations backed by formally verified Idris 2 code.
 * All methods delegate to the JavaScript FFI wrapper.
 */
export const SafeJson = {
  /**
   * Check if a JSON string is valid.
   * Delegates to proven_json_is_valid via FFI.
   *
   * @param input - JSON string to validate.
   * @returns Result with boolean validity flag, or error.
   */
  isValid(input: string): JsonResult<boolean> {
    return JsSafeJson.isValid(input) as JsonResult<boolean>;
  },

  /**
   * Get the JSON value type.
   * Delegates to proven_json_get_type via FFI.
   *
   * @param input - JSON string.
   * @returns The JSON type identifier.
   */
  getType(input: string): number {
    return JsSafeJson.getType(input);
  },
} as const;
