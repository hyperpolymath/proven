// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeJson - JSON validation without exceptions.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.ts';
import { err, ok, type Result } from './result.ts';

/**
 * JSON value type enumeration.
 */
export const JsonType = {
  NULL: 0,
  BOOL: 1,
  NUMBER: 2,
  STRING: 3,
  ARRAY: 4,
  OBJECT: 5,
  INVALID: -1,
} as const;

/**
 * Safe JSON operations backed by formally verified Idris 2 code.
 */
export class SafeJson {
  /**
   * Check if a string is valid JSON.
   *
   * @param json - The JSON string to validate.
   * @returns Result containing a boolean or an error string.
   */
  static isValid(json: string): Result<boolean> {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(json);
    const result = symbols.proven_json_is_valid(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Get the root-level JSON value type.
   *
   * @param json - The JSON string to inspect.
   * @returns A JsonType enum value.
   */
  static getType(json: string): number {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(json);
    return symbols.proven_json_get_type(bytes, bytes.length);
  }
}

/**
 * A validated JSON value.
 */
export class JsonValue {
  #raw: string;

  constructor(raw: string) {
    this.#raw = raw;
  }

  /**
   * Parse and validate a JSON string.
   *
   * @param json - The JSON to parse.
   * @returns Result containing a JsonValue or an error string.
   */
  static parse(json: string): Result<JsonValue> {
    const valid = SafeJson.isValid(json);
    if (!valid.ok) return valid;
    if (!valid.value) return err('Invalid JSON');
    return ok(new JsonValue(json));
  }

  /** The raw JSON string. */
  toString(): string {
    return this.#raw;
  }

  /** The root-level JSON type. */
  getType(): number {
    return SafeJson.getType(this.#raw);
  }
}
