// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeJson - JSON validation without exceptions.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * JSON value type enumeration.
 * @readonly
 * @enum {number}
 */
export const JsonType = Object.freeze({
  NULL: 0,
  BOOL: 1,
  NUMBER: 2,
  STRING: 3,
  ARRAY: 4,
  OBJECT: 5,
  INVALID: -1,
});

/**
 * Safe JSON operations backed by formally verified Idris 2 code.
 */
export class SafeJson {
  /**
   * Check if a string is valid JSON.
   *
   * @param {string} json - The JSON string to validate.
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static isValid(json) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(json);
    const result = symbols.proven_json_is_valid(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Get the root-level JSON value type.
   *
   * @param {string} json - The JSON string to inspect.
   * @returns {number} A JsonType enum value.
   */
  static getType(json) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(json);
    return symbols.proven_json_get_type(bytes, bytes.length);
  }
}

/**
 * A validated JSON value.
 */
export class JsonValue {
  /** @type {string} */
  #raw;

  /**
   * @param {string} raw - The validated JSON string.
   */
  constructor(raw) {
    this.#raw = raw;
  }

  /**
   * Parse and validate a JSON string.
   *
   * @param {string} json - The JSON to parse.
   * @returns {{ ok: true, value: JsonValue } | { ok: false, error: string }}
   */
  static parse(json) {
    const valid = SafeJson.isValid(json);
    if (!valid.ok) return valid;
    if (!valid.value) return err('Invalid JSON');
    return ok(new JsonValue(json));
  }

  /** @returns {string} The raw JSON string. */
  toString() { return this.#raw; }

  /** @returns {number} The root-level JSON type. */
  getType() { return SafeJson.getType(this.#raw); }
}
