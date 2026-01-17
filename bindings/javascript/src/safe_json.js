// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeJson - JSON parsing that cannot crash.
 *
 * Provides safe JSON parsing and manipulation with type-safe accessors.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Wrapper for JSON values with safe accessors.
 */
export class JsonValue {
  /** @type {any} */
  #value;

  /**
   * Create a JsonValue wrapper.
   *
   * @param {any} value - The underlying value
   */
  constructor(value) {
    this.#value = value;
  }

  /**
   * Get the raw value.
   *
   * @returns {any}
   */
  get value() {
    return this.#value;
  }

  /**
   * Check if value is null.
   *
   * @returns {boolean}
   */
  isNull() {
    return this.#value === null;
  }

  /**
   * Check if value is a boolean.
   *
   * @returns {boolean}
   */
  isBoolean() {
    return typeof this.#value === 'boolean';
  }

  /**
   * Check if value is a number.
   *
   * @returns {boolean}
   */
  isNumber() {
    return typeof this.#value === 'number';
  }

  /**
   * Check if value is a string.
   *
   * @returns {boolean}
   */
  isString() {
    return typeof this.#value === 'string';
  }

  /**
   * Check if value is an array.
   *
   * @returns {boolean}
   */
  isArray() {
    return Array.isArray(this.#value);
  }

  /**
   * Check if value is an object.
   *
   * @returns {boolean}
   */
  isObject() {
    return this.#value !== null && typeof this.#value === 'object' && !Array.isArray(this.#value);
  }

  /**
   * Get as boolean.
   *
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  asBoolean() {
    if (typeof this.#value === 'boolean') {
      return ok(this.#value);
    }
    return err('Value is not a boolean');
  }

  /**
   * Get as number.
   *
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  asNumber() {
    if (typeof this.#value === 'number') {
      return ok(this.#value);
    }
    return err('Value is not a number');
  }

  /**
   * Get as string.
   *
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  asString() {
    if (typeof this.#value === 'string') {
      return ok(this.#value);
    }
    return err('Value is not a string');
  }

  /**
   * Get as array.
   *
   * @returns {{ ok: true, value: JsonValue[] } | { ok: false, error: string }}
   */
  asArray() {
    if (Array.isArray(this.#value)) {
      return ok(this.#value.map((item) => new JsonValue(item)));
    }
    return err('Value is not an array');
  }

  /**
   * Get object property.
   *
   * @param {string} key - Property key
   * @returns {{ ok: true, value: JsonValue } | { ok: false, error: string }}
   */
  get(key) {
    if (!this.isObject()) {
      return err('Value is not an object');
    }
    if (!(key in this.#value)) {
      return err(`Key '${key}' not found`);
    }
    return ok(new JsonValue(this.#value[key]));
  }

  /**
   * Get array element by index.
   *
   * @param {number} index - Array index
   * @returns {{ ok: true, value: JsonValue } | { ok: false, error: string }}
   */
  at(index) {
    if (!Array.isArray(this.#value)) {
      return err('Value is not an array');
    }
    if (index < 0 || index >= this.#value.length) {
      return err('Index out of bounds');
    }
    return ok(new JsonValue(this.#value[index]));
  }

  /**
   * Get nested value by path.
   *
   * @param {string} path - Dot-separated path (e.g., "user.name")
   * @returns {{ ok: true, value: JsonValue } | { ok: false, error: string }}
   */
  getPath(path) {
    const parts = path.split('.');
    /** @type {JsonValue} */
    let current = this;

    for (const part of parts) {
      // Check if it's an array index
      const arrayMatch = part.match(/^(\w+)\[(\d+)\]$/);
      if (arrayMatch) {
        const [, key, indexStr] = arrayMatch;
        const getResult = current.get(key);
        if (!getResult.ok) {
          return getResult;
        }
        const atResult = getResult.value.at(parseInt(indexStr, 10));
        if (!atResult.ok) {
          return atResult;
        }
        current = atResult.value;
      } else {
        const getResult = current.get(part);
        if (!getResult.ok) {
          return getResult;
        }
        current = getResult.value;
      }
    }

    return ok(current);
  }

  /**
   * Get object keys.
   *
   * @returns {string[]}
   */
  keys() {
    if (this.isObject()) {
      return Object.keys(this.#value);
    }
    return [];
  }

  /**
   * Get array length or object key count.
   *
   * @returns {number}
   */
  length() {
    if (Array.isArray(this.#value)) {
      return this.#value.length;
    }
    if (this.isObject()) {
      return Object.keys(this.#value).length;
    }
    return 0;
  }
}

/**
 * Safe JSON operations.
 */
export class SafeJson {
  /**
   * Parse JSON string safely.
   *
   * @param {string} jsonString - JSON string to parse
   * @returns {{ ok: true, value: JsonValue } | { ok: false, error: string }}
   *
   * @example
   * const result = SafeJson.parse('{"name": "test"}');
   * if (result.ok) {
   *   const name = result.value.get('name');
   * }
   */
  static parse(jsonString) {
    try {
      const parsed = JSON.parse(jsonString);
      return ok(new JsonValue(parsed));
    } catch (error) {
      return err(`JSON parse error: ${error.message}`);
    }
  }

  /**
   * Stringify value safely.
   *
   * @param {any} value - Value to stringify
   * @param {number} indent - Indentation spaces
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static stringify(value, indent = 0) {
    try {
      const result = JSON.stringify(value, null, indent);
      if (result === undefined) {
        return err('Value cannot be serialized to JSON');
      }
      return ok(result);
    } catch (error) {
      return err(`JSON stringify error: ${error.message}`);
    }
  }

  /**
   * Deep clone a value via JSON round-trip.
   *
   * @template T
   * @param {T} value - Value to clone
   * @returns {{ ok: true, value: T } | { ok: false, error: string }}
   */
  static clone(value) {
    try {
      return ok(JSON.parse(JSON.stringify(value)));
    } catch (error) {
      return err(`Clone error: ${error.message}`);
    }
  }

  /**
   * Check if string is valid JSON.
   *
   * @param {string} jsonString - String to validate
   * @returns {boolean}
   */
  static isValid(jsonString) {
    return SafeJson.parse(jsonString).ok;
  }

  /**
   * Get value at path, with type coercion.
   *
   * @param {string} jsonString - JSON string
   * @param {string} path - Dot-separated path
   * @returns {{ ok: true, value: any } | { ok: false, error: string }}
   */
  static getPath(jsonString, path) {
    const parseResult = SafeJson.parse(jsonString);
    if (!parseResult.ok) {
      return parseResult;
    }
    const pathResult = parseResult.value.getPath(path);
    if (!pathResult.ok) {
      return pathResult;
    }
    return ok(pathResult.value.value);
  }
}
