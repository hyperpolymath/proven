// SPDX-License-Identifier: PMPL-1.0
/**
 * Safe JSON operations.
 */

import { err, ok, type Result } from './result.ts';

/**
 * Safe JSON operations.
 */
export class SafeJson {
  /**
   * Parse JSON safely.
   *
   * @example
   * ```ts
   * const result = SafeJson.parse('{"key": "value"}');
   * if (result.ok) {
   *   console.log(result.value); // { key: "value" }
   * }
   * ```
   */
  static parse(s: string): Result<unknown> {
    try {
      return ok(JSON.parse(s));
    } catch (e) {
      return err(e instanceof Error ? e.message : 'Parse error');
    }
  }

  /** Stringify to JSON safely. */
  static stringify(value: unknown, indent?: number): Result<string> {
    try {
      return ok(JSON.stringify(value, null, indent));
    } catch (e) {
      return err(e instanceof Error ? e.message : 'Stringify error');
    }
  }

  /** Check if a string is valid JSON. */
  static isValid(s: string): boolean {
    try {
      JSON.parse(s);
      return true;
    } catch {
      return false;
    }
  }

  /** Safely access nested property using dot notation. */
  static getPath(obj: unknown, path: string): unknown | undefined {
    const parts = path.split('.');
    let current: unknown = obj;

    for (const part of parts) {
      if (current === null || current === undefined) {
        return undefined;
      }
      if (typeof current !== 'object') {
        return undefined;
      }
      current = (current as Record<string, unknown>)[part];
    }

    return current;
  }
}
