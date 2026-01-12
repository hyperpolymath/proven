// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeString - String operations that cannot crash.
 *
 * Provides safe UTF-8 validation and escaping for SQL, HTML, and JavaScript
 * without exceptions or security vulnerabilities.
 */

import { getExports, encodeString, decodeString, freePtr } from './wasm.js';
import { statusFromCode } from './error.js';

/**
 * Safe string operations with proven correctness guarantees.
 */
export class SafeString {
  /**
   * Check if a string is valid UTF-8.
   *
   * Note: JavaScript strings are always valid UTF-16, so this primarily
   * validates when working with binary data converted to strings.
   */
  static isValidUtf8(data: Uint8Array): boolean {
    const exports = getExports();
    const fn = exports['proven_string_is_valid_utf8'] as (ptr: number, len: number) => number;

    // Allocate and copy data
    const allocFn = exports['proven_alloc'] as (size: number) => number;
    const ptr = allocFn(data.length);
    const memory = exports['memory'] as WebAssembly.Memory;
    new Uint8Array(memory.buffer, ptr, data.length).set(data);

    const result = fn(ptr, data.length);
    freePtr(ptr);

    const status = result >> 16;
    const value = result & 0xffff;

    return statusFromCode(status) === 'ok' && value === 1;
  }

  /**
   * Escape a string for safe SQL interpolation.
   *
   * Prevents SQL injection by escaping dangerous characters.
   *
   * @example
   * SafeString.escapeSql("O'Brien")  // "O''Brien"
   */
  static escapeSql(value: string): string | null {
    const exports = getExports();
    const fn = exports['proven_string_escape_sql'] as (ptr: number, len: number) => number;

    const { ptr, len } = encodeString(value);
    const resultPtr = fn(ptr, len);
    freePtr(ptr);

    if (resultPtr === 0) {
      return null;
    }

    // Read result length from first 4 bytes, then string
    const memory = exports['memory'] as WebAssembly.Memory;
    const view = new DataView(memory.buffer);
    const resultLen = view.getUint32(resultPtr, true);

    const escaped = decodeString(resultPtr + 4, resultLen);
    freePtr(resultPtr);

    return escaped;
  }

  /**
   * Escape a string for safe HTML insertion.
   *
   * Prevents XSS by escaping < > & " ' characters.
   *
   * @example
   * SafeString.escapeHtml("<script>alert(1)</script>")
   * // "&lt;script&gt;alert(1)&lt;/script&gt;"
   */
  static escapeHtml(value: string): string | null {
    const exports = getExports();
    const fn = exports['proven_string_escape_html'] as (ptr: number, len: number) => number;

    const { ptr, len } = encodeString(value);
    const resultPtr = fn(ptr, len);
    freePtr(ptr);

    if (resultPtr === 0) {
      return null;
    }

    const memory = exports['memory'] as WebAssembly.Memory;
    const view = new DataView(memory.buffer);
    const resultLen = view.getUint32(resultPtr, true);

    const escaped = decodeString(resultPtr + 4, resultLen);
    freePtr(resultPtr);

    return escaped;
  }

  /**
   * Escape a string for safe JavaScript string literal insertion.
   *
   * Prevents XSS in JavaScript contexts.
   *
   * @example
   * SafeString.escapeJs('alert("hi")')  // 'alert(\\"hi\\")'
   */
  static escapeJs(value: string): string | null {
    const exports = getExports();
    const fn = exports['proven_string_escape_js'] as (ptr: number, len: number) => number;

    const { ptr, len } = encodeString(value);
    const resultPtr = fn(ptr, len);
    freePtr(ptr);

    if (resultPtr === 0) {
      return null;
    }

    const memory = exports['memory'] as WebAssembly.Memory;
    const view = new DataView(memory.buffer);
    const resultLen = view.getUint32(resultPtr, true);

    const escaped = decodeString(resultPtr + 4, resultLen);
    freePtr(resultPtr);

    return escaped;
  }
}
