// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeString - Safe string operations
 *
 * Provides length-bounded string operations, validation,
 * sanitization, and safe manipulation.
 */

import { Result, Option, ErrorCode, isAlphanumeric, isDigit, isLetter, isWhitespace } from "./common";

/**
 * SafeString provides bounded and validated string operations.
 */
export class SafeString {
  private _value: string;
  private _maxLength: i32;

  constructor(value: string = "", maxLength: i32 = 65536) {
    this._maxLength = maxLength;
    this._value = value.length > maxLength ? value.substring(0, maxLength) : value;
  }

  /**
   * Get the string value.
   */
  get value(): string {
    return this._value;
  }

  /**
   * Get the string length.
   */
  get length(): i32 {
    return this._value.length;
  }

  /**
   * Check if string is empty.
   */
  get isEmpty(): bool {
    return this._value.length == 0;
  }

  /**
   * Truncate string to max length.
   */
  static truncate(s: string, maxLength: i32): string {
    if (s.length <= maxLength) return s;
    return s.substring(0, maxLength);
  }

  /**
   * Truncate with ellipsis.
   */
  static truncateWithEllipsis(s: string, maxLength: i32, ellipsis: string = "..."): string {
    if (s.length <= maxLength) return s;
    if (maxLength <= ellipsis.length) return ellipsis.substring(0, maxLength);
    return s.substring(0, maxLength - ellipsis.length) + ellipsis;
  }

  /**
   * Check if string contains only alphanumeric characters.
   */
  static isAlphanumeric(s: string): bool {
    if (s.length == 0) return false;
    for (let i = 0; i < s.length; i++) {
      if (!isAlphanumeric(s.charCodeAt(i))) return false;
    }
    return true;
  }

  /**
   * Check if string contains only digits.
   */
  static isNumeric(s: string): bool {
    if (s.length == 0) return false;
    for (let i = 0; i < s.length; i++) {
      if (!isDigit(s.charCodeAt(i))) return false;
    }
    return true;
  }

  /**
   * Check if string contains only letters.
   */
  static isAlpha(s: string): bool {
    if (s.length == 0) return false;
    for (let i = 0; i < s.length; i++) {
      if (!isLetter(s.charCodeAt(i))) return false;
    }
    return true;
  }

  /**
   * Check if string is ASCII only.
   */
  static isAscii(s: string): bool {
    for (let i = 0; i < s.length; i++) {
      if (s.charCodeAt(i) > 127) return false;
    }
    return true;
  }

  /**
   * Check if string is printable ASCII only.
   */
  static isPrintableAscii(s: string): bool {
    for (let i = 0; i < s.length; i++) {
      const code = s.charCodeAt(i);
      if (code < 32 || code > 126) return false;
    }
    return true;
  }

  /**
   * Trim whitespace from both ends.
   */
  static trim(s: string): string {
    let start = 0;
    let end = s.length;

    while (start < end && isWhitespace(s.charCodeAt(start))) {
      start++;
    }
    while (end > start && isWhitespace(s.charCodeAt(end - 1))) {
      end--;
    }

    return s.substring(start, end);
  }

  /**
   * Trim whitespace from start.
   */
  static trimStart(s: string): string {
    let start = 0;
    while (start < s.length && isWhitespace(s.charCodeAt(start))) {
      start++;
    }
    return s.substring(start);
  }

  /**
   * Trim whitespace from end.
   */
  static trimEnd(s: string): string {
    let end = s.length;
    while (end > 0 && isWhitespace(s.charCodeAt(end - 1))) {
      end--;
    }
    return s.substring(0, end);
  }

  /**
   * Normalize whitespace (collapse multiple spaces into one).
   */
  static normalizeWhitespace(s: string): string {
    let result = "";
    let lastWasSpace = true;  // Start true to trim leading

    for (let i = 0; i < s.length; i++) {
      const code = s.charCodeAt(i);
      if (isWhitespace(code)) {
        if (!lastWasSpace) {
          result += " ";
          lastWasSpace = true;
        }
      } else {
        result += String.fromCharCode(code);
        lastWasSpace = false;
      }
    }

    // Trim trailing space
    if (result.length > 0 && result.charCodeAt(result.length - 1) == 32) {
      result = result.substring(0, result.length - 1);
    }

    return result;
  }

  /**
   * Sanitize string by removing control characters.
   */
  static sanitize(s: string): string {
    let result = "";
    for (let i = 0; i < s.length; i++) {
      const code = s.charCodeAt(i);
      // Keep printable ASCII and common Unicode
      if (code >= 32 && code != 127) {
        result += String.fromCharCode(code);
      }
    }
    return result;
  }

  /**
   * Remove non-alphanumeric characters.
   */
  static removeNonAlphanumeric(s: string): string {
    let result = "";
    for (let i = 0; i < s.length; i++) {
      const code = s.charCodeAt(i);
      if (isAlphanumeric(code)) {
        result += String.fromCharCode(code);
      }
    }
    return result;
  }

  /**
   * Pad string to length on left.
   */
  static padStart(s: string, targetLength: i32, padChar: string = " "): string {
    if (s.length >= targetLength) return s;
    const padLen = targetLength - s.length;
    let padding = "";
    for (let i = 0; i < padLen; i++) {
      padding += padChar;
    }
    return padding + s;
  }

  /**
   * Pad string to length on right.
   */
  static padEnd(s: string, targetLength: i32, padChar: string = " "): string {
    if (s.length >= targetLength) return s;
    const padLen = targetLength - s.length;
    let padding = "";
    for (let i = 0; i < padLen; i++) {
      padding += padChar;
    }
    return s + padding;
  }

  /**
   * Safe substring with bounds checking.
   */
  static safeSubstring(s: string, start: i32, end: i32 = -1): string {
    const len = s.length;
    const safeStart = start < 0 ? 0 : (start > len ? len : start);
    const safeEnd = end < 0 ? len : (end > len ? len : end);
    if (safeStart >= safeEnd) return "";
    return s.substring(safeStart, safeEnd);
  }

  /**
   * Count occurrences of substring.
   */
  static countOccurrences(s: string, needle: string): i32 {
    if (needle.length == 0) return 0;
    let count = 0;
    let pos = 0;
    while (true) {
      const idx = s.indexOf(needle, pos);
      if (idx < 0) break;
      count++;
      pos = idx + needle.length;
    }
    return count;
  }

  /**
   * Replace all occurrences of a substring.
   */
  static replaceAll(s: string, search: string, replacement: string): string {
    if (search.length == 0) return s;
    return s.split(search).join(replacement);
  }

  /**
   * Escape HTML special characters.
   */
  static escapeHtml(s: string): string {
    let result = "";
    for (let i = 0; i < s.length; i++) {
      const code = s.charCodeAt(i);
      if (code == 38) result += "&amp;";        // &
      else if (code == 60) result += "&lt;";     // <
      else if (code == 62) result += "&gt;";     // >
      else if (code == 34) result += "&quot;";   // "
      else if (code == 39) result += "&#39;";    // '
      else result += String.fromCharCode(code);
    }
    return result;
  }

  /**
   * Check if string matches a simple pattern (with * wildcard).
   */
  static matchesPattern(s: string, pattern: string): bool {
    // Simple glob matching with * wildcard
    const parts = pattern.split("*");
    if (parts.length == 1) return s == pattern;

    let pos = 0;
    for (let i = 0; i < parts.length; i++) {
      const part = parts[i];
      if (part.length == 0) continue;

      if (i == 0) {
        // Must start with first part
        if (!s.startsWith(part)) return false;
        pos = part.length;
      } else if (i == parts.length - 1) {
        // Must end with last part
        if (!s.endsWith(part)) return false;
      } else {
        // Must contain middle part
        const idx = s.indexOf(part, pos);
        if (idx < 0) return false;
        pos = idx + part.length;
      }
    }
    return true;
  }

  // Instance methods
  append(s: string): SafeString {
    const combined = this._value + s;
    return new SafeString(combined, this._maxLength);
  }

  truncateTo(length: i32): SafeString {
    return new SafeString(SafeString.truncate(this._value, length), this._maxLength);
  }

  sanitized(): SafeString {
    return new SafeString(SafeString.sanitize(this._value), this._maxLength);
  }

  trimmed(): SafeString {
    return new SafeString(SafeString.trim(this._value), this._maxLength);
  }
}
