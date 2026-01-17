// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeString - String operations that cannot crash.
 *
 * Provides safe string manipulation with bounds checking and encoding validation.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Safe string operations.
 */
export class SafeString {
  /**
   * Safe character access by index.
   *
   * @param {string} str - Input string
   * @param {number} index - Character index
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   *
   * @example
   * SafeString.charAt("hello", 1)  // { ok: true, value: "e" }
   */
  static charAt(str, index) {
    if (index < 0 || index >= str.length) {
      return err('Index out of bounds');
    }
    return ok(str.charAt(index));
  }

  /**
   * Safe code point access.
   *
   * @param {string} str - Input string
   * @param {number} index - Character index
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static codePointAt(str, index) {
    if (index < 0 || index >= str.length) {
      return err('Index out of bounds');
    }
    const codePoint = str.codePointAt(index);
    if (codePoint === undefined) {
      return err('Invalid code point');
    }
    return ok(codePoint);
  }

  /**
   * Safe substring extraction.
   *
   * @param {string} str - Input string
   * @param {number} start - Start index
   * @param {number} end - End index (exclusive)
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static substring(str, start, end) {
    if (start < 0 || end < 0 || start > str.length || end > str.length) {
      return err('Index out of bounds');
    }
    if (start > end) {
      return err('Start index greater than end index');
    }
    return ok(str.substring(start, end));
  }

  /**
   * Safe string split with limit.
   *
   * @param {string} str - Input string
   * @param {string} separator - Separator
   * @param {number} limit - Maximum parts
   * @returns {string[]}
   */
  static split(str, separator, limit = Infinity) {
    return str.split(separator).slice(0, limit);
  }

  /**
   * Truncate string to length with optional ellipsis.
   *
   * @param {string} str - Input string
   * @param {number} maxLength - Maximum length
   * @param {string} suffix - Suffix to add if truncated
   * @returns {string}
   */
  static truncate(str, maxLength, suffix = '...') {
    if (str.length <= maxLength) {
      return str;
    }
    const truncLength = maxLength - suffix.length;
    if (truncLength <= 0) {
      return suffix.slice(0, maxLength);
    }
    return str.slice(0, truncLength) + suffix;
  }

  /**
   * HTML escape special characters.
   *
   * @param {string} str - Input string
   * @returns {string}
   */
  static escapeHtml(str) {
    const htmlEscapes = {
      '&': '&amp;',
      '<': '&lt;',
      '>': '&gt;',
      '"': '&quot;',
      "'": '&#x27;',
    };
    return str.replace(/[&<>"']/g, (char) => htmlEscapes[char]);
  }

  /**
   * Unescape HTML entities.
   *
   * @param {string} str - Input string
   * @returns {string}
   */
  static unescapeHtml(str) {
    const htmlUnescapes = {
      '&amp;': '&',
      '&lt;': '<',
      '&gt;': '>',
      '&quot;': '"',
      '&#x27;': "'",
      '&#39;': "'",
    };
    return str.replace(/&(?:amp|lt|gt|quot|#x27|#39);/g, (entity) => htmlUnescapes[entity] || entity);
  }

  /**
   * Check if string is valid UTF-8.
   *
   * @param {string} str - Input string
   * @returns {boolean}
   */
  static isValidUtf8(str) {
    try {
      encodeURIComponent(str);
      return true;
    } catch {
      return false;
    }
  }

  /**
   * Remove control characters.
   *
   * @param {string} str - Input string
   * @returns {string}
   */
  static removeControlChars(str) {
    // eslint-disable-next-line no-control-regex
    return str.replace(/[\x00-\x1F\x7F]/g, '');
  }

  /**
   * Normalize whitespace (collapse multiple spaces).
   *
   * @param {string} str - Input string
   * @returns {string}
   */
  static normalizeWhitespace(str) {
    return str.replace(/\s+/g, ' ').trim();
  }

  /**
   * Pad start of string safely.
   *
   * @param {string} str - Input string
   * @param {number} targetLength - Target length
   * @param {string} padString - Padding string
   * @returns {string}
   */
  static padStart(str, targetLength, padString = ' ') {
    if (padString.length === 0) {
      return str;
    }
    return str.padStart(targetLength, padString);
  }

  /**
   * Pad end of string safely.
   *
   * @param {string} str - Input string
   * @param {number} targetLength - Target length
   * @param {string} padString - Padding string
   * @returns {string}
   */
  static padEnd(str, targetLength, padString = ' ') {
    if (padString.length === 0) {
      return str;
    }
    return str.padEnd(targetLength, padString);
  }

  /**
   * Count occurrences of substring.
   *
   * @param {string} str - Input string
   * @param {string} searchString - String to count
   * @returns {number}
   */
  static countOccurrences(str, searchString) {
    if (searchString.length === 0) {
      return 0;
    }
    let count = 0;
    let position = 0;
    while ((position = str.indexOf(searchString, position)) !== -1) {
      count++;
      position += searchString.length;
    }
    return count;
  }

  /**
   * Reverse string safely (handles surrogate pairs).
   *
   * @param {string} str - Input string
   * @returns {string}
   */
  static reverse(str) {
    return [...str].reverse().join('');
  }

  /**
   * Check if string is empty or whitespace only.
   *
   * @param {string} str - Input string
   * @returns {boolean}
   */
  static isBlank(str) {
    return str.trim().length === 0;
  }

  /**
   * Convert to camelCase.
   *
   * @param {string} str - Input string
   * @returns {string}
   */
  static toCamelCase(str) {
    return str
      .replace(/[-_\s]+(.)?/g, (_, char) => (char ? char.toUpperCase() : ''))
      .replace(/^[A-Z]/, (char) => char.toLowerCase());
  }

  /**
   * Convert to snake_case.
   *
   * @param {string} str - Input string
   * @returns {string}
   */
  static toSnakeCase(str) {
    return str
      .replace(/([A-Z])/g, '_$1')
      .replace(/[-\s]+/g, '_')
      .replace(/^_/, '')
      .toLowerCase();
  }

  /**
   * Convert to kebab-case.
   *
   * @param {string} str - Input string
   * @returns {string}
   */
  static toKebabCase(str) {
    return str
      .replace(/([A-Z])/g, '-$1')
      .replace(/[_\s]+/g, '-')
      .replace(/^-/, '')
      .toLowerCase();
  }
}
