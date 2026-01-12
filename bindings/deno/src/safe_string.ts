// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * Safe string operations with injection prevention.
 */

/**
 * Safe string operations with injection prevention.
 */
export class SafeString {
  /**
   * Escape HTML special characters.
   *
   * @example
   * ```ts
   * const escaped = SafeString.escapeHtml("<script>alert('xss')</script>");
   * // "&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;"
   * ```
   */
  static escapeHtml(s: string): string {
    return s
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#x27;');
  }

  /** Escape SQL single quotes. */
  static escapeSql(s: string): string {
    return s.replace(/'/g, "''");
  }

  /** Escape JavaScript special characters. */
  static escapeJs(s: string): string {
    return s
      .replace(/\\/g, '\\\\')
      .replace(/'/g, "\\'")
      .replace(/"/g, '\\"')
      .replace(/\n/g, '\\n')
      .replace(/\r/g, '\\r')
      .replace(/\t/g, '\\t');
  }

  /** URL encode a string. */
  static urlEncode(s: string): string {
    return encodeURIComponent(s);
  }

  /** URL decode a string. */
  static urlDecode(s: string): string {
    return decodeURIComponent(s);
  }

  /** Escape shell special characters (wraps in single quotes). */
  static escapeShell(s: string): string {
    return "'" + s.replace(/'/g, "'\\''") + "'";
  }

  /** Check if a string contains potential injection patterns. */
  static detectInjection(s: string): boolean {
    const patterns = [
      '<script',
      'javascript:',
      'onerror=',
      'onclick=',
      'onload=',
      'eval(',
      'expression(',
      'vbscript:',
      'data:',
      'SELECT ',
      'INSERT ',
      'UPDATE ',
      'DELETE ',
      'DROP ',
      'UNION ',
      '--',
      '/*',
      '*/',
      '; ',
      "' OR ",
      '" OR ',
    ];

    const lower = s.toLowerCase();
    return patterns.some((p) => lower.includes(p.toLowerCase()));
  }

  /** Truncate a string to a maximum length, respecting word boundaries. */
  static truncate(s: string, maxLength: number, suffix = '...'): string {
    if (s.length <= maxLength) return s;
    const truncated = s.slice(0, maxLength - suffix.length);
    const lastSpace = truncated.lastIndexOf(' ');
    if (lastSpace > 0) {
      return truncated.slice(0, lastSpace) + suffix;
    }
    return truncated + suffix;
  }
}
