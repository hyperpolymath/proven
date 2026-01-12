// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe string operations for Dart with XSS and injection prevention.
library;

import 'dart:convert';

/// Safe string operations with escaping and sanitization.
class SafeString {
  /// HTML entity mappings for escaping.
  static const Map<String, String> _htmlEntities = {
    '&': '&amp;',
    '<': '&lt;',
    '>': '&gt;',
    '"': '&quot;',
    "'": '&#x27;',
  };

  /// Escape string for safe HTML output (XSS prevention).
  static String escapeHtml(String input) {
    final buffer = StringBuffer();
    for (final char in input.runes) {
      final str = String.fromCharCode(char);
      buffer.write(_htmlEntities[str] ?? str);
    }
    return buffer.toString();
  }

  /// Escape string for SQL single quotes.
  ///
  /// **Note:** Prefer parameterized queries! This is for edge cases only.
  static String escapeSql(String input) {
    return input.replaceAll("'", "''");
  }

  /// Escape string for JavaScript string context.
  static String escapeJs(String input) {
    final buffer = StringBuffer();
    for (final char in input.runes) {
      switch (char) {
        case 0x5C: // backslash
          buffer.write(r'\\');
        case 0x27: // single quote
          buffer.write(r"\'");
        case 0x22: // double quote
          buffer.write(r'\"');
        case 0x0A: // newline
          buffer.write(r'\n');
        case 0x0D: // carriage return
          buffer.write(r'\r');
        case 0x09: // tab
          buffer.write(r'\t');
        case 0x3C: // < (prevent script injection)
          buffer.write(r'\x3C');
        case 0x3E: // > (prevent script injection)
          buffer.write(r'\x3E');
        default:
          buffer.writeCharCode(char);
      }
    }
    return buffer.toString();
  }

  /// Escape string for shell command (single-quote wrapping).
  static String escapeShell(String input) {
    // In single quotes, only ' needs escaping (as '\'' - end quote, escaped quote, start quote)
    return "'${input.replaceAll("'", "'\\''")}'";
  }

  /// Escape special characters for use in regex patterns.
  static String escapeRegex(String input) {
    return input.replaceAllMapped(
      RegExp(r'[.*+?^${}()|\[\]\\]'),
      (match) => '\\${match.group(0)}',
    );
  }

  /// URL-encode a string.
  static String urlEncode(String input) {
    return Uri.encodeComponent(input);
  }

  /// URL-decode a string.
  static String urlDecode(String input) {
    return Uri.decodeComponent(input);
  }

  /// Base64-encode a string.
  static String base64Encode(String input) {
    return base64.encode(utf8.encode(input));
  }

  /// Base64-decode a string.
  static String? base64Decode(String input) {
    try {
      return utf8.decode(base64.decode(input));
    } catch (_) {
      return null;
    }
  }

  /// Safely truncate a string to a maximum length.
  static String truncate(String input, int maxLength, {String suffix = '...'}) {
    if (maxLength <= 0) return '';
    if (input.length <= maxLength) return input;

    final suffixLength = suffix.length;
    if (maxLength <= suffixLength) {
      return input.substring(0, maxLength);
    }

    return '${input.substring(0, maxLength - suffixLength)}$suffix';
  }

  /// Strip HTML tags from string (basic - not a full parser).
  static String stripHtml(String input) {
    return input.replaceAll(RegExp(r'<[^>]*>'), '');
  }

  /// Check if string contains only alphanumeric characters.
  static bool isAlphanumeric(String input) {
    if (input.isEmpty) return false;
    return RegExp(r'^[a-zA-Z0-9]+$').hasMatch(input);
  }

  /// Check if string contains only ASCII characters.
  static bool isAscii(String input) {
    for (final unit in input.codeUnits) {
      if (unit > 127) return false;
    }
    return true;
  }

  /// Check if string is valid UTF-8.
  static bool isValidUtf8(String input) {
    try {
      utf8.encode(input);
      return true;
    } catch (_) {
      return false;
    }
  }

  /// Sanitize string to contain only specified allowed characters.
  static String sanitize(String input, {String allowed = r'a-zA-Z0-9_\-'}) {
    final pattern = RegExp('[^$allowed]');
    return input.replaceAll(pattern, '');
  }

  /// Remove control characters from string.
  static String removeControlChars(String input) {
    // Remove ASCII control characters (0-31) except common whitespace
    return input.replaceAll(RegExp(r'[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]'), '');
  }

  /// Normalize whitespace (collapse multiple spaces, trim).
  static String normalizeWhitespace(String input) {
    return input.replaceAll(RegExp(r'\s+'), ' ').trim();
  }

  /// Check if string looks like it contains injection attempts.
  static bool containsSuspiciousPatterns(String input) {
    final patterns = [
      RegExp(r'<script', caseSensitive: false),
      RegExp(r'javascript:', caseSensitive: false),
      RegExp(r'on\w+\s*=', caseSensitive: false),
      RegExp(r'union\s+select', caseSensitive: false),
      RegExp(r';\s*drop\s+table', caseSensitive: false),
    ];

    for (final pattern in patterns) {
      if (pattern.hasMatch(input)) return true;
    }
    return false;
  }
}
