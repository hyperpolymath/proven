// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe string operations via libproven FFI.
///
/// All escaping and validation is performed in the formally verified
/// Idris 2 core. This module marshals Dart strings to C byte arrays
/// and converts results back.
library;

import 'dart:ffi';

import 'package:ffi/ffi.dart';

import 'ffi.dart';

/// Safe string operations with escaping and sanitization.
///
/// Delegates to libproven for XSS prevention, SQL injection prevention,
/// and other string safety operations.
class SafeString {
  /// Escape string for safe HTML output (XSS prevention).
  /// Returns null on error.
  static String? escapeHtml(String input) {
    final (ptr, len) = toNativeUtf8Bytes(input);
    try {
      final result = provenStringEscapeHtml(ptr, len);
      return callStringFfi(result);
    } finally {
      calloc.free(ptr);
    }
  }

  /// Escape string for SQL single quotes.
  /// Returns null on error.
  ///
  /// **Note:** Prefer parameterized queries. This is for edge cases only.
  static String? escapeSql(String input) {
    final (ptr, len) = toNativeUtf8Bytes(input);
    try {
      final result = provenStringEscapeSql(ptr, len);
      return callStringFfi(result);
    } finally {
      calloc.free(ptr);
    }
  }

  /// Escape string for JavaScript string context.
  /// Returns null on error.
  static String? escapeJs(String input) {
    final (ptr, len) = toNativeUtf8Bytes(input);
    try {
      final result = provenStringEscapeJs(ptr, len);
      return callStringFfi(result);
    } finally {
      calloc.free(ptr);
    }
  }

  /// Check if byte sequence is valid UTF-8.
  static bool? isValidUtf8(String input) {
    final (ptr, len) = toNativeUtf8Bytes(input);
    try {
      final result = provenStringIsValidUtf8(ptr, len);
      if (result.status != ProvenStatus.ok) return null;
      return result.value;
    } finally {
      calloc.free(ptr);
    }
  }
}
