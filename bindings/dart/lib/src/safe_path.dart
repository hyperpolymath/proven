// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe filesystem path operations via libproven FFI.
///
/// All computation is performed in the formally verified Idris 2 core.
/// Provides directory traversal detection and filename sanitization.
library;

import 'dart:ffi';

import 'package:ffi/ffi.dart';

import 'ffi.dart';

/// Safe filesystem path operations.
///
/// Delegates to libproven for traversal detection and sanitization.
class SafePath {
  /// Check if a path contains directory traversal sequences.
  /// Returns null on FFI error.
  static bool? hasTraversal(String path) {
    final (ptr, len) = toNativeUtf8Bytes(path);
    try {
      final result = provenPathHasTraversal(ptr, len);
      if (result.status != ProvenStatus.ok) return null;
      return result.value;
    } finally {
      calloc.free(ptr);
    }
  }

  /// Check if a path is safe (no traversal).
  /// Returns null on FFI error.
  static bool? isSafePath(String path) {
    final traversal = hasTraversal(path);
    if (traversal == null) return null;
    return !traversal;
  }

  /// Sanitize a filename by removing dangerous characters.
  /// Returns null on FFI error.
  static String? sanitizeFilename(String filename) {
    final (ptr, len) = toNativeUtf8Bytes(filename);
    try {
      final result = provenPathSanitizeFilename(ptr, len);
      return callStringFfi(result);
    } finally {
      calloc.free(ptr);
    }
  }
}
