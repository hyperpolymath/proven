// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe JSON validation via libproven FFI.
///
/// All validation is performed in the formally verified Idris 2 core.
library;

import 'dart:ffi';

import 'package:ffi/ffi.dart';

import 'ffi.dart';

/// Safe JSON operations.
///
/// Delegates to libproven for JSON validation.
class SafeJson {
  /// Check if a string is valid JSON.
  /// Returns null on FFI error.
  static bool? isValid(String jsonString) {
    final (ptr, len) = toNativeUtf8Bytes(jsonString);
    try {
      final result = provenJsonIsValid(ptr, len);
      if (result.status != ProvenStatus.ok) return null;
      return result.value;
    } finally {
      calloc.free(ptr);
    }
  }
}
