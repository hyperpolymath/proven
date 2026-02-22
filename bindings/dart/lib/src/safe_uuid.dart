// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe UUID generation and validation via libproven FFI.
///
/// All computation is performed in the formally verified Idris 2 core.
/// Follows RFC 4122.
library;

import 'dart:ffi';

import 'package:ffi/ffi.dart';

import 'ffi.dart';

/// Safe UUID operations.
///
/// Delegates to libproven for UUID v4 generation, parsing, and validation.
class SafeUuid {
  /// Generate a new v4 (random) UUID string.
  /// Returns null on FFI error.
  static String? v4() {
    final result = provenUuidV4();
    if (result.status != ProvenStatus.ok) return null;
    final strResult = provenUuidToString(result.uuid);
    return callStringFfi(strResult);
  }

  /// Parse a UUID string and return its canonical lowercase form.
  /// Returns null if the string is not a valid UUID.
  static String? parse(String input) {
    final (ptr, len) = toNativeUtf8Bytes(input);
    try {
      final result = provenUuidParse(ptr, len);
      if (result.status != ProvenStatus.ok) return null;
      final strResult = provenUuidToString(result.uuid);
      return callStringFfi(strResult);
    } finally {
      calloc.free(ptr);
    }
  }

  /// Check if a string is a valid UUID.
  static bool isValid(String input) {
    final (ptr, len) = toNativeUtf8Bytes(input);
    try {
      final result = provenUuidParse(ptr, len);
      return result.status == ProvenStatus.ok;
    } finally {
      calloc.free(ptr);
    }
  }

  /// Check if a UUID string represents the nil UUID (all zeros).
  /// Returns null on FFI error.
  static bool? isNil(String input) {
    final (ptr, len) = toNativeUtf8Bytes(input);
    try {
      final result = provenUuidParse(ptr, len);
      if (result.status != ProvenStatus.ok) return null;
      return provenUuidIsNil(result.uuid);
    } finally {
      calloc.free(ptr);
    }
  }

  /// Get the version number of a UUID string.
  /// Returns null on FFI error or invalid UUID.
  static int? version(String input) {
    final (ptr, len) = toNativeUtf8Bytes(input);
    try {
      final result = provenUuidParse(ptr, len);
      if (result.status != ProvenStatus.ok) return null;
      return provenUuidVersion(result.uuid);
    } finally {
      calloc.free(ptr);
    }
  }
}
