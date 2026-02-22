// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe cryptographic operations via libproven FFI.
///
/// All computation is performed in the formally verified Idris 2 core.
/// This module provides constant-time comparison and secure random bytes.
library;

import 'dart:ffi';
import 'dart:typed_data';

import 'package:ffi/ffi.dart';

import 'ffi.dart';

/// Safe cryptographic operations.
///
/// Delegates to libproven for timing-attack-safe comparisons
/// and cryptographically secure random bytes.
class SafeCrypto {
  /// Constant-time comparison of two byte sequences.
  /// Returns null on FFI error, true if equal, false if not.
  static bool? constantTimeEquals(Uint8List a, Uint8List b) {
    final ptrA = calloc<Uint8>(a.length);
    final ptrB = calloc<Uint8>(b.length);
    try {
      for (var i = 0; i < a.length; i++) {
        ptrA[i] = a[i];
      }
      for (var i = 0; i < b.length; i++) {
        ptrB[i] = b[i];
      }
      final result =
          provenCryptoConstantTimeEq(ptrA, a.length, ptrB, b.length);
      if (result.status != ProvenStatus.ok) return null;
      return result.value;
    } finally {
      calloc.free(ptrA);
      calloc.free(ptrB);
    }
  }

  /// Constant-time string comparison.
  /// Returns null on FFI error, true if equal, false if not.
  static bool? constantTimeEqualsStrings(String a, String b) {
    final bytesA = Uint8List.fromList(a.codeUnits);
    final bytesB = Uint8List.fromList(b.codeUnits);
    return constantTimeEquals(bytesA, bytesB);
  }

  /// Generate cryptographically secure random bytes.
  /// Returns null on FFI error.
  static Uint8List? randomBytes(int count) {
    if (count <= 0) return Uint8List(0);
    final ptr = calloc<Uint8>(count);
    try {
      final status = provenCryptoRandomBytes(ptr, count);
      if (status != ProvenStatus.ok) return null;
      final bytes = Uint8List(count);
      for (var i = 0; i < count; i++) {
        bytes[i] = ptr[i];
      }
      return bytes;
    } finally {
      calloc.free(ptr);
    }
  }
}
