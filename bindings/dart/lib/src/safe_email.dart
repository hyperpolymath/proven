// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe email validation via libproven FFI.
///
/// All validation logic is performed in the formally verified Idris 2 core.
library;

import 'dart:ffi';

import 'package:ffi/ffi.dart';

import 'ffi.dart';

/// Safe email validation.
///
/// Delegates to libproven for email address validation.
class SafeEmail {
  /// Validate an email address.
  /// Returns null on FFI error.
  static bool? isValid(String email) {
    final (ptr, len) = toNativeUtf8Bytes(email);
    try {
      final result = provenEmailIsValid(ptr, len);
      if (result.status != ProvenStatus.ok) return null;
      return result.value;
    } finally {
      calloc.free(ptr);
    }
  }
}
