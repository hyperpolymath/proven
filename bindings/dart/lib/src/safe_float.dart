// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe floating-point operations via libproven FFI.
///
/// All computation is performed in the formally verified Idris 2 core.
/// Returns null on error (NaN, Infinity, division by zero, etc.).
library;

import 'ffi.dart';

/// Safe floating-point operations.
///
/// Delegates to libproven for NaN/Infinity-safe arithmetic.
class SafeFloat {
  /// Safe division. Returns null on division by zero or non-finite result.
  static double? div(double a, double b) {
    final result = provenFloatDiv(a, b);
    if (result.status != ProvenStatus.ok) return null;
    return result.value;
  }

  /// Check if a value is finite (not NaN or Infinity).
  static bool isFinite(double value) {
    return provenFloatIsFinite(value);
  }

  /// Check if a value is NaN.
  static bool isNan(double value) {
    return provenFloatIsNan(value);
  }

  /// Safe square root. Returns null for negative input or non-finite result.
  static double? sqrt(double value) {
    final result = provenFloatSqrt(value);
    if (result.status != ProvenStatus.ok) return null;
    return result.value;
  }

  /// Safe natural logarithm. Returns null for non-positive input.
  static double? ln(double value) {
    final result = provenFloatLn(value);
    if (result.status != ProvenStatus.ok) return null;
    return result.value;
  }
}
