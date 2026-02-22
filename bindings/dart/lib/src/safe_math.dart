// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe mathematical operations via libproven FFI.
///
/// All computation is performed in Idris 2 with dependent types and
/// totality checking. This module is a thin Dart wrapper that marshals
/// arguments across the C ABI boundary and returns nullable types on error.
library;

import 'ffi.dart';

/// Safe mathematical operations with overflow checking.
///
/// Every method delegates to the formally verified Idris 2 implementation
/// via the libproven C ABI. Returns null on error (overflow, division by
/// zero, etc.) instead of crashing.
class SafeMath {
  /// Checked addition. Returns null on overflow.
  static int? add(int a, int b) {
    final result = provenMathAddChecked(a, b);
    if (result.status != ProvenStatus.ok) return null;
    return result.value;
  }

  /// Checked subtraction. Returns null on underflow.
  static int? sub(int a, int b) {
    final result = provenMathSubChecked(a, b);
    if (result.status != ProvenStatus.ok) return null;
    return result.value;
  }

  /// Checked multiplication. Returns null on overflow.
  static int? mul(int a, int b) {
    final result = provenMathMulChecked(a, b);
    if (result.status != ProvenStatus.ok) return null;
    return result.value;
  }

  /// Checked division. Returns null on division by zero or overflow.
  static int? div(int a, int b) {
    final result = provenMathDiv(a, b);
    if (result.status != ProvenStatus.ok) return null;
    return result.value;
  }

  /// Checked modulo. Returns null on division by zero.
  static int? mod(int a, int b) {
    final result = provenMathMod(a, b);
    if (result.status != ProvenStatus.ok) return null;
    return result.value;
  }

  /// Safe absolute value. Returns null if input is INT64_MIN.
  static int? abs(int a) {
    final result = provenMathAbsSafe(a);
    if (result.status != ProvenStatus.ok) return null;
    return result.value;
  }

  /// Clamp value to range [lo, hi].
  static int clamp(int value, int lo, int hi) {
    return provenMathClamp(lo, hi, value);
  }

  /// Checked integer exponentiation. Returns null on overflow.
  static int? pow(int base, int exponent) {
    final result = provenMathPowChecked(base, exponent);
    if (result.status != ProvenStatus.ok) return null;
    return result.value;
  }
}
