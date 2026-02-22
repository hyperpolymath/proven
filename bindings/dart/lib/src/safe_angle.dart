// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe angle operations via libproven FFI.
///
/// All computation is performed in the formally verified Idris 2 core.
library;

import 'ffi.dart';

/// Safe angle operations.
///
/// Delegates to libproven for angle unit conversion and normalization.
class SafeAngle {
  /// Convert degrees to radians.
  static double degreesToRadians(double degrees) {
    return provenAngleDegToRad(degrees);
  }

  /// Convert radians to degrees.
  static double radiansToDegrees(double radians) {
    return provenAngleRadToDeg(radians);
  }

  /// Normalize angle to [0, 360) degrees.
  static double normalizeDegrees(double degrees) {
    return provenAngleNormalizeDegrees(degrees);
  }

  /// Normalize angle to [0, 2*PI) radians.
  static double normalizeRadians(double radians) {
    return provenAngleNormalizeRadians(radians);
  }
}
