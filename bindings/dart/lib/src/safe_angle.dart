// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe angle operations for Dart with unit conversions.
library;

import 'dart:math' as math;

/// Angle unit.
enum AngleUnit {
  radians,
  degrees,
  gradians,
  turns,
}

/// An angle value with safe operations.
class Angle implements Comparable<Angle> {
  final double _radians;

  const Angle._(this._radians);

  /// Create from radians.
  factory Angle.radians(double value) {
    if (value.isNaN || value.isInfinite) {
      throw ArgumentError('Angle cannot be NaN or Infinite');
    }
    return Angle._(value);
  }

  /// Create from degrees.
  factory Angle.degrees(double value) {
    if (value.isNaN || value.isInfinite) {
      throw ArgumentError('Angle cannot be NaN or Infinite');
    }
    return Angle._(value * math.pi / 180);
  }

  /// Create from gradians.
  factory Angle.gradians(double value) {
    if (value.isNaN || value.isInfinite) {
      throw ArgumentError('Angle cannot be NaN or Infinite');
    }
    return Angle._(value * math.pi / 200);
  }

  /// Create from turns (1 turn = 360 degrees).
  factory Angle.turns(double value) {
    if (value.isNaN || value.isInfinite) {
      throw ArgumentError('Angle cannot be NaN or Infinite');
    }
    return Angle._(value * 2 * math.pi);
  }

  /// Zero angle.
  static const Angle zero = Angle._(0);

  /// Right angle (90 degrees).
  static const Angle rightAngle = Angle._(math.pi / 2);

  /// Straight angle (180 degrees).
  static const Angle straightAngle = Angle._(math.pi);

  /// Full circle (360 degrees).
  static const Angle fullCircle = Angle._(2 * math.pi);

  /// Get value in radians.
  double get inRadians => _radians;

  /// Get value in degrees.
  double get inDegrees => _radians * 180 / math.pi;

  /// Get value in gradians.
  double get inGradians => _radians * 200 / math.pi;

  /// Get value in turns.
  double get inTurns => _radians / (2 * math.pi);

  /// Normalize to [0, 2*PI).
  Angle normalize() {
    var r = _radians % (2 * math.pi);
    if (r < 0) r += 2 * math.pi;
    return Angle._(r);
  }

  /// Normalize to [-PI, PI).
  Angle normalizeSymmetric() {
    var r = _radians % (2 * math.pi);
    if (r < 0) r += 2 * math.pi;
    if (r >= math.pi) r -= 2 * math.pi;
    return Angle._(r);
  }

  /// Get sine.
  double get sin => math.sin(_radians);

  /// Get cosine.
  double get cos => math.cos(_radians);

  /// Get tangent.
  double get tan => math.tan(_radians);

  /// Create from arc sine.
  static Angle? asin(double value) {
    if (value < -1 || value > 1) return null;
    return Angle._(math.asin(value));
  }

  /// Create from arc cosine.
  static Angle? acos(double value) {
    if (value < -1 || value > 1) return null;
    return Angle._(math.acos(value));
  }

  /// Create from arc tangent.
  static Angle atan(double value) {
    return Angle._(math.atan(value));
  }

  /// Create from arc tangent of y/x.
  static Angle atan2(double y, double x) {
    return Angle._(math.atan2(y, x));
  }

  /// Add angles.
  Angle operator +(Angle other) => Angle._(_radians + other._radians);

  /// Subtract angles.
  Angle operator -(Angle other) => Angle._(_radians - other._radians);

  /// Negate angle.
  Angle operator -() => Angle._(-_radians);

  /// Multiply by scalar.
  Angle operator *(double factor) => Angle._(_radians * factor);

  /// Divide by scalar.
  Angle operator /(double divisor) {
    if (divisor == 0) throw ArgumentError('Cannot divide by zero');
    return Angle._(_radians / divisor);
  }

  @override
  int compareTo(Angle other) => _radians.compareTo(other._radians);

  bool operator <(Angle other) => _radians < other._radians;
  bool operator <=(Angle other) => _radians <= other._radians;
  bool operator >(Angle other) => _radians > other._radians;
  bool operator >=(Angle other) => _radians >= other._radians;

  @override
  bool operator ==(Object other) {
    return other is Angle && _radians == other._radians;
  }

  @override
  int get hashCode => _radians.hashCode;

  @override
  String toString() => '${inDegrees.toStringAsFixed(2)}°';
}

/// Safe angle operations.
class SafeAngle {
  /// Convert between units.
  static double convert(double value, AngleUnit from, AngleUnit to) {
    if (value.isNaN || value.isInfinite) return value;

    // Convert to radians first
    final radians = _toRadians(value, from);
    // Convert from radians to target
    return _fromRadians(radians, to);
  }

  static double _toRadians(double value, AngleUnit from) {
    switch (from) {
      case AngleUnit.radians:
        return value;
      case AngleUnit.degrees:
        return value * math.pi / 180;
      case AngleUnit.gradians:
        return value * math.pi / 200;
      case AngleUnit.turns:
        return value * 2 * math.pi;
    }
  }

  static double _fromRadians(double radians, AngleUnit to) {
    switch (to) {
      case AngleUnit.radians:
        return radians;
      case AngleUnit.degrees:
        return radians * 180 / math.pi;
      case AngleUnit.gradians:
        return radians * 200 / math.pi;
      case AngleUnit.turns:
        return radians / (2 * math.pi);
    }
  }

  /// Normalize angle to [0, 360) degrees.
  static double normalizeDegrees(double degrees) {
    if (degrees.isNaN || degrees.isInfinite) return degrees;
    var d = degrees % 360;
    if (d < 0) d += 360;
    return d;
  }

  /// Normalize angle to [0, 2*PI) radians.
  static double normalizeRadians(double radians) {
    if (radians.isNaN || radians.isInfinite) return radians;
    var r = radians % (2 * math.pi);
    if (r < 0) r += 2 * math.pi;
    return r;
  }

  /// Get the smallest difference between two angles in degrees.
  static double differenceDegrees(double a, double b) {
    var diff = (a - b) % 360;
    if (diff > 180) diff -= 360;
    if (diff < -180) diff += 360;
    return diff;
  }

  /// Get the smallest difference between two angles in radians.
  static double differenceRadians(double a, double b) {
    var diff = (a - b) % (2 * math.pi);
    if (diff > math.pi) diff -= 2 * math.pi;
    if (diff < -math.pi) diff += 2 * math.pi;
    return diff;
  }

  /// Interpolate between two angles (taking the shortest path).
  static double lerpDegrees(double a, double b, double t) {
    final diff = differenceDegrees(b, a);
    return normalizeDegrees(a + diff * t);
  }

  /// Interpolate between two angles in radians (taking the shortest path).
  static double lerpRadians(double a, double b, double t) {
    final diff = differenceRadians(b, a);
    return normalizeRadians(a + diff * t);
  }

  /// Check if an angle is within a range (handles wraparound).
  static bool isWithinRange(double angle, double start, double end, {bool degrees = true}) {
    if (degrees) {
      angle = normalizeDegrees(angle);
      start = normalizeDegrees(start);
      end = normalizeDegrees(end);

      if (start <= end) {
        return angle >= start && angle <= end;
      } else {
        return angle >= start || angle <= end;
      }
    } else {
      angle = normalizeRadians(angle);
      start = normalizeRadians(start);
      end = normalizeRadians(end);

      if (start <= end) {
        return angle >= start && angle <= end;
      } else {
        return angle >= start || angle <= end;
      }
    }
  }

  /// Convert compass bearing to standard angle (0° = East, counterclockwise).
  static double bearingToStandard(double bearing) {
    return normalizeDegrees(90 - bearing);
  }

  /// Convert standard angle to compass bearing (0° = North, clockwise).
  static double standardToBearing(double angle) {
    return normalizeDegrees(90 - angle);
  }

  /// Check if angle represents a valid value.
  static bool isValid(double value) {
    return !value.isNaN && !value.isInfinite;
  }
}
