// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe floating-point operations for Dart with NaN/Infinity protection.
library;

/// Result of a float operation.
class FloatResult {
  final double? value;
  final String? error;

  const FloatResult.ok(double v)
      : value = v,
        error = null;

  const FloatResult.error(String e)
      : value = null,
        error = e;

  bool get isOk => error == null;

  double unwrap() {
    if (error != null) {
      throw FloatException(error!);
    }
    return value!;
  }

  double unwrapOr(double defaultValue) => value ?? defaultValue;
}

/// Exception thrown on float errors.
class FloatException implements Exception {
  final String message;
  const FloatException(this.message);

  @override
  String toString() => 'FloatException: $message';
}

/// A validated finite float that cannot be NaN or Infinity.
class FiniteFloat {
  final double _value;

  FiniteFloat._(this._value);

  /// Create a FiniteFloat, returning null if invalid.
  static FiniteFloat? tryCreate(double value) {
    if (value.isNaN || value.isInfinite) {
      return null;
    }
    return FiniteFloat._(value);
  }

  /// Create a FiniteFloat, throwing if invalid.
  factory FiniteFloat(double value) {
    if (value.isNaN) {
      throw const FloatException('Value is NaN');
    }
    if (value.isInfinite) {
      throw const FloatException('Value is Infinite');
    }
    return FiniteFloat._(value);
  }

  double get value => _value;

  FiniteFloat? add(FiniteFloat other) {
    return tryCreate(_value + other._value);
  }

  FiniteFloat? subtract(FiniteFloat other) {
    return tryCreate(_value - other._value);
  }

  FiniteFloat? multiply(FiniteFloat other) {
    return tryCreate(_value * other._value);
  }

  FiniteFloat? divide(FiniteFloat other) {
    if (other._value == 0) return null;
    return tryCreate(_value / other._value);
  }

  @override
  String toString() => _value.toString();

  @override
  bool operator ==(Object other) {
    return other is FiniteFloat && _value == other._value;
  }

  @override
  int get hashCode => _value.hashCode;
}

/// Safe floating-point operations.
class SafeFloat {
  /// Check if a value is finite (not NaN or Infinity).
  static bool isFinite(double value) {
    return !value.isNaN && !value.isInfinite;
  }

  /// Check if a value is valid (finite).
  static bool isValid(double value) => isFinite(value);

  /// Clamp to finite value (NaN and Infinity become the bounds).
  static double clampFinite(double value, double min, double max) {
    if (value.isNaN) return min;
    if (value == double.infinity) return max;
    if (value == double.negativeInfinity) return min;
    return value.clamp(min, max);
  }

  /// Safe addition.
  static FloatResult add(double a, double b) {
    if (!isFinite(a) || !isFinite(b)) {
      return const FloatResult.error('Invalid operand');
    }
    final result = a + b;
    if (!isFinite(result)) {
      return const FloatResult.error('Result is not finite');
    }
    return FloatResult.ok(result);
  }

  /// Safe subtraction.
  static FloatResult subtract(double a, double b) {
    if (!isFinite(a) || !isFinite(b)) {
      return const FloatResult.error('Invalid operand');
    }
    final result = a - b;
    if (!isFinite(result)) {
      return const FloatResult.error('Result is not finite');
    }
    return FloatResult.ok(result);
  }

  /// Safe multiplication.
  static FloatResult multiply(double a, double b) {
    if (!isFinite(a) || !isFinite(b)) {
      return const FloatResult.error('Invalid operand');
    }
    final result = a * b;
    if (!isFinite(result)) {
      return const FloatResult.error('Result is not finite');
    }
    return FloatResult.ok(result);
  }

  /// Safe division.
  static FloatResult divide(double a, double b) {
    if (!isFinite(a) || !isFinite(b)) {
      return const FloatResult.error('Invalid operand');
    }
    if (b == 0) {
      return const FloatResult.error('Division by zero');
    }
    final result = a / b;
    if (!isFinite(result)) {
      return const FloatResult.error('Result is not finite');
    }
    return FloatResult.ok(result);
  }

  /// Safe power.
  static FloatResult pow(double base, double exponent) {
    if (!isFinite(base) || !isFinite(exponent)) {
      return const FloatResult.error('Invalid operand');
    }
    final result = _pow(base, exponent);
    if (!isFinite(result)) {
      return const FloatResult.error('Result is not finite');
    }
    return FloatResult.ok(result);
  }

  static double _pow(double x, double y) {
    // Use dart:math pow but return as double
    return x == 0 && y == 0 ? 1.0 : (x < 0 ? double.nan : _exp(y * _ln(x)));
  }

  static double _exp(double x) {
    // Simple exponential using series
    if (x.abs() > 700) return x > 0 ? double.infinity : 0;
    double result = 1.0;
    double term = 1.0;
    for (int i = 1; i < 100; i++) {
      term *= x / i;
      result += term;
      if (term.abs() < 1e-15) break;
    }
    return result;
  }

  static double _ln(double x) {
    if (x <= 0) return double.nan;
    if (x == 1) return 0;

    // Use natural log identity
    int exp = 0;
    while (x > 2) {
      x /= 2.718281828459045;
      exp++;
    }
    while (x < 0.5) {
      x *= 2.718281828459045;
      exp--;
    }

    // Taylor series for ln near 1
    final y = (x - 1) / (x + 1);
    double result = 0;
    double term = y;
    for (int i = 1; i < 100; i += 2) {
      result += term / i;
      term *= y * y;
      if (term.abs() < 1e-15) break;
    }
    return 2 * result + exp;
  }

  /// Safe square root.
  static FloatResult sqrt(double value) {
    if (!isFinite(value)) {
      return const FloatResult.error('Invalid operand');
    }
    if (value < 0) {
      return const FloatResult.error('Cannot take square root of negative');
    }
    return FloatResult.ok(_sqrt(value));
  }

  static double _sqrt(double x) {
    if (x == 0) return 0;
    double guess = x / 2;
    for (int i = 0; i < 50; i++) {
      final newGuess = (guess + x / guess) / 2;
      if ((newGuess - guess).abs() < 1e-15) break;
      guess = newGuess;
    }
    return guess;
  }

  /// Parse a float with validation.
  static FloatResult parse(String value) {
    final parsed = double.tryParse(value);
    if (parsed == null) {
      return const FloatResult.error('Invalid number format');
    }
    if (!isFinite(parsed)) {
      return const FloatResult.error('Value is not finite');
    }
    return FloatResult.ok(parsed);
  }

  /// Round to decimal places.
  static double roundTo(double value, int decimalPlaces) {
    if (!isFinite(value)) return value;
    if (decimalPlaces < 0) return value;

    final multiplier = _pow10(decimalPlaces);
    return (value * multiplier).roundToDouble() / multiplier;
  }

  static double _pow10(int n) {
    double result = 1;
    for (int i = 0; i < n; i++) {
      result *= 10;
    }
    return result;
  }

  /// Truncate to decimal places.
  static double truncateTo(double value, int decimalPlaces) {
    if (!isFinite(value)) return value;
    if (decimalPlaces < 0) return value;

    final multiplier = _pow10(decimalPlaces);
    return (value * multiplier).truncateToDouble() / multiplier;
  }

  /// Check if two floats are approximately equal.
  static bool approxEqual(double a, double b, {double epsilon = 1e-10}) {
    if (!isFinite(a) || !isFinite(b)) return false;
    return (a - b).abs() < epsilon;
  }

  /// Lerp between two values.
  static FloatResult lerp(double a, double b, double t) {
    if (!isFinite(a) || !isFinite(b) || !isFinite(t)) {
      return const FloatResult.error('Invalid operand');
    }
    final result = a + (b - a) * t;
    if (!isFinite(result)) {
      return const FloatResult.error('Result is not finite');
    }
    return FloatResult.ok(result);
  }

  /// Inverse lerp (find t given a value between a and b).
  static FloatResult inverseLerp(double a, double b, double value) {
    if (!isFinite(a) || !isFinite(b) || !isFinite(value)) {
      return const FloatResult.error('Invalid operand');
    }
    if (a == b) {
      return const FloatResult.error('a and b cannot be equal');
    }
    final result = (value - a) / (b - a);
    if (!isFinite(result)) {
      return const FloatResult.error('Result is not finite');
    }
    return FloatResult.ok(result);
  }

  /// Sign of value (-1, 0, 1).
  static int sign(double value) {
    if (value.isNaN) return 0;
    if (value > 0) return 1;
    if (value < 0) return -1;
    return 0;
  }
}
