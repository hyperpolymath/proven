// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe mathematical operations for Dart with overflow protection.
library;

/// Result of a checked integer operation.
class CheckedInt {
  final int? value;
  final bool overflow;

  const CheckedInt.ok(int val)
      : value = val,
        overflow = false;

  const CheckedInt.overflow()
      : value = null,
        overflow = true;

  bool get isOk => !overflow;

  /// Get value or throw if overflow.
  int unwrap() {
    if (overflow) {
      throw OverflowException('Integer overflow occurred');
    }
    return value!;
  }

  /// Get value or return default.
  int unwrapOr(int defaultValue) => value ?? defaultValue;

  @override
  String toString() => overflow ? 'CheckedInt.overflow()' : 'CheckedInt.ok($value)';
}

/// Exception thrown on integer overflow.
class OverflowException implements Exception {
  final String message;
  const OverflowException(this.message);

  @override
  String toString() => 'OverflowException: $message';
}

/// Safe mathematical operations with overflow checking.
class SafeMath {
  /// Maximum safe integer in Dart (64-bit signed).
  static const int maxInt = 0x7FFFFFFFFFFFFFFF;

  /// Minimum safe integer in Dart (64-bit signed).
  static const int minInt = -0x8000000000000000;

  /// Checked addition that detects overflow.
  static CheckedInt add(int a, int b) {
    // Check for overflow before performing operation
    if (b > 0 && a > maxInt - b) {
      return const CheckedInt.overflow();
    }
    if (b < 0 && a < minInt - b) {
      return const CheckedInt.overflow();
    }
    return CheckedInt.ok(a + b);
  }

  /// Checked subtraction that detects overflow.
  static CheckedInt sub(int a, int b) {
    // Check for overflow before performing operation
    if (b < 0 && a > maxInt + b) {
      return const CheckedInt.overflow();
    }
    if (b > 0 && a < minInt + b) {
      return const CheckedInt.overflow();
    }
    return CheckedInt.ok(a - b);
  }

  /// Checked multiplication that detects overflow.
  static CheckedInt mul(int a, int b) {
    if (a == 0 || b == 0) {
      return const CheckedInt.ok(0);
    }

    // Special case for minInt * -1
    if (a == minInt && b == -1) {
      return const CheckedInt.overflow();
    }
    if (b == minInt && a == -1) {
      return const CheckedInt.overflow();
    }

    // Use BigInt for overflow detection
    final bigResult = BigInt.from(a) * BigInt.from(b);
    if (bigResult > BigInt.from(maxInt) || bigResult < BigInt.from(minInt)) {
      return const CheckedInt.overflow();
    }

    return CheckedInt.ok(a * b);
  }

  /// Checked division that handles division by zero.
  static CheckedInt div(int numerator, int denominator) {
    if (denominator == 0) {
      return const CheckedInt.overflow();
    }

    // Check for overflow: minInt / -1 would overflow
    if (numerator == minInt && denominator == -1) {
      return const CheckedInt.overflow();
    }

    return CheckedInt.ok(numerator ~/ denominator);
  }

  /// Checked modulo operation.
  static CheckedInt mod(int a, int b) {
    if (b == 0) {
      return const CheckedInt.overflow();
    }
    return CheckedInt.ok(a % b);
  }

  /// Checked negation.
  static CheckedInt neg(int a) {
    if (a == minInt) {
      return const CheckedInt.overflow();
    }
    return CheckedInt.ok(-a);
  }

  /// Checked absolute value.
  static CheckedInt abs(int a) {
    if (a == minInt) {
      return const CheckedInt.overflow();
    }
    return CheckedInt.ok(a.abs());
  }

  /// Checked power operation.
  static CheckedInt pow(int base, int exponent) {
    if (exponent < 0) {
      return const CheckedInt.overflow();
    }
    if (exponent == 0) {
      return const CheckedInt.ok(1);
    }
    if (base == 0) {
      return const CheckedInt.ok(0);
    }
    if (base == 1) {
      return CheckedInt.ok(1);
    }
    if (base == -1) {
      return CheckedInt.ok(exponent.isOdd ? -1 : 1);
    }

    // Use BigInt for overflow detection
    final bigResult = BigInt.from(base).pow(exponent);
    if (bigResult > BigInt.from(maxInt) || bigResult < BigInt.from(minInt)) {
      return const CheckedInt.overflow();
    }

    return CheckedInt.ok(bigResult.toInt());
  }

  /// Clamp value to range.
  static int clamp(int value, int min, int max) {
    if (value < min) return min;
    if (value > max) return max;
    return value;
  }

  /// Check if addition would overflow without performing it.
  static bool wouldAddOverflow(int a, int b) {
    return add(a, b).overflow;
  }

  /// Check if multiplication would overflow without performing it.
  static bool wouldMulOverflow(int a, int b) {
    return mul(a, b).overflow;
  }
}
