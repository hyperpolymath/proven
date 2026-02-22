// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe phone number validation via libproven FFI.
///
/// All parsing is performed in the formally verified Idris 2 core.
/// Follows E.164 standard.
library;

import 'dart:ffi';

import 'package:ffi/ffi.dart';

import 'ffi.dart';

/// Parsed phone number.
class PhoneNumber {
  /// Country calling code (e.g., 1 for US/CA, 44 for UK).
  final int countryCode;

  /// National number as integer.
  final int nationalNumber;

  const PhoneNumber({
    required this.countryCode,
    required this.nationalNumber,
  });

  /// Format in E.164 format (e.g., +15551234567).
  String? toE164() {
    final result = provenPhoneFormatE164(countryCode, nationalNumber);
    return callStringFfi(result);
  }

  @override
  String toString() => '+$countryCode$nationalNumber';
}

/// Safe phone number operations.
///
/// Delegates to libproven for phone number parsing and formatting.
class SafePhone {
  /// Parse a phone number string.
  /// Returns null on invalid input.
  static PhoneNumber? parse(String input) {
    final (ptr, len) = toNativeUtf8Bytes(input);
    try {
      final result = provenPhoneParse(ptr, len);
      if (result.status != ProvenStatus.ok) return null;
      return PhoneNumber(
        countryCode: result.countryCode,
        nationalNumber: result.nationalNumber,
      );
    } finally {
      calloc.free(ptr);
    }
  }

  /// Check if a phone number string is valid.
  static bool isValid(String input) {
    return parse(input) != null;
  }

  /// Format a phone number in E.164 format.
  /// Returns null on invalid input.
  static String? formatE164(String input) {
    final phone = parse(input);
    return phone?.toE164();
  }
}
