// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe currency operations via libproven FFI.
///
/// All parsing is performed in the formally verified Idris 2 core.
library;

import 'dart:ffi';

import 'package:ffi/ffi.dart';

import 'ffi.dart';

/// Parsed currency value.
class CurrencyValue {
  /// Amount in minor units (cents, satoshis, etc.).
  final int amountMinor;

  /// Three-letter ISO 4217 currency code.
  final String code;

  /// Number of decimal places for this currency.
  final int decimalPlaces;

  const CurrencyValue({
    required this.amountMinor,
    required this.code,
    required this.decimalPlaces,
  });

  @override
  String toString() => '$amountMinor ($code, $decimalPlaces decimals)';
}

/// Safe currency operations.
///
/// Delegates to libproven for currency code parsing and validation.
class SafeCurrency {
  /// Parse a currency amount string.
  /// Returns null on invalid input.
  static CurrencyValue? parse(String input) {
    final (ptr, len) = toNativeUtf8Bytes(input);
    try {
      final result = provenCurrencyParse(ptr, len);
      if (result.status != ProvenStatus.ok) return null;
      final codeBytes = [
        result.code[0],
        result.code[1],
        result.code[2],
      ];
      final code = String.fromCharCodes(codeBytes);
      return CurrencyValue(
        amountMinor: result.amountMinor,
        code: code,
        decimalPlaces: result.decimalPlaces,
      );
    } finally {
      calloc.free(ptr);
    }
  }
}
