// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe hexadecimal encoding and decoding.
library;

import 'dart:typed_data';

/// Hex decoding result.
class HexResult {
  final Uint8List? value;
  final String? error;

  const HexResult.ok(Uint8List bytes)
      : value = bytes,
        error = null;

  const HexResult.error(String e)
      : value = null,
        error = e;

  bool get isOk => error == null;
  bool get isError => error != null;

  /// Get bytes or throw if error.
  Uint8List unwrap() {
    if (error != null) {
      throw HexException(error!);
    }
    return value!;
  }

  /// Get bytes or return default.
  Uint8List unwrapOr(Uint8List defaultValue) => value ?? defaultValue;
}

/// Exception thrown for hex errors.
class HexException implements Exception {
  final String message;
  const HexException(this.message);

  @override
  String toString() => 'HexException: $message';
}

/// Safe hexadecimal encoding and decoding.
class SafeHex {
  static const String _lowerHexChars = '0123456789abcdef';
  static const String _upperHexChars = '0123456789ABCDEF';

  /// Check if character is valid hex.
  static bool isHexChar(String char) {
    if (char.length != 1) return false;
    final c = char.codeUnitAt(0);
    return (c >= 0x30 && c <= 0x39) || // 0-9
        (c >= 0x41 && c <= 0x46) || // A-F
        (c >= 0x61 && c <= 0x66); // a-f
  }

  /// Validate hex string.
  static bool isValid(String hex) {
    if (hex.isEmpty) return true;
    for (var i = 0; i < hex.length; i++) {
      if (!isHexChar(hex[i])) return false;
    }
    return true;
  }

  /// Validate hex string with even length (valid for byte decoding).
  static bool isValidBytes(String hex) {
    return hex.length % 2 == 0 && isValid(hex);
  }

  /// Convert hex character to nibble value (0-15).
  static int? hexCharToNibble(String char) {
    if (char.length != 1) return null;
    final c = char.codeUnitAt(0);

    if (c >= 0x30 && c <= 0x39) {
      return c - 0x30; // 0-9
    } else if (c >= 0x41 && c <= 0x46) {
      return c - 0x41 + 10; // A-F
    } else if (c >= 0x61 && c <= 0x66) {
      return c - 0x61 + 10; // a-f
    }
    return null;
  }

  /// Convert nibble value (0-15) to lowercase hex character.
  static String nibbleToHexChar(int nibble) {
    if (nibble < 0 || nibble > 15) {
      throw HexException('Nibble must be 0-15');
    }
    return _lowerHexChars[nibble];
  }

  /// Convert nibble value (0-15) to uppercase hex character.
  static String nibbleToHexCharUpper(int nibble) {
    if (nibble < 0 || nibble > 15) {
      throw HexException('Nibble must be 0-15');
    }
    return _upperHexChars[nibble];
  }

  /// Encode bytes to lowercase hex string.
  static String encode(List<int> bytes) {
    final buffer = StringBuffer();
    for (final b in bytes) {
      buffer.write(_lowerHexChars[(b >> 4) & 0x0F]);
      buffer.write(_lowerHexChars[b & 0x0F]);
    }
    return buffer.toString();
  }

  /// Encode bytes to uppercase hex string.
  static String encodeUpper(List<int> bytes) {
    final buffer = StringBuffer();
    for (final b in bytes) {
      buffer.write(_upperHexChars[(b >> 4) & 0x0F]);
      buffer.write(_upperHexChars[b & 0x0F]);
    }
    return buffer.toString();
  }

  /// Decode hex string to bytes.
  static HexResult decode(String hex) {
    // Allow and strip common prefixes
    var cleanHex = hex;
    if (cleanHex.startsWith('0x') || cleanHex.startsWith('0X')) {
      cleanHex = cleanHex.substring(2);
    }

    // Remove any whitespace, colons, or dashes
    cleanHex = cleanHex.replaceAll(RegExp(r'[\s:\-]'), '');

    if (cleanHex.isEmpty) {
      return HexResult.ok(Uint8List(0));
    }

    if (cleanHex.length % 2 != 0) {
      return const HexResult.error('Hex string has odd length');
    }

    final bytes = Uint8List(cleanHex.length ~/ 2);
    for (var i = 0; i < bytes.length; i++) {
      final high = hexCharToNibble(cleanHex[i * 2]);
      final low = hexCharToNibble(cleanHex[i * 2 + 1]);

      if (high == null || low == null) {
        return const HexResult.error('Invalid hex character');
      }

      bytes[i] = (high << 4) | low;
    }

    return HexResult.ok(bytes);
  }

  /// Decode hex string, throwing on error.
  static Uint8List decodeStrict(String hex) {
    return decode(hex).unwrap();
  }

  /// Constant-time comparison of two hex strings.
  /// Returns true if equal (case-insensitive).
  static bool constantTimeEqual(String a, String b) {
    if (a.length != b.length) {
      return false;
    }

    var diff = 0;
    for (var i = 0; i < a.length; i++) {
      // Compare lowercase versions
      final charA = a.codeUnitAt(i) | 0x20; // Force lowercase for letters
      final charB = b.codeUnitAt(i) | 0x20;
      diff |= charA ^ charB;
    }

    return diff == 0;
  }

  /// Constant-time comparison of two byte arrays.
  static bool constantTimeEqualBytes(List<int> a, List<int> b) {
    if (a.length != b.length) {
      return false;
    }

    var diff = 0;
    for (var i = 0; i < a.length; i++) {
      diff |= a[i] ^ b[i];
    }

    return diff == 0;
  }

  /// Format hex with spaces between bytes.
  static String formatSpaced(String hex) {
    final result = decode(hex);
    if (!result.isOk) {
      throw HexException(result.error!);
    }

    final bytes = result.value!;
    if (bytes.isEmpty) return '';

    final buffer = StringBuffer();
    for (var i = 0; i < bytes.length; i++) {
      if (i > 0) buffer.write(' ');
      buffer.write(_lowerHexChars[(bytes[i] >> 4) & 0x0F]);
      buffer.write(_lowerHexChars[bytes[i] & 0x0F]);
    }
    return buffer.toString();
  }

  /// Format hex with colons between bytes (MAC address style).
  static String formatColons(String hex) {
    final result = decode(hex);
    if (!result.isOk) {
      throw HexException(result.error!);
    }

    final bytes = result.value!;
    if (bytes.isEmpty) return '';

    final buffer = StringBuffer();
    for (var i = 0; i < bytes.length; i++) {
      if (i > 0) buffer.write(':');
      buffer.write(_lowerHexChars[(bytes[i] >> 4) & 0x0F]);
      buffer.write(_lowerHexChars[bytes[i] & 0x0F]);
    }
    return buffer.toString();
  }

  /// Format hex with dashes between bytes.
  static String formatDashed(String hex) {
    final result = decode(hex);
    if (!result.isOk) {
      throw HexException(result.error!);
    }

    final bytes = result.value!;
    if (bytes.isEmpty) return '';

    final buffer = StringBuffer();
    for (var i = 0; i < bytes.length; i++) {
      if (i > 0) buffer.write('-');
      buffer.write(_lowerHexChars[(bytes[i] >> 4) & 0x0F]);
      buffer.write(_lowerHexChars[bytes[i] & 0x0F]);
    }
    return buffer.toString();
  }

  /// Convert integer to hex string with minimum width.
  static String intToHex(int value, {int minWidth = 0, bool upperCase = false}) {
    if (value < 0) {
      throw HexException('Value must be non-negative');
    }

    var hex = value.toRadixString(16);
    if (upperCase) {
      hex = hex.toUpperCase();
    }

    if (hex.length < minWidth) {
      hex = hex.padLeft(minWidth, '0');
    }

    return hex;
  }

  /// Parse hex string to integer.
  static int? hexToInt(String hex) {
    var cleanHex = hex;
    if (cleanHex.startsWith('0x') || cleanHex.startsWith('0X')) {
      cleanHex = cleanHex.substring(2);
    }

    if (!isValid(cleanHex)) {
      return null;
    }

    return int.tryParse(cleanHex, radix: 16);
  }

  /// Parse hex string to integer, throwing on error.
  static int hexToIntStrict(String hex) {
    final result = hexToInt(hex);
    if (result == null) {
      throw HexException('Invalid hex number');
    }
    return result;
  }

  /// Add 0x prefix if not present.
  static String addPrefix(String hex) {
    if (hex.startsWith('0x') || hex.startsWith('0X')) {
      return hex;
    }
    return '0x$hex';
  }

  /// Remove 0x prefix if present.
  static String removePrefix(String hex) {
    if (hex.startsWith('0x') || hex.startsWith('0X')) {
      return hex.substring(2);
    }
    return hex;
  }

  /// Pad hex string to byte boundary (even length).
  static String padToByte(String hex) {
    final clean = removePrefix(hex);
    if (clean.length % 2 == 0) {
      return clean;
    }
    return '0$clean';
  }

  /// Reverse byte order in hex string.
  static String reverseBytes(String hex) {
    final result = decode(hex);
    if (!result.isOk) {
      throw HexException(result.error!);
    }

    final bytes = result.value!;
    final reversed = Uint8List.fromList(bytes.reversed.toList());
    return encode(reversed);
  }

  /// XOR two hex strings.
  static String xor(String a, String b) {
    final bytesA = decode(a);
    final bytesB = decode(b);

    if (!bytesA.isOk) {
      throw HexException('Invalid hex in first argument: ${bytesA.error}');
    }
    if (!bytesB.isOk) {
      throw HexException('Invalid hex in second argument: ${bytesB.error}');
    }

    final aBytes = bytesA.value!;
    final bBytes = bytesB.value!;

    if (aBytes.length != bBytes.length) {
      throw HexException('Hex strings must have same length');
    }

    final result = Uint8List(aBytes.length);
    for (var i = 0; i < aBytes.length; i++) {
      result[i] = aBytes[i] ^ bBytes[i];
    }

    return encode(result);
  }
}
