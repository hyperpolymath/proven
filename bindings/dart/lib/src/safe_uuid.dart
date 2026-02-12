// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe UUID generation and validation following RFC 4122.
library;

import 'dart:math';
import 'dart:typed_data';

/// UUID version types.
enum UuidVersion {
  /// Version 1: Time-based UUID.
  v1,

  /// Version 2: DCE Security UUID.
  v2,

  /// Version 3: Name-based UUID using MD5.
  v3,

  /// Version 4: Random UUID.
  v4,

  /// Version 5: Name-based UUID using SHA-1.
  v5,

  /// Nil UUID (all zeros).
  nil,
}

/// UUID variant types per RFC 4122.
enum UuidVariant {
  /// Reserved, NCS backward compatibility.
  ncs,

  /// RFC 4122 variant.
  rfc4122,

  /// Reserved, Microsoft Corporation backward compatibility.
  microsoft,

  /// Reserved for future definition.
  future,
}

/// Result of UUID parsing operation.
class UuidResult {
  final Uuid? value;
  final String? error;

  const UuidResult.ok(Uuid uuid)
      : value = uuid,
        error = null;

  const UuidResult.error(String e)
      : value = null,
        error = e;

  bool get isOk => error == null;
  bool get isError => error != null;

  /// Get UUID or throw if error.
  Uuid unwrap() {
    if (error != null) {
      throw UuidException(error!);
    }
    return value!;
  }

  /// Get UUID or return default.
  Uuid unwrapOr(Uuid defaultValue) => value ?? defaultValue;
}

/// Exception thrown for UUID errors.
class UuidException implements Exception {
  final String message;
  const UuidException(this.message);

  @override
  String toString() => 'UuidException: $message';
}

/// A validated UUID (128 bits).
class Uuid {
  final Uint8List _bytes;

  /// Create UUID from bytes.
  Uuid.fromBytes(List<int> bytes)
      : _bytes = Uint8List.fromList(bytes) {
    if (_bytes.length != 16) {
      throw UuidException('UUID must be exactly 16 bytes');
    }
  }

  /// The nil UUID (all zeros).
  static final Uuid nil = Uuid.fromBytes(Uint8List(16));

  /// DNS namespace UUID.
  static final Uuid namespaceDns = Uuid.fromBytes([
    0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
  ]);

  /// URL namespace UUID.
  static final Uuid namespaceUrl = Uuid.fromBytes([
    0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
  ]);

  /// OID namespace UUID.
  static final Uuid namespaceOid = Uuid.fromBytes([
    0x6b, 0xa7, 0xb8, 0x12, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
  ]);

  /// X.500 namespace UUID.
  static final Uuid namespaceX500 = Uuid.fromBytes([
    0x6b, 0xa7, 0xb8, 0x14, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
  ]);

  /// Get the bytes.
  Uint8List get bytes => Uint8List.fromList(_bytes);

  /// Get the UUID version.
  UuidVersion get version {
    final versionNibble = (_bytes[6] >> 4) & 0x0F;
    switch (versionNibble) {
      case 1:
        return UuidVersion.v1;
      case 2:
        return UuidVersion.v2;
      case 3:
        return UuidVersion.v3;
      case 4:
        return UuidVersion.v4;
      case 5:
        return UuidVersion.v5;
      default:
        return UuidVersion.nil;
    }
  }

  /// Get the UUID variant.
  UuidVariant get variant {
    final variantByte = _bytes[8];
    if ((variantByte >> 7) == 0) {
      return UuidVariant.ncs;
    } else if ((variantByte >> 6) == 0x02) {
      return UuidVariant.rfc4122;
    } else if ((variantByte >> 5) == 0x06) {
      return UuidVariant.microsoft;
    } else {
      return UuidVariant.future;
    }
  }

  /// Check if this is the nil UUID.
  bool get isNil => _bytes.every((b) => b == 0);

  /// Format as canonical string (lowercase).
  @override
  String toString() {
    final hex = _bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
    return '${hex.substring(0, 8)}-${hex.substring(8, 12)}-'
        '${hex.substring(12, 16)}-${hex.substring(16, 20)}-${hex.substring(20, 32)}';
  }

  /// Format as uppercase string.
  String toUpperCase() => toString().toUpperCase();

  /// Format as URN.
  String toUrn() => 'urn:uuid:$this';

  /// Format without hyphens.
  String toHex() =>
      _bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    if (other is! Uuid) return false;
    for (var i = 0; i < 16; i++) {
      if (_bytes[i] != other._bytes[i]) return false;
    }
    return true;
  }

  @override
  int get hashCode {
    var hash = 0;
    for (var i = 0; i < 16; i++) {
      hash = (hash * 31 + _bytes[i]) & 0x7FFFFFFF;
    }
    return hash;
  }
}

/// Safe UUID operations.
class SafeUuid {
  static final Random _secureRandom = Random.secure();

  /// Parse UUID from canonical string format.
  static UuidResult parse(String input) {
    final trimmed = input.trim();

    // Handle URN format
    var uuidStr = trimmed;
    if (uuidStr.toLowerCase().startsWith('urn:uuid:')) {
      uuidStr = uuidStr.substring(9);
    }

    // Handle braced format {uuid}
    if (uuidStr.startsWith('{') && uuidStr.endsWith('}')) {
      uuidStr = uuidStr.substring(1, uuidStr.length - 1);
    }

    // Validate length
    if (uuidStr.length != 36) {
      return UuidResult.error(
          'UUID must be 36 characters, got ${uuidStr.length}');
    }

    // Validate hyphen positions
    if (uuidStr[8] != '-' ||
        uuidStr[13] != '-' ||
        uuidStr[18] != '-' ||
        uuidStr[23] != '-') {
      return const UuidResult.error('Invalid UUID hyphen positions');
    }

    // Extract hex digits
    final hexStr = uuidStr.replaceAll('-', '');
    if (hexStr.length != 32) {
      return const UuidResult.error('Invalid UUID hex length');
    }

    // Parse hex bytes
    final bytes = Uint8List(16);
    for (var i = 0; i < 16; i++) {
      final byteStr = hexStr.substring(i * 2, i * 2 + 2);
      final parsed = int.tryParse(byteStr, radix: 16);
      if (parsed == null) {
        return const UuidResult.error('Invalid hex character in UUID');
      }
      bytes[i] = parsed;
    }

    return UuidResult.ok(Uuid.fromBytes(bytes));
  }

  /// Generate a v4 (random) UUID.
  static Uuid v4() {
    final bytes = Uint8List(16);
    for (var i = 0; i < 16; i++) {
      bytes[i] = _secureRandom.nextInt(256);
    }

    // Set version to 4
    bytes[6] = (bytes[6] & 0x0F) | 0x40;
    // Set variant to RFC 4122
    bytes[8] = (bytes[8] & 0x3F) | 0x80;

    return Uuid.fromBytes(bytes);
  }

  /// Generate v4 UUID from provided random bytes.
  static Uuid v4FromBytes(List<int> randomBytes) {
    if (randomBytes.length != 16) {
      throw UuidException('Random bytes must be exactly 16 bytes');
    }

    final bytes = Uint8List.fromList(randomBytes);
    // Set version to 4
    bytes[6] = (bytes[6] & 0x0F) | 0x40;
    // Set variant to RFC 4122
    bytes[8] = (bytes[8] & 0x3F) | 0x80;

    return Uuid.fromBytes(bytes);
  }

  /// Check if string is valid UUID format.
  static bool isValid(String input) {
    return parse(input).isOk;
  }

  /// Check if UUID is nil.
  static bool isNil(String input) {
    final result = parse(input);
    return result.isOk && result.value!.isNil;
  }

  /// Compare two UUID strings (case-insensitive).
  static bool areEqual(String a, String b) {
    final resultA = parse(a);
    final resultB = parse(b);

    if (!resultA.isOk || !resultB.isOk) return false;
    return resultA.value! == resultB.value!;
  }

  /// Normalize UUID string to lowercase canonical format.
  static String? normalize(String input) {
    final result = parse(input);
    return result.isOk ? result.value!.toString() : null;
  }

  /// Get version from UUID string.
  static UuidVersion? getVersion(String input) {
    final result = parse(input);
    return result.isOk ? result.value!.version : null;
  }

  /// Get variant from UUID string.
  static UuidVariant? getVariant(String input) {
    final result = parse(input);
    return result.isOk ? result.value!.variant : null;
  }
}
