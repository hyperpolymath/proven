// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Proven - A safety library for Dart
///
/// Provides safe, validated operations for common programming tasks:
/// - [SafeMath] - Overflow-checked arithmetic
/// - [SafeString] - XSS prevention and string sanitization
/// - [SafePath] - Directory traversal protection
/// - [SafeEmail] - Email validation
/// - [SafeNetwork] - IP address validation
/// - [SafeCrypto] - Cryptographic operations
/// - [SafeUuid] - UUID generation and validation (RFC 4122)
/// - [SafeCurrency] - Type-safe monetary values (ISO 4217)
/// - [SafePhone] - Phone number validation (E.164)
/// - [SafeHex] - Hexadecimal encoding and decoding
library proven;

export 'src/safe_crypto.dart';
export 'src/safe_currency.dart';
export 'src/safe_email.dart';
export 'src/safe_hex.dart';
export 'src/safe_math.dart';
export 'src/safe_network.dart';
export 'src/safe_path.dart';
export 'src/safe_phone.dart';
export 'src/safe_string.dart';
export 'src/safe_uuid.dart';
