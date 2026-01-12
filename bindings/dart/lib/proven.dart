// SPDX-License-Identifier: PMPL-1.0
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
library proven;

export 'src/safe_crypto.dart';
export 'src/safe_email.dart';
export 'src/safe_math.dart';
export 'src/safe_network.dart';
export 'src/safe_path.dart';
export 'src/safe_string.dart';
