// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Proven - Dart FFI bindings to libproven (formally verified safety library).
///
/// All computation is performed in Idris 2 with dependent types and totality
/// checking. This package is a thin wrapper that marshals arguments across the
/// C ABI boundary via dart:ffi.
///
/// Requires libproven shared library (libproven.so / libproven.dylib /
/// proven.dll) to be available on the library load path.
///
/// Modules:
/// - [SafeMath] - Overflow-checked arithmetic
/// - [SafeString] - XSS prevention and string escaping
/// - [SafeFloat] - NaN/Infinity-safe floating-point operations
/// - [SafePath] - Directory traversal protection
/// - [SafeEmail] - Email validation
/// - [SafeNetwork] - IP address parsing and classification
/// - [SafeCrypto] - Constant-time comparison, secure random bytes
/// - [SafeHex] - Hexadecimal encoding and decoding
/// - [SafeUuid] - UUID v4 generation and validation (RFC 4122)
/// - [SafeJson] - JSON validation
/// - [SafeColor] - Color parsing and conversion
/// - [SafeAngle] - Angle unit conversion and normalization
/// - [SafeUnit] - Physical unit conversions
/// - [SafeCurrency] - Currency parsing (ISO 4217)
/// - [SafePhone] - Phone number parsing (E.164)
/// - [SafeDateTime] - Date/time parsing and calendar queries
/// - [SafeVersion] - Semantic version parsing (SemVer 2.0.0)
/// - [SafeUrl] - URL encoding and decoding
library proven;

export 'src/ffi.dart' show ProvenStatus;
export 'src/safe_angle.dart';
export 'src/safe_color.dart';
export 'src/safe_crypto.dart';
export 'src/safe_currency.dart';
export 'src/safe_datetime.dart';
export 'src/safe_email.dart';
export 'src/safe_float.dart';
export 'src/safe_hex.dart';
export 'src/safe_json.dart';
export 'src/safe_math.dart';
export 'src/safe_network.dart';
export 'src/safe_path.dart';
export 'src/safe_phone.dart';
export 'src/safe_string.dart';
export 'src/safe_unit.dart';
export 'src/safe_url.dart';
export 'src/safe_uuid.dart';
export 'src/safe_version.dart';
