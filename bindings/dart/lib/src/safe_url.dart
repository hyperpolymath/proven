// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe URL operations via libproven FFI.
///
/// URL encoding and decoding is performed in the formally verified
/// Idris 2 core.
library;

import 'dart:ffi';

import 'package:ffi/ffi.dart';

import 'ffi.dart';

/// Safe URL operations.
///
/// Delegates to libproven for URL encoding/decoding.
/// Note: URL parsing via proven_url_parse involves more complex struct
/// handling; the encode/decode operations are the primary FFI surface.
class SafeUrl {
  /// URL-encode a string.
  /// Returns null on FFI error.
  static String? encode(String value) {
    final (ptr, len) = toNativeUtf8Bytes(value);
    try {
      // Use the HTTP URL encode function from libproven
      final encodeFunc = provenLib.lookupFunction<
          StringResult Function(Pointer<Uint8>, Size),
          StringResult Function(
              Pointer<Uint8>, int)>('proven_http_url_encode');
      final result = encodeFunc(ptr, len);
      return callStringFfi(result);
    } finally {
      calloc.free(ptr);
    }
  }

  /// URL-decode a string.
  /// Returns null on FFI error or invalid input.
  static String? decode(String value) {
    final (ptr, len) = toNativeUtf8Bytes(value);
    try {
      final decodeFunc = provenLib.lookupFunction<
          StringResult Function(Pointer<Uint8>, Size),
          StringResult Function(
              Pointer<Uint8>, int)>('proven_http_url_decode');
      final result = decodeFunc(ptr, len);
      return callStringFfi(result);
    } finally {
      calloc.free(ptr);
    }
  }
}
