// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe hexadecimal encoding and decoding via libproven FFI.
///
/// All computation is performed in the formally verified Idris 2 core.
library;

import 'dart:ffi';
import 'dart:typed_data';

import 'package:ffi/ffi.dart';

import 'ffi.dart';

/// Safe hexadecimal encoding and decoding.
///
/// Delegates to libproven for hex encode/decode operations.
class SafeHex {
  /// Encode bytes to lowercase hex string.
  /// Returns null on FFI error.
  static String? encode(Uint8List bytes) {
    return _encode(bytes, uppercase: false);
  }

  /// Encode bytes to uppercase hex string.
  /// Returns null on FFI error.
  static String? encodeUpper(Uint8List bytes) {
    return _encode(bytes, uppercase: true);
  }

  static String? _encode(Uint8List bytes, {required bool uppercase}) {
    if (bytes.isEmpty) return '';
    final ptr = calloc<Uint8>(bytes.length);
    try {
      for (var i = 0; i < bytes.length; i++) {
        ptr[i] = bytes[i];
      }
      final result = provenHexEncode(ptr, bytes.length, uppercase);
      return callStringFfi(result);
    } finally {
      calloc.free(ptr);
    }
  }

  /// Decode hex string to bytes.
  /// Returns null on invalid hex input or FFI error.
  static Uint8List? decode(String hex) {
    if (hex.isEmpty) return Uint8List(0);
    final (ptr, len) = toNativeUtf8Bytes(hex);
    try {
      final result = provenHexDecode(ptr, len);
      if (result.status != ProvenStatus.ok) return null;
      if (result.data == nullptr) return null;
      final bytes = Uint8List(result.length);
      for (var i = 0; i < result.length; i++) {
        bytes[i] = result.data[i];
      }
      // Free the C-allocated decode buffer
      final resultPtr = calloc<HexDecodeResult>();
      resultPtr.ref.status = result.status;
      resultPtr.ref.data = result.data;
      resultPtr.ref.length = result.length;
      provenHexFree(resultPtr);
      calloc.free(resultPtr);
      return bytes;
    } finally {
      calloc.free(ptr);
    }
  }
}
