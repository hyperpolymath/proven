// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe network operations via libproven FFI.
///
/// All IP parsing and classification is performed in the formally verified
/// Idris 2 core.
library;

import 'dart:ffi';

import 'package:ffi/ffi.dart';

import 'ffi.dart';

/// Classification of IP addresses.
enum IpClassification {
  loopback,
  private_,
  public_,
  invalid,
}

/// Parsed IPv4 address result.
class ParsedIPv4 {
  final int a;
  final int b;
  final int c;
  final int d;

  const ParsedIPv4(this.a, this.b, this.c, this.d);

  @override
  String toString() => '$a.$b.$c.$d';
}

/// Safe network validation and operations.
///
/// Delegates to libproven for IP address parsing and classification.
class SafeNetwork {
  /// Parse an IPv4 address string.
  /// Returns null if the address is invalid.
  static ParsedIPv4? parseIPv4(String address) {
    final (ptr, len) = toNativeUtf8Bytes(address);
    try {
      final result = provenNetworkParseIPv4(ptr, len);
      if (result.status != ProvenStatus.ok) return null;
      return ParsedIPv4(
        result.address.a,
        result.address.b,
        result.address.c,
        result.address.d,
      );
    } finally {
      calloc.free(ptr);
    }
  }

  /// Check if string is a valid IPv4 address.
  static bool isValidIPv4(String address) {
    return parseIPv4(address) != null;
  }

  /// Classify an IPv4 address.
  static IpClassification classifyIPv4(String address) {
    final (ptr, len) = toNativeUtf8Bytes(address);
    try {
      final parseResult = provenNetworkParseIPv4(ptr, len);
      if (parseResult.status != ProvenStatus.ok) {
        return IpClassification.invalid;
      }
      final addr = parseResult.address;
      if (provenNetworkIPv4IsLoopback(addr)) {
        return IpClassification.loopback;
      }
      if (provenNetworkIPv4IsPrivate(addr)) {
        return IpClassification.private_;
      }
      return IpClassification.public_;
    } finally {
      calloc.free(ptr);
    }
  }
}
