// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe semantic versioning via libproven FFI.
///
/// All parsing and comparison is performed in the formally verified
/// Idris 2 core. Follows SemVer 2.0.0.
library;

import 'dart:ffi';

import 'package:ffi/ffi.dart';

import 'ffi.dart';

/// A parsed semantic version.
class SemanticVersion {
  final int major;
  final int minor;
  final int patch;
  final String? preRelease;
  final String? buildMetadata;

  const SemanticVersion({
    required this.major,
    required this.minor,
    required this.patch,
    this.preRelease,
    this.buildMetadata,
  });

  @override
  String toString() {
    final buffer = StringBuffer('$major.$minor.$patch');
    if (preRelease != null) {
      buffer.write('-$preRelease');
    }
    if (buildMetadata != null) {
      buffer.write('+$buildMetadata');
    }
    return buffer.toString();
  }
}

/// Safe version operations.
///
/// Delegates to libproven for semantic version parsing and comparison.
class SafeVersion {
  /// Parse a version string.
  /// Returns null on invalid input.
  static SemanticVersion? parse(String version) {
    final (ptr, len) = toNativeUtf8Bytes(version);
    try {
      final result = provenVersionParse(ptr, len);
      if (result.status != ProvenStatus.ok) return null;
      final ver = result.version;

      String? preRelease;
      if (ver.preRelease != nullptr && ver.preReleaseLen > 0) {
        preRelease = ver.preRelease.toDartString(length: ver.preReleaseLen);
      }

      String? buildMeta;
      if (ver.buildMetadata != nullptr && ver.buildMetadataLen > 0) {
        buildMeta =
            ver.buildMetadata.toDartString(length: ver.buildMetadataLen);
      }

      return SemanticVersion(
        major: ver.major,
        minor: ver.minor,
        patch: ver.patch,
        preRelease: preRelease,
        buildMetadata: buildMeta,
      );
    } finally {
      calloc.free(ptr);
    }
  }

  /// Check if a version string is valid.
  static bool isValid(String version) {
    return parse(version) != null;
  }
}
