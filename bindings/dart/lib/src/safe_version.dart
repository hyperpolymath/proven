// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe semantic versioning for Dart.
library;

/// Result of a version operation.
class VersionResult {
  final SemanticVersion? version;
  final String? error;

  const VersionResult.ok(SemanticVersion v)
      : version = v,
        error = null;

  const VersionResult.error(String e)
      : version = null,
        error = e;

  bool get isOk => error == null;

  SemanticVersion unwrap() {
    if (error != null) {
      throw VersionException(error!);
    }
    return version!;
  }
}

/// Exception thrown on version errors.
class VersionException implements Exception {
  final String message;
  const VersionException(this.message);

  @override
  String toString() => 'VersionException: $message';
}

/// A semantic version following SemVer 2.0.0.
class SemanticVersion implements Comparable<SemanticVersion> {
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

  /// Parse a version string.
  static VersionResult parse(String versionString) {
    var input = versionString.trim();

    // Remove leading 'v' if present
    if (input.startsWith('v') || input.startsWith('V')) {
      input = input.substring(1);
    }

    // Split build metadata
    String? buildMetadata;
    final buildIdx = input.indexOf('+');
    if (buildIdx != -1) {
      buildMetadata = input.substring(buildIdx + 1);
      input = input.substring(0, buildIdx);
      if (buildMetadata.isEmpty || !_isValidIdentifier(buildMetadata)) {
        return const VersionResult.error('Invalid build metadata');
      }
    }

    // Split prerelease
    String? preRelease;
    final preIdx = input.indexOf('-');
    if (preIdx != -1) {
      preRelease = input.substring(preIdx + 1);
      input = input.substring(0, preIdx);
      if (preRelease.isEmpty || !_isValidPreRelease(preRelease)) {
        return const VersionResult.error('Invalid pre-release');
      }
    }

    // Parse major.minor.patch
    final parts = input.split('.');
    if (parts.length != 3) {
      return const VersionResult.error('Version must have 3 parts');
    }

    final major = int.tryParse(parts[0]);
    final minor = int.tryParse(parts[1]);
    final patch = int.tryParse(parts[2]);

    if (major == null || minor == null || patch == null) {
      return const VersionResult.error('Version parts must be integers');
    }

    if (major < 0 || minor < 0 || patch < 0) {
      return const VersionResult.error('Version parts cannot be negative');
    }

    // Check for leading zeros
    if ((parts[0].length > 1 && parts[0].startsWith('0')) ||
        (parts[1].length > 1 && parts[1].startsWith('0')) ||
        (parts[2].length > 1 && parts[2].startsWith('0'))) {
      return const VersionResult.error('Version parts cannot have leading zeros');
    }

    return VersionResult.ok(SemanticVersion(
      major: major,
      minor: minor,
      patch: patch,
      preRelease: preRelease,
      buildMetadata: buildMetadata,
    ));
  }

  static bool _isValidIdentifier(String id) {
    return RegExp(r'^[0-9A-Za-z.-]+$').hasMatch(id);
  }

  static bool _isValidPreRelease(String pr) {
    for (final part in pr.split('.')) {
      if (part.isEmpty) return false;
      if (!_isValidIdentifier(part)) return false;
      // Numeric identifiers must not have leading zeros
      if (RegExp(r'^[0-9]+$').hasMatch(part) && part.length > 1 && part.startsWith('0')) {
        return false;
      }
    }
    return true;
  }

  /// Check if this is a pre-release version.
  bool get isPreRelease => preRelease != null;

  /// Check if this is a stable version (>= 1.0.0 and no pre-release).
  bool get isStable => major >= 1 && preRelease == null;

  /// Bump major version.
  SemanticVersion bumpMajor() {
    return SemanticVersion(major: major + 1, minor: 0, patch: 0);
  }

  /// Bump minor version.
  SemanticVersion bumpMinor() {
    return SemanticVersion(major: major, minor: minor + 1, patch: 0);
  }

  /// Bump patch version.
  SemanticVersion bumpPatch() {
    return SemanticVersion(major: major, minor: minor, patch: patch + 1);
  }

  /// Add pre-release tag.
  SemanticVersion withPreRelease(String pr) {
    return SemanticVersion(
      major: major,
      minor: minor,
      patch: patch,
      preRelease: pr,
      buildMetadata: buildMetadata,
    );
  }

  /// Add build metadata.
  SemanticVersion withBuildMetadata(String bm) {
    return SemanticVersion(
      major: major,
      minor: minor,
      patch: patch,
      preRelease: preRelease,
      buildMetadata: bm,
    );
  }

  /// Remove pre-release and build metadata.
  SemanticVersion toRelease() {
    return SemanticVersion(major: major, minor: minor, patch: patch);
  }

  @override
  int compareTo(SemanticVersion other) {
    // Compare major.minor.patch
    if (major != other.major) return major.compareTo(other.major);
    if (minor != other.minor) return minor.compareTo(other.minor);
    if (patch != other.patch) return patch.compareTo(other.patch);

    // Pre-release versions have lower precedence
    if (preRelease == null && other.preRelease == null) return 0;
    if (preRelease == null) return 1;
    if (other.preRelease == null) return -1;

    // Compare pre-release identifiers
    final thisParts = preRelease!.split('.');
    final otherParts = other.preRelease!.split('.');

    final maxLen = thisParts.length > otherParts.length ? thisParts.length : otherParts.length;
    for (int i = 0; i < maxLen; i++) {
      if (i >= thisParts.length) return -1;
      if (i >= otherParts.length) return 1;

      final thisNum = int.tryParse(thisParts[i]);
      final otherNum = int.tryParse(otherParts[i]);

      if (thisNum != null && otherNum != null) {
        if (thisNum != otherNum) return thisNum.compareTo(otherNum);
      } else if (thisNum != null) {
        return -1; // Numeric < alphanumeric
      } else if (otherNum != null) {
        return 1;
      } else {
        final cmp = thisParts[i].compareTo(otherParts[i]);
        if (cmp != 0) return cmp;
      }
    }

    return 0;
  }

  bool operator <(SemanticVersion other) => compareTo(other) < 0;
  bool operator <=(SemanticVersion other) => compareTo(other) <= 0;
  bool operator >(SemanticVersion other) => compareTo(other) > 0;
  bool operator >=(SemanticVersion other) => compareTo(other) >= 0;

  @override
  bool operator ==(Object other) {
    if (other is! SemanticVersion) return false;
    return compareTo(other) == 0 && buildMetadata == other.buildMetadata;
  }

  @override
  int get hashCode =>
      major.hashCode ^ minor.hashCode ^ patch.hashCode ^ preRelease.hashCode ^ buildMetadata.hashCode;

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
class SafeVersion {
  /// Parse a version string.
  static VersionResult parse(String version) => SemanticVersion.parse(version);

  /// Check if a version string is valid.
  static bool isValid(String version) => SemanticVersion.parse(version).isOk;

  /// Compare two version strings.
  static int compare(String a, String b) {
    final va = SemanticVersion.parse(a);
    final vb = SemanticVersion.parse(b);
    if (!va.isOk || !vb.isOk) {
      throw const VersionException('Invalid version string');
    }
    return va.version!.compareTo(vb.version!);
  }

  /// Check if version a is greater than version b.
  static bool isGreater(String a, String b) => compare(a, b) > 0;

  /// Check if version a is less than version b.
  static bool isLess(String a, String b) => compare(a, b) < 0;

  /// Check if two versions are equal.
  static bool isEqual(String a, String b) => compare(a, b) == 0;

  /// Check if a version satisfies a range (simple caret/tilde ranges).
  static bool satisfies(String version, String range) {
    final v = SemanticVersion.parse(version);
    if (!v.isOk) return false;

    var rangeStr = range.trim();

    // Caret range: ^1.2.3 allows >= 1.2.3 and < 2.0.0
    if (rangeStr.startsWith('^')) {
      final r = SemanticVersion.parse(rangeStr.substring(1));
      if (!r.isOk) return false;
      return v.version! >= r.version! && v.version!.major == r.version!.major;
    }

    // Tilde range: ~1.2.3 allows >= 1.2.3 and < 1.3.0
    if (rangeStr.startsWith('~')) {
      final r = SemanticVersion.parse(rangeStr.substring(1));
      if (!r.isOk) return false;
      return v.version! >= r.version! &&
          v.version!.major == r.version!.major &&
          v.version!.minor == r.version!.minor;
    }

    // Exact match
    final r = SemanticVersion.parse(rangeStr);
    if (!r.isOk) return false;
    return v.version!.compareTo(r.version!) == 0;
  }
}
