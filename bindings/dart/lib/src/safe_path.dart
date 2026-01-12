// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe filesystem path operations for Dart with traversal protection.
library;

import 'dart:io';

/// Result of a path operation.
class PathResult {
  final String? path;
  final String? error;

  const PathResult.ok(String p)
      : path = p,
        error = null;

  const PathResult.error(String e)
      : path = null,
        error = e;

  bool get isOk => error == null;
  bool get isError => error != null;

  String unwrap() {
    if (error != null) {
      throw PathException(error!);
    }
    return path!;
  }

  String unwrapOr(String defaultPath) => path ?? defaultPath;
}

/// Exception for path-related errors.
class PathException implements Exception {
  final String message;
  const PathException(this.message);

  @override
  String toString() => 'PathException: $message';
}

/// Safe filesystem path operations.
class SafePath {
  /// Characters that are not allowed in filenames.
  static const String _dangerousChars = r'<>:"/\|?*';

  /// Check if a path contains directory traversal sequences.
  static bool hasTraversal(String path) {
    // Check for .. sequences
    if (path.contains('..')) return true;

    // Check for ~ expansion attempts
    if (path.startsWith('~')) return true;

    return false;
  }

  /// Check if a path is safe (no traversal).
  static bool isSafePath(String path) => !hasTraversal(path);

  /// Sanitize a filename by removing dangerous characters.
  static String sanitizeFilename(String filename) {
    var safe = filename;

    // Remove .. sequences
    safe = safe.replaceAll('..', '_');

    // Remove dangerous characters
    for (final char in _dangerousChars.runes) {
      safe = safe.replaceAll(String.fromCharCode(char), '_');
    }

    // Remove null bytes
    safe = safe.replaceAll('\x00', '_');

    // Remove leading/trailing dots and spaces
    safe = safe.replaceAll(RegExp(r'^[.\s]+|[.\s]+$'), '');

    return safe;
  }

  /// Safely join path components, checking for traversal.
  static PathResult join(String base, List<String> parts) {
    var path = base;

    for (final part in parts) {
      if (hasTraversal(part)) {
        return const PathResult.error('traversal_detected');
      }

      final safePart = sanitizeFilename(part);
      path = '$path${Platform.pathSeparator}$safePart';
    }

    return PathResult.ok(path);
  }

  /// Resolve path and verify it's within a base directory.
  static PathResult resolveWithin(String basePath, String userPath) {
    try {
      // Get real path of base
      final baseDir = Directory(basePath);
      if (!baseDir.existsSync()) {
        return const PathResult.error('base_not_found');
      }
      final realBase = baseDir.resolveSymbolicLinksSync();

      // Construct and resolve full path
      final fullPath = '$realBase${Platform.pathSeparator}$userPath';
      final targetFile = File(fullPath);
      final targetDir = Directory(fullPath);

      String realPath;
      if (targetFile.existsSync()) {
        realPath = targetFile.resolveSymbolicLinksSync();
      } else if (targetDir.existsSync()) {
        realPath = targetDir.resolveSymbolicLinksSync();
      } else {
        // Path doesn't exist - normalize it manually
        realPath = Uri.file(fullPath).toFilePath();
      }

      // Verify the resolved path is within base
      if (!realPath.startsWith(realBase)) {
        return const PathResult.error('path_escapes_base');
      }

      return PathResult.ok(realPath);
    } catch (e) {
      return PathResult.error('resolution_failed: $e');
    }
  }

  /// Get safe basename (strip directory components).
  static String safeBasename(String path) {
    // Get the last component
    final parts = path.split(RegExp(r'[/\\]'));
    final name = parts.isNotEmpty ? parts.last : path;

    // Sanitize it
    return sanitizeFilename(name);
  }

  /// Check if filename has an allowed extension.
  static bool hasAllowedExtension(String filename, List<String> allowedExtensions) {
    final lastDot = filename.lastIndexOf('.');
    if (lastDot == -1 || lastDot == filename.length - 1) {
      return false;
    }

    final ext = filename.substring(lastDot + 1).toLowerCase();
    return allowedExtensions.any((allowed) => allowed.toLowerCase() == ext);
  }

  /// Get file extension (lowercase).
  static String? getExtension(String filename) {
    final lastDot = filename.lastIndexOf('.');
    if (lastDot == -1 || lastDot == filename.length - 1) {
      return null;
    }
    return filename.substring(lastDot + 1).toLowerCase();
  }

  /// Check if path is absolute.
  static bool isAbsolutePath(String path) {
    if (Platform.isWindows) {
      // Windows: C:\ or \\server
      return RegExp(r'^[a-zA-Z]:\\|^\\\\').hasMatch(path);
    }
    // Unix-like: starts with /
    return path.startsWith('/');
  }

  /// Check if path exists and is readable.
  static bool isReadable(String path) {
    try {
      final file = File(path);
      if (file.existsSync()) {
        return true;
      }
      final dir = Directory(path);
      return dir.existsSync();
    } catch (_) {
      return false;
    }
  }

  /// Check if path is a regular file (not symlink, device, etc).
  static bool isRegularFile(String path) {
    try {
      final stat = FileStat.statSync(path);
      return stat.type == FileSystemEntityType.file;
    } catch (_) {
      return false;
    }
  }

  /// Check if path is a directory.
  static bool isDirectory(String path) {
    try {
      final stat = FileStat.statSync(path);
      return stat.type == FileSystemEntityType.directory;
    } catch (_) {
      return false;
    }
  }

  /// Check if path is a symbolic link.
  static bool isSymlink(String path) {
    try {
      return FileSystemEntity.isLinkSync(path);
    } catch (_) {
      return false;
    }
  }

  /// Create directory safely (checks for traversal first).
  static Future<bool> safeMkdir(String base, String name) async {
    if (hasTraversal(name)) {
      return false;
    }

    final safeName = sanitizeFilename(name);
    final fullPath = '$base${Platform.pathSeparator}$safeName';

    try {
      await Directory(fullPath).create(recursive: true);
      return true;
    } catch (_) {
      return false;
    }
  }

  /// Normalize a path by resolving . and redundant separators.
  /// Does NOT resolve .. (use resolveWithin for that).
  static String normalizePath(String path) {
    // Replace multiple separators with single
    var normalized = path.replaceAll(RegExp(r'[/\\]+'), Platform.pathSeparator);

    // Remove trailing separator (unless root)
    if (normalized.length > 1 && normalized.endsWith(Platform.pathSeparator)) {
      normalized = normalized.substring(0, normalized.length - 1);
    }

    return normalized;
  }
}
