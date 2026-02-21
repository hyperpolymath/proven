// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe JSON parsing and manipulation for Dart with depth limits.
library;

import 'dart:convert';

/// Result of a JSON operation.
class JsonResult {
  final dynamic value;
  final String? error;

  const JsonResult.ok(this.value) : error = null;

  const JsonResult.error(String e)
      : value = null,
        error = e;

  bool get isOk => error == null;

  dynamic unwrap() {
    if (error != null) {
      throw JsonException(error!);
    }
    return value;
  }

  T unwrapAs<T>() {
    if (error != null) {
      throw JsonException(error!);
    }
    if (value is! T) {
      throw JsonException('Expected ${T.toString()}, got ${value.runtimeType}');
    }
    return value as T;
  }
}

/// Exception thrown on JSON errors.
class JsonException implements Exception {
  final String message;
  const JsonException(this.message);

  @override
  String toString() => 'JsonException: $message';
}

/// Configuration for JSON parsing.
class JsonConfig {
  /// Maximum depth of nested structures.
  final int maxDepth;

  /// Maximum string length.
  final int maxStringLength;

  /// Maximum array length.
  final int maxArrayLength;

  /// Maximum number of object keys.
  final int maxObjectKeys;

  const JsonConfig({
    this.maxDepth = 64,
    this.maxStringLength = 1048576,
    this.maxArrayLength = 10000,
    this.maxObjectKeys = 1000,
  });

  static const JsonConfig standard = JsonConfig();

  static const JsonConfig strict = JsonConfig(
    maxDepth: 16,
    maxStringLength: 65536,
    maxArrayLength: 1000,
    maxObjectKeys: 100,
  );
}

/// Safe JSON operations.
class SafeJson {
  /// Parse JSON with depth and size limits.
  static JsonResult parse(String jsonString, {JsonConfig config = JsonConfig.standard}) {
    if (jsonString.length > config.maxStringLength) {
      return const JsonResult.error('JSON string exceeds maximum length');
    }

    try {
      final decoded = jsonDecode(jsonString);
      final validationError = _validateDepth(decoded, config, 0);
      if (validationError != null) {
        return JsonResult.error(validationError);
      }
      return JsonResult.ok(decoded);
    } on FormatException catch (e) {
      return JsonResult.error('Invalid JSON: ${e.message}');
    } catch (e) {
      return JsonResult.error('JSON parse error: $e');
    }
  }

  static String? _validateDepth(dynamic value, JsonConfig config, int depth) {
    if (depth > config.maxDepth) {
      return 'Maximum depth exceeded';
    }

    if (value is Map) {
      if (value.length > config.maxObjectKeys) {
        return 'Object has too many keys';
      }
      for (final entry in value.entries) {
        if (entry.key is String && (entry.key as String).length > config.maxStringLength) {
          return 'Object key exceeds maximum string length';
        }
        final error = _validateDepth(entry.value, config, depth + 1);
        if (error != null) return error;
      }
    } else if (value is List) {
      if (value.length > config.maxArrayLength) {
        return 'Array exceeds maximum length';
      }
      for (final item in value) {
        final error = _validateDepth(item, config, depth + 1);
        if (error != null) return error;
      }
    } else if (value is String) {
      if (value.length > config.maxStringLength) {
        return 'String exceeds maximum length';
      }
    }

    return null;
  }

  /// Stringify a value to JSON.
  static JsonResult stringify(dynamic value, {bool pretty = false}) {
    try {
      final encoder = pretty ? const JsonEncoder.withIndent('  ') : const JsonEncoder();
      return JsonResult.ok(encoder.convert(value));
    } catch (e) {
      return JsonResult.error('JSON stringify error: $e');
    }
  }

  /// Safely get a value from a nested path.
  static dynamic getPath(dynamic json, List<dynamic> path) {
    dynamic current = json;
    for (final key in path) {
      if (current == null) return null;
      if (current is Map && key is String) {
        current = current[key];
      } else if (current is List && key is int) {
        if (key < 0 || key >= current.length) return null;
        current = current[key];
      } else {
        return null;
      }
    }
    return current;
  }

  /// Get a string value from a path.
  static String? getString(dynamic json, List<dynamic> path) {
    final value = getPath(json, path);
    return value is String ? value : null;
  }

  /// Get an int value from a path.
  static int? getInt(dynamic json, List<dynamic> path) {
    final value = getPath(json, path);
    if (value is int) return value;
    if (value is double) return value.toInt();
    return null;
  }

  /// Get a double value from a path.
  static double? getDouble(dynamic json, List<dynamic> path) {
    final value = getPath(json, path);
    if (value is double) return value;
    if (value is int) return value.toDouble();
    return null;
  }

  /// Get a bool value from a path.
  static bool? getBool(dynamic json, List<dynamic> path) {
    final value = getPath(json, path);
    return value is bool ? value : null;
  }

  /// Get a list from a path.
  static List<dynamic>? getList(dynamic json, List<dynamic> path) {
    final value = getPath(json, path);
    return value is List ? value : null;
  }

  /// Get a map from a path.
  static Map<String, dynamic>? getMap(dynamic json, List<dynamic> path) {
    final value = getPath(json, path);
    if (value is Map) {
      return Map<String, dynamic>.from(value);
    }
    return null;
  }

  /// Merge two JSON objects.
  static Map<String, dynamic> merge(
    Map<String, dynamic> base,
    Map<String, dynamic> overlay, {
    bool deep = true,
  }) {
    final result = Map<String, dynamic>.from(base);

    for (final entry in overlay.entries) {
      if (deep && result[entry.key] is Map && entry.value is Map) {
        result[entry.key] = merge(
          Map<String, dynamic>.from(result[entry.key] as Map),
          Map<String, dynamic>.from(entry.value as Map),
          deep: true,
        );
      } else {
        result[entry.key] = entry.value;
      }
    }

    return result;
  }

  /// Check if a value is valid JSON.
  static bool isValid(String jsonString) {
    try {
      jsonDecode(jsonString);
      return true;
    } catch (e) {
      return false;
    }
  }

  /// Deep clone a JSON value.
  static dynamic clone(dynamic value) {
    if (value is Map) {
      return value.map((k, v) => MapEntry(k, clone(v)));
    } else if (value is List) {
      return value.map(clone).toList();
    }
    return value;
  }
}
