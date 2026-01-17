// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe date and time operations for Dart.
library;

/// Result of a DateTime operation.
class DateTimeResult {
  final DateTime? value;
  final String? error;

  const DateTimeResult.ok(DateTime v)
      : value = v,
        error = null;

  const DateTimeResult.error(String e)
      : value = null,
        error = e;

  bool get isOk => error == null;

  DateTime unwrap() {
    if (error != null) {
      throw DateTimeException(error!);
    }
    return value!;
  }
}

/// Exception thrown on DateTime errors.
class DateTimeException implements Exception {
  final String message;
  const DateTimeException(this.message);

  @override
  String toString() => 'DateTimeException: $message';
}

/// Time duration with named components.
class SafeDuration {
  final int days;
  final int hours;
  final int minutes;
  final int seconds;
  final int milliseconds;

  const SafeDuration({
    this.days = 0,
    this.hours = 0,
    this.minutes = 0,
    this.seconds = 0,
    this.milliseconds = 0,
  });

  /// Convert to Dart Duration.
  Duration toDuration() {
    return Duration(
      days: days,
      hours: hours,
      minutes: minutes,
      seconds: seconds,
      milliseconds: milliseconds,
    );
  }

  /// Create from Dart Duration.
  factory SafeDuration.fromDuration(Duration d) {
    final totalMs = d.inMilliseconds;
    final days = totalMs ~/ 86400000;
    final remaining = totalMs - days * 86400000;
    final hours = remaining ~/ 3600000;
    final remaining2 = remaining - hours * 3600000;
    final minutes = remaining2 ~/ 60000;
    final remaining3 = remaining2 - minutes * 60000;
    final seconds = remaining3 ~/ 1000;
    final milliseconds = remaining3 - seconds * 1000;

    return SafeDuration(
      days: days,
      hours: hours,
      minutes: minutes,
      seconds: seconds,
      milliseconds: milliseconds,
    );
  }

  /// Total milliseconds.
  int get totalMilliseconds => toDuration().inMilliseconds;

  @override
  String toString() {
    final parts = <String>[];
    if (days > 0) parts.add('${days}d');
    if (hours > 0) parts.add('${hours}h');
    if (minutes > 0) parts.add('${minutes}m');
    if (seconds > 0) parts.add('${seconds}s');
    if (milliseconds > 0) parts.add('${milliseconds}ms');
    return parts.isEmpty ? '0s' : parts.join(' ');
  }
}

/// Safe DateTime operations.
class SafeDateTime {
  /// Minimum supported year.
  static const int minYear = 1;

  /// Maximum supported year.
  static const int maxYear = 9999;

  /// Parse an ISO 8601 date string.
  static DateTimeResult parseIso8601(String dateString) {
    try {
      final dt = DateTime.parse(dateString);
      if (dt.year < minYear || dt.year > maxYear) {
        return const DateTimeResult.error('Year out of range');
      }
      return DateTimeResult.ok(dt);
    } on FormatException {
      return const DateTimeResult.error('Invalid ISO 8601 format');
    }
  }

  /// Parse a date with custom format.
  static DateTimeResult parseFormat(String dateString, String format) {
    // Simple format parsing for common patterns
    try {
      if (format == 'yyyy-MM-dd') {
        final parts = dateString.split('-');
        if (parts.length != 3) {
          return const DateTimeResult.error('Invalid date format');
        }
        final year = int.parse(parts[0]);
        final month = int.parse(parts[1]);
        final day = int.parse(parts[2]);
        return create(year: year, month: month, day: day);
      } else if (format == 'dd/MM/yyyy') {
        final parts = dateString.split('/');
        if (parts.length != 3) {
          return const DateTimeResult.error('Invalid date format');
        }
        final day = int.parse(parts[0]);
        final month = int.parse(parts[1]);
        final year = int.parse(parts[2]);
        return create(year: year, month: month, day: day);
      } else if (format == 'MM/dd/yyyy') {
        final parts = dateString.split('/');
        if (parts.length != 3) {
          return const DateTimeResult.error('Invalid date format');
        }
        final month = int.parse(parts[0]);
        final day = int.parse(parts[1]);
        final year = int.parse(parts[2]);
        return create(year: year, month: month, day: day);
      }
      return const DateTimeResult.error('Unsupported format');
    } catch (e) {
      return DateTimeResult.error('Parse error: $e');
    }
  }

  /// Create a validated DateTime.
  static DateTimeResult create({
    required int year,
    int month = 1,
    int day = 1,
    int hour = 0,
    int minute = 0,
    int second = 0,
    int millisecond = 0,
  }) {
    if (year < minYear || year > maxYear) {
      return const DateTimeResult.error('Year out of range');
    }
    if (month < 1 || month > 12) {
      return const DateTimeResult.error('Month must be 1-12');
    }
    if (day < 1 || day > daysInMonth(year, month)) {
      return const DateTimeResult.error('Invalid day for month');
    }
    if (hour < 0 || hour > 23) {
      return const DateTimeResult.error('Hour must be 0-23');
    }
    if (minute < 0 || minute > 59) {
      return const DateTimeResult.error('Minute must be 0-59');
    }
    if (second < 0 || second > 59) {
      return const DateTimeResult.error('Second must be 0-59');
    }
    if (millisecond < 0 || millisecond > 999) {
      return const DateTimeResult.error('Millisecond must be 0-999');
    }

    return DateTimeResult.ok(DateTime(year, month, day, hour, minute, second, millisecond));
  }

  /// Get the number of days in a month.
  static int daysInMonth(int year, int month) {
    const daysPerMonth = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    if (month == 2 && isLeapYear(year)) {
      return 29;
    }
    return daysPerMonth[month];
  }

  /// Check if a year is a leap year.
  static bool isLeapYear(int year) {
    return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
  }

  /// Add duration safely.
  static DateTimeResult addDuration(DateTime dt, SafeDuration duration) {
    try {
      final result = dt.add(duration.toDuration());
      if (result.year < minYear || result.year > maxYear) {
        return const DateTimeResult.error('Result year out of range');
      }
      return DateTimeResult.ok(result);
    } catch (e) {
      return DateTimeResult.error('Duration overflow: $e');
    }
  }

  /// Subtract duration safely.
  static DateTimeResult subtractDuration(DateTime dt, SafeDuration duration) {
    try {
      final result = dt.subtract(duration.toDuration());
      if (result.year < minYear || result.year > maxYear) {
        return const DateTimeResult.error('Result year out of range');
      }
      return DateTimeResult.ok(result);
    } catch (e) {
      return DateTimeResult.error('Duration overflow: $e');
    }
  }

  /// Get difference between two dates as SafeDuration.
  static SafeDuration difference(DateTime a, DateTime b) {
    return SafeDuration.fromDuration(a.difference(b));
  }

  /// Format a DateTime as ISO 8601.
  static String toIso8601(DateTime dt) {
    return dt.toIso8601String();
  }

  /// Get start of day.
  static DateTime startOfDay(DateTime dt) {
    return DateTime(dt.year, dt.month, dt.day);
  }

  /// Get end of day.
  static DateTime endOfDay(DateTime dt) {
    return DateTime(dt.year, dt.month, dt.day, 23, 59, 59, 999);
  }

  /// Get start of month.
  static DateTime startOfMonth(DateTime dt) {
    return DateTime(dt.year, dt.month, 1);
  }

  /// Get end of month.
  static DateTime endOfMonth(DateTime dt) {
    return DateTime(dt.year, dt.month, daysInMonth(dt.year, dt.month), 23, 59, 59, 999);
  }

  /// Get start of year.
  static DateTime startOfYear(DateTime dt) {
    return DateTime(dt.year, 1, 1);
  }

  /// Get end of year.
  static DateTime endOfYear(DateTime dt) {
    return DateTime(dt.year, 12, 31, 23, 59, 59, 999);
  }

  /// Check if a date is in the past.
  static bool isPast(DateTime dt) {
    return dt.isBefore(DateTime.now());
  }

  /// Check if a date is in the future.
  static bool isFuture(DateTime dt) {
    return dt.isAfter(DateTime.now());
  }

  /// Check if two dates are on the same day.
  static bool isSameDay(DateTime a, DateTime b) {
    return a.year == b.year && a.month == b.month && a.day == b.day;
  }

  /// Get the day of year (1-366).
  static int dayOfYear(DateTime dt) {
    final startOfYear = DateTime(dt.year, 1, 1);
    return dt.difference(startOfYear).inDays + 1;
  }

  /// Get the week of year (ISO 8601).
  static int weekOfYear(DateTime dt) {
    final jan1 = DateTime(dt.year, 1, 1);
    final dayOfYear = dt.difference(jan1).inDays;
    return ((dayOfYear - dt.weekday + 10) / 7).floor();
  }

  /// Get Unix timestamp in seconds.
  static int toUnixSeconds(DateTime dt) {
    return dt.millisecondsSinceEpoch ~/ 1000;
  }

  /// Get Unix timestamp in milliseconds.
  static int toUnixMillis(DateTime dt) {
    return dt.millisecondsSinceEpoch;
  }

  /// Create from Unix timestamp in seconds.
  static DateTimeResult fromUnixSeconds(int seconds) {
    try {
      final dt = DateTime.fromMillisecondsSinceEpoch(seconds * 1000);
      if (dt.year < minYear || dt.year > maxYear) {
        return const DateTimeResult.error('Year out of range');
      }
      return DateTimeResult.ok(dt);
    } catch (e) {
      return DateTimeResult.error('Invalid timestamp: $e');
    }
  }

  /// Create from Unix timestamp in milliseconds.
  static DateTimeResult fromUnixMillis(int milliseconds) {
    try {
      final dt = DateTime.fromMillisecondsSinceEpoch(milliseconds);
      if (dt.year < minYear || dt.year > maxYear) {
        return const DateTimeResult.error('Year out of range');
      }
      return DateTimeResult.ok(dt);
    } catch (e) {
      return DateTimeResult.error('Invalid timestamp: $e');
    }
  }
}
