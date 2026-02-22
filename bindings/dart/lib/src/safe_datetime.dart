// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe date and time operations via libproven FFI.
///
/// All computation is performed in the formally verified Idris 2 core.
library;

import 'dart:ffi';

import 'package:ffi/ffi.dart';

import 'ffi.dart';

/// Parsed date-time value.
class DateTimeValue {
  final int year;
  final int month;
  final int day;
  final int hour;
  final int minute;
  final int second;

  const DateTimeValue({
    required this.year,
    required this.month,
    required this.day,
    required this.hour,
    required this.minute,
    required this.second,
  });

  @override
  String toString() =>
      '$year-${month.toString().padLeft(2, '0')}-${day.toString().padLeft(2, '0')}T'
      '${hour.toString().padLeft(2, '0')}:${minute.toString().padLeft(2, '0')}:${second.toString().padLeft(2, '0')}';
}

/// Safe DateTime operations.
///
/// Delegates to libproven for date parsing, formatting, and calendar queries.
class SafeDateTime {
  /// Parse an ISO 8601 date string.
  /// Returns null on invalid input.
  static DateTimeValue? parseIso8601(String dateString) {
    final (ptr, len) = toNativeUtf8Bytes(dateString);
    try {
      final result = provenDatetimeParse(ptr, len);
      if (result.status != ProvenStatus.ok) return null;
      return DateTimeValue(
        year: result.datetime.year,
        month: result.datetime.month,
        day: result.datetime.day,
        hour: result.datetime.hour,
        minute: result.datetime.minute,
        second: result.datetime.second,
      );
    } finally {
      calloc.free(ptr);
    }
  }

  /// Check if a year is a leap year.
  static bool isLeapYear(int year) {
    return provenDatetimeIsLeapYear(year);
  }

  /// Get the number of days in a month.
  static int daysInMonth(int year, int month) {
    return provenDatetimeDaysInMonth(year, month);
  }
}
