// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe physical unit conversions for Dart.
library;

/// Length units.
enum LengthUnit {
  meters,
  kilometers,
  centimeters,
  millimeters,
  miles,
  yards,
  feet,
  inches,
}

/// Mass units.
enum MassUnit {
  kilograms,
  grams,
  milligrams,
  pounds,
  ounces,
  stones,
}

/// Temperature units.
enum TemperatureUnit {
  celsius,
  fahrenheit,
  kelvin,
}

/// Time units.
enum TimeUnit {
  seconds,
  milliseconds,
  microseconds,
  nanoseconds,
  minutes,
  hours,
  days,
  weeks,
}

/// Data size units.
enum DataUnit {
  bytes,
  kilobytes,
  megabytes,
  gigabytes,
  terabytes,
  kibibytes,
  mebibytes,
  gibibytes,
  tebibytes,
}

/// Safe unit conversion operations.
class SafeUnit {
  // ========== Length ==========

  /// Convert length to meters (base unit).
  static double lengthToMeters(double value, LengthUnit from) {
    switch (from) {
      case LengthUnit.meters:
        return value;
      case LengthUnit.kilometers:
        return value * 1000.0;
      case LengthUnit.centimeters:
        return value / 100.0;
      case LengthUnit.millimeters:
        return value / 1000.0;
      case LengthUnit.miles:
        return value * 1609.344;
      case LengthUnit.yards:
        return value * 0.9144;
      case LengthUnit.feet:
        return value * 0.3048;
      case LengthUnit.inches:
        return value * 0.0254;
    }
  }

  /// Convert length from meters.
  static double lengthFromMeters(double meters, LengthUnit to) {
    switch (to) {
      case LengthUnit.meters:
        return meters;
      case LengthUnit.kilometers:
        return meters / 1000.0;
      case LengthUnit.centimeters:
        return meters * 100.0;
      case LengthUnit.millimeters:
        return meters * 1000.0;
      case LengthUnit.miles:
        return meters / 1609.344;
      case LengthUnit.yards:
        return meters / 0.9144;
      case LengthUnit.feet:
        return meters / 0.3048;
      case LengthUnit.inches:
        return meters / 0.0254;
    }
  }

  /// Convert length between units.
  static double convertLength(double value, LengthUnit from, LengthUnit to) {
    final meters = lengthToMeters(value, from);
    return lengthFromMeters(meters, to);
  }

  // ========== Mass ==========

  /// Convert mass to kilograms (base unit).
  static double massToKilograms(double value, MassUnit from) {
    switch (from) {
      case MassUnit.kilograms:
        return value;
      case MassUnit.grams:
        return value / 1000.0;
      case MassUnit.milligrams:
        return value / 1000000.0;
      case MassUnit.pounds:
        return value * 0.453592;
      case MassUnit.ounces:
        return value * 0.0283495;
      case MassUnit.stones:
        return value * 6.35029;
    }
  }

  /// Convert mass from kilograms.
  static double massFromKilograms(double kg, MassUnit to) {
    switch (to) {
      case MassUnit.kilograms:
        return kg;
      case MassUnit.grams:
        return kg * 1000.0;
      case MassUnit.milligrams:
        return kg * 1000000.0;
      case MassUnit.pounds:
        return kg / 0.453592;
      case MassUnit.ounces:
        return kg / 0.0283495;
      case MassUnit.stones:
        return kg / 6.35029;
    }
  }

  /// Convert mass between units.
  static double convertMass(double value, MassUnit from, MassUnit to) {
    final kg = massToKilograms(value, from);
    return massFromKilograms(kg, to);
  }

  // ========== Temperature ==========

  /// Convert temperature between units.
  static double convertTemperature(double value, TemperatureUnit from, TemperatureUnit to) {
    // Convert to Kelvin first
    double kelvin;
    switch (from) {
      case TemperatureUnit.celsius:
        kelvin = value + 273.15;
        break;
      case TemperatureUnit.fahrenheit:
        kelvin = (value - 32.0) * 5.0 / 9.0 + 273.15;
        break;
      case TemperatureUnit.kelvin:
        kelvin = value;
        break;
    }

    // Convert from Kelvin
    switch (to) {
      case TemperatureUnit.celsius:
        return kelvin - 273.15;
      case TemperatureUnit.fahrenheit:
        return (kelvin - 273.15) * 9.0 / 5.0 + 32.0;
      case TemperatureUnit.kelvin:
        return kelvin;
    }
  }

  // ========== Time ==========

  /// Convert time to seconds (base unit).
  static double timeToSeconds(double value, TimeUnit from) {
    switch (from) {
      case TimeUnit.seconds:
        return value;
      case TimeUnit.milliseconds:
        return value / 1000.0;
      case TimeUnit.microseconds:
        return value / 1000000.0;
      case TimeUnit.nanoseconds:
        return value / 1000000000.0;
      case TimeUnit.minutes:
        return value * 60.0;
      case TimeUnit.hours:
        return value * 3600.0;
      case TimeUnit.days:
        return value * 86400.0;
      case TimeUnit.weeks:
        return value * 604800.0;
    }
  }

  /// Convert time from seconds.
  static double timeFromSeconds(double secs, TimeUnit to) {
    switch (to) {
      case TimeUnit.seconds:
        return secs;
      case TimeUnit.milliseconds:
        return secs * 1000.0;
      case TimeUnit.microseconds:
        return secs * 1000000.0;
      case TimeUnit.nanoseconds:
        return secs * 1000000000.0;
      case TimeUnit.minutes:
        return secs / 60.0;
      case TimeUnit.hours:
        return secs / 3600.0;
      case TimeUnit.days:
        return secs / 86400.0;
      case TimeUnit.weeks:
        return secs / 604800.0;
    }
  }

  /// Convert time between units.
  static double convertTime(double value, TimeUnit from, TimeUnit to) {
    final secs = timeToSeconds(value, from);
    return timeFromSeconds(secs, to);
  }

  // ========== Data Size ==========

  /// Convert data size to bytes (base unit).
  static double dataToBytes(double value, DataUnit from) {
    switch (from) {
      case DataUnit.bytes:
        return value;
      case DataUnit.kilobytes:
        return value * 1000.0;
      case DataUnit.megabytes:
        return value * 1000000.0;
      case DataUnit.gigabytes:
        return value * 1000000000.0;
      case DataUnit.terabytes:
        return value * 1000000000000.0;
      case DataUnit.kibibytes:
        return value * 1024.0;
      case DataUnit.mebibytes:
        return value * 1048576.0;
      case DataUnit.gibibytes:
        return value * 1073741824.0;
      case DataUnit.tebibytes:
        return value * 1099511627776.0;
    }
  }

  /// Convert data size from bytes.
  static double dataFromBytes(double bytes, DataUnit to) {
    switch (to) {
      case DataUnit.bytes:
        return bytes;
      case DataUnit.kilobytes:
        return bytes / 1000.0;
      case DataUnit.megabytes:
        return bytes / 1000000.0;
      case DataUnit.gigabytes:
        return bytes / 1000000000.0;
      case DataUnit.terabytes:
        return bytes / 1000000000000.0;
      case DataUnit.kibibytes:
        return bytes / 1024.0;
      case DataUnit.mebibytes:
        return bytes / 1048576.0;
      case DataUnit.gibibytes:
        return bytes / 1073741824.0;
      case DataUnit.tebibytes:
        return bytes / 1099511627776.0;
    }
  }

  /// Convert data size between units.
  static double convertData(double value, DataUnit from, DataUnit to) {
    final bytes = dataToBytes(value, from);
    return dataFromBytes(bytes, to);
  }

  /// Format bytes as human-readable string.
  static String formatBytes(double bytes, {bool binary = false}) {
    final units = binary
        ? ['B', 'KiB', 'MiB', 'GiB', 'TiB', 'PiB']
        : ['B', 'KB', 'MB', 'GB', 'TB', 'PB'];
    final base = binary ? 1024.0 : 1000.0;

    if (bytes < base) {
      return '${bytes.toStringAsFixed(0)} ${units[0]}';
    }

    var unitIndex = 0;
    var size = bytes;
    while (size >= base && unitIndex < units.length - 1) {
      size /= base;
      unitIndex++;
    }

    return '${size.toStringAsFixed(2)} ${units[unitIndex]}';
  }
}
