// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe physical unit conversions via libproven FFI.
///
/// All computation is performed in the formally verified Idris 2 core.
library;

import 'ffi.dart';

/// Length units (must match C ABI enum order).
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

/// Temperature units (must match C ABI enum order).
enum TemperatureUnit {
  celsius,
  fahrenheit,
  kelvin,
}

/// Safe unit conversion operations.
///
/// Delegates to libproven for length and temperature conversions.
class SafeUnit {
  /// Convert length between units.
  /// Returns null on FFI error.
  static double? convertLength(
      double value, LengthUnit from, LengthUnit to) {
    final result =
        provenUnitConvertLength(value, from.index, to.index);
    if (result.status != ProvenStatus.ok) return null;
    return result.value;
  }

  /// Convert temperature between units.
  /// Returns null on FFI error.
  static double? convertTemperature(
      double value, TemperatureUnit from, TemperatureUnit to) {
    final result =
        provenUnitConvertTemp(value, from.index, to.index);
    if (result.status != ProvenStatus.ok) return null;
    return result.value;
  }
}
