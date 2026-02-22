// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafeUnit - Unit conversion that cannot crash.
////
//// Thin FFI wrapper over libproven proven_unit_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

/// Length units (matches ProvenLengthUnit enum values).
pub type LengthUnit {
  Meters
  Kilometers
  Centimeters
  Millimeters
  Feet
  Inches
  Miles
  Yards
}

/// Temperature units (matches ProvenTempUnit enum values).
pub type TemperatureUnit {
  Celsius
  Fahrenheit
  Kelvin
}

/// Convert length between units.
/// Returns Error for NaN input.
@external(erlang, "proven_nif", "unit_convert_length")
pub fn convert_length(
  value: Float,
  from: LengthUnit,
  to: LengthUnit,
) -> Result(Float, String)

/// Convert temperature between units.
/// Returns Error for NaN input or temperatures below absolute zero.
@external(erlang, "proven_nif", "unit_convert_temp")
pub fn convert_temp(
  value: Float,
  from: TemperatureUnit,
  to: TemperatureUnit,
) -> Result(Float, String)
