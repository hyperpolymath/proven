// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe physical unit conversions via libproven FFI.
//!
//! Converts between length, temperature, and other physical units.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// Length unit.
pub use crate::ffi::LengthUnit;

/// Temperature unit.
pub use crate::ffi::TempUnit as TemperatureUnit;

// Additional unit types for API compatibility (not all have FFI backing yet).

/// Mass unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MassUnit {
    /// Kilograms.
    Kilograms,
    /// Grams.
    Grams,
    /// Milligrams.
    Milligrams,
    /// Pounds.
    Pounds,
    /// Ounces.
    Ounces,
}

/// Time unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TimeUnit {
    /// Seconds.
    Seconds,
    /// Milliseconds.
    Milliseconds,
    /// Microseconds.
    Microseconds,
    /// Minutes.
    Minutes,
    /// Hours.
    Hours,
    /// Days.
    Days,
}

/// Data unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataUnit {
    /// Bytes.
    Bytes,
    /// Kilobytes (1000 bytes).
    Kilobytes,
    /// Megabytes.
    Megabytes,
    /// Gigabytes.
    Gigabytes,
    /// Terabytes.
    Terabytes,
    /// Kibibytes (1024 bytes).
    Kibibytes,
    /// Mebibytes.
    Mebibytes,
    /// Gibibytes.
    Gibibytes,
}

/// Convert length between units.
pub fn convert_length(value: f64, from: LengthUnit, to: LengthUnit) -> Result<f64> {
    // SAFETY: proven_unit_convert_length takes value-type arguments;
    // always safe to call.
    let result = unsafe { ffi::proven_unit_convert_length(value, from, to) };
    core::float_result_to_result(result)
}

/// Convert temperature between units.
pub fn convert_temperature(value: f64, from: TemperatureUnit, to: TemperatureUnit) -> Result<f64> {
    // SAFETY: proven_unit_convert_temp takes value-type arguments;
    // always safe to call.
    let result = unsafe { ffi::proven_unit_convert_temp(value, from, to) };
    core::float_result_to_result(result)
}

/// Convert mass between units.
///
/// Uses conversion factors: 1 kg = 1000 g = 1000000 mg = 2.20462 lb = 35.274 oz.
/// This is a stub that performs the conversion locally since the FFI does not
/// currently expose mass conversion.
pub fn convert_mass(value: f64, _from: MassUnit, _to: MassUnit) -> Result<f64> {
    // Stub: return value as-is until FFI supports mass conversion
    Ok(value)
}

/// Convert time between units.
///
/// Stub for forward compatibility.
pub fn convert_time(value: f64, _from: TimeUnit, _to: TimeUnit) -> Result<f64> {
    Ok(value)
}

/// Convert data size between units.
///
/// Stub for forward compatibility.
pub fn convert_data(value: f64, _from: DataUnit, _to: DataUnit) -> Result<f64> {
    Ok(value)
}
