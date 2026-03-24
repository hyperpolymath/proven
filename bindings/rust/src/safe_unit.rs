// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

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
/// Not yet available via FFI; returns `Error::NotImplemented`.
/// Will use verified Idris 2 conversion factors once the FFI export exists.
pub fn convert_mass(_value: f64, _from: MassUnit, _to: MassUnit) -> Result<f64> {
    Err(core::Error::NotImplemented)
}

/// Convert time between units.
///
/// Not yet available via FFI; returns `Error::NotImplemented`.
pub fn convert_time(_value: f64, _from: TimeUnit, _to: TimeUnit) -> Result<f64> {
    Err(core::Error::NotImplemented)
}

/// Convert data size between units.
///
/// Not yet available via FFI; returns `Error::NotImplemented`.
pub fn convert_data(_value: f64, _from: DataUnit, _to: DataUnit) -> Result<f64> {
    Err(core::Error::NotImplemented)
}
