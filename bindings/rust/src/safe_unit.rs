// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe physical unit conversions.
//!
//! Provides type-safe unit conversions for length, mass,
//! temperature, time, and other physical quantities.

use crate::{Error, Result};

/// Length units.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LengthUnit {
    Meters,
    Kilometers,
    Centimeters,
    Millimeters,
    Miles,
    Yards,
    Feet,
    Inches,
}

impl LengthUnit {
    /// Convert to meters (base unit).
    pub fn to_meters(&self, value: f64) -> f64 {
        match self {
            LengthUnit::Meters => value,
            LengthUnit::Kilometers => value * 1000.0,
            LengthUnit::Centimeters => value / 100.0,
            LengthUnit::Millimeters => value / 1000.0,
            LengthUnit::Miles => value * 1609.344,
            LengthUnit::Yards => value * 0.9144,
            LengthUnit::Feet => value * 0.3048,
            LengthUnit::Inches => value * 0.0254,
        }
    }

    /// Convert from meters.
    pub fn from_meters(&self, meters: f64) -> f64 {
        match self {
            LengthUnit::Meters => meters,
            LengthUnit::Kilometers => meters / 1000.0,
            LengthUnit::Centimeters => meters * 100.0,
            LengthUnit::Millimeters => meters * 1000.0,
            LengthUnit::Miles => meters / 1609.344,
            LengthUnit::Yards => meters / 0.9144,
            LengthUnit::Feet => meters / 0.3048,
            LengthUnit::Inches => meters / 0.0254,
        }
    }
}

/// Convert length between units.
pub fn convert_length(value: f64, from: LengthUnit, to: LengthUnit) -> f64 {
    let meters = from.to_meters(value);
    to.from_meters(meters)
}

/// Mass units.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MassUnit {
    Kilograms,
    Grams,
    Milligrams,
    Pounds,
    Ounces,
    Stones,
}

impl MassUnit {
    /// Convert to kilograms (base unit).
    pub fn to_kilograms(&self, value: f64) -> f64 {
        match self {
            MassUnit::Kilograms => value,
            MassUnit::Grams => value / 1000.0,
            MassUnit::Milligrams => value / 1_000_000.0,
            MassUnit::Pounds => value * 0.453592,
            MassUnit::Ounces => value * 0.0283495,
            MassUnit::Stones => value * 6.35029,
        }
    }

    /// Convert from kilograms.
    pub fn from_kilograms(&self, kg: f64) -> f64 {
        match self {
            MassUnit::Kilograms => kg,
            MassUnit::Grams => kg * 1000.0,
            MassUnit::Milligrams => kg * 1_000_000.0,
            MassUnit::Pounds => kg / 0.453592,
            MassUnit::Ounces => kg / 0.0283495,
            MassUnit::Stones => kg / 6.35029,
        }
    }
}

/// Convert mass between units.
pub fn convert_mass(value: f64, from: MassUnit, to: MassUnit) -> f64 {
    let kg = from.to_kilograms(value);
    to.from_kilograms(kg)
}

/// Temperature units.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TemperatureUnit {
    Celsius,
    Fahrenheit,
    Kelvin,
}

/// Convert temperature between units.
pub fn convert_temperature(value: f64, from: TemperatureUnit, to: TemperatureUnit) -> f64 {
    // Convert to Kelvin first
    let kelvin = match from {
        TemperatureUnit::Celsius => value + 273.15,
        TemperatureUnit::Fahrenheit => (value - 32.0) * 5.0 / 9.0 + 273.15,
        TemperatureUnit::Kelvin => value,
    };

    // Convert from Kelvin
    match to {
        TemperatureUnit::Celsius => kelvin - 273.15,
        TemperatureUnit::Fahrenheit => (kelvin - 273.15) * 9.0 / 5.0 + 32.0,
        TemperatureUnit::Kelvin => kelvin,
    }
}

/// Time units.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TimeUnit {
    Seconds,
    Milliseconds,
    Microseconds,
    Nanoseconds,
    Minutes,
    Hours,
    Days,
    Weeks,
}

impl TimeUnit {
    /// Convert to seconds (base unit).
    pub fn to_seconds(&self, value: f64) -> f64 {
        match self {
            TimeUnit::Seconds => value,
            TimeUnit::Milliseconds => value / 1000.0,
            TimeUnit::Microseconds => value / 1_000_000.0,
            TimeUnit::Nanoseconds => value / 1_000_000_000.0,
            TimeUnit::Minutes => value * 60.0,
            TimeUnit::Hours => value * 3600.0,
            TimeUnit::Days => value * 86400.0,
            TimeUnit::Weeks => value * 604800.0,
        }
    }

    /// Convert from seconds.
    pub fn from_seconds(&self, secs: f64) -> f64 {
        match self {
            TimeUnit::Seconds => secs,
            TimeUnit::Milliseconds => secs * 1000.0,
            TimeUnit::Microseconds => secs * 1_000_000.0,
            TimeUnit::Nanoseconds => secs * 1_000_000_000.0,
            TimeUnit::Minutes => secs / 60.0,
            TimeUnit::Hours => secs / 3600.0,
            TimeUnit::Days => secs / 86400.0,
            TimeUnit::Weeks => secs / 604800.0,
        }
    }
}

/// Convert time between units.
pub fn convert_time(value: f64, from: TimeUnit, to: TimeUnit) -> f64 {
    let secs = from.to_seconds(value);
    to.from_seconds(secs)
}

/// Data size units.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataUnit {
    Bytes,
    Kilobytes,
    Megabytes,
    Gigabytes,
    Terabytes,
    Kibibytes,
    Mebibytes,
    Gibibytes,
    Tebibytes,
}

impl DataUnit {
    /// Convert to bytes.
    pub fn to_bytes(&self, value: f64) -> f64 {
        match self {
            DataUnit::Bytes => value,
            DataUnit::Kilobytes => value * 1000.0,
            DataUnit::Megabytes => value * 1_000_000.0,
            DataUnit::Gigabytes => value * 1_000_000_000.0,
            DataUnit::Terabytes => value * 1_000_000_000_000.0,
            DataUnit::Kibibytes => value * 1024.0,
            DataUnit::Mebibytes => value * 1_048_576.0,
            DataUnit::Gibibytes => value * 1_073_741_824.0,
            DataUnit::Tebibytes => value * 1_099_511_627_776.0,
        }
    }

    /// Convert from bytes.
    pub fn from_bytes(&self, bytes: f64) -> f64 {
        match self {
            DataUnit::Bytes => bytes,
            DataUnit::Kilobytes => bytes / 1000.0,
            DataUnit::Megabytes => bytes / 1_000_000.0,
            DataUnit::Gigabytes => bytes / 1_000_000_000.0,
            DataUnit::Terabytes => bytes / 1_000_000_000_000.0,
            DataUnit::Kibibytes => bytes / 1024.0,
            DataUnit::Mebibytes => bytes / 1_048_576.0,
            DataUnit::Gibibytes => bytes / 1_073_741_824.0,
            DataUnit::Tebibytes => bytes / 1_099_511_627_776.0,
        }
    }
}

/// Convert data size between units.
pub fn convert_data(value: f64, from: DataUnit, to: DataUnit) -> f64 {
    let bytes = from.to_bytes(value);
    to.from_bytes(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_length() {
        let meters = convert_length(1.0, LengthUnit::Kilometers, LengthUnit::Meters);
        assert!((meters - 1000.0).abs() < 0.001);

        let miles = convert_length(1609.344, LengthUnit::Meters, LengthUnit::Miles);
        assert!((miles - 1.0).abs() < 0.001);
    }

    #[test]
    fn test_temperature() {
        let f = convert_temperature(0.0, TemperatureUnit::Celsius, TemperatureUnit::Fahrenheit);
        assert!((f - 32.0).abs() < 0.001);

        let c = convert_temperature(212.0, TemperatureUnit::Fahrenheit, TemperatureUnit::Celsius);
        assert!((c - 100.0).abs() < 0.001);
    }

    #[test]
    fn test_data() {
        let mb = convert_data(1.0, DataUnit::Gigabytes, DataUnit::Megabytes);
        assert!((mb - 1000.0).abs() < 0.001);

        let gib = convert_data(1024.0, DataUnit::Mebibytes, DataUnit::Gibibytes);
        assert!((gib - 1.0).abs() < 0.001);
    }
}
