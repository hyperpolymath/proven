// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafeUnit - Unit conversion that cannot crash.
////
//// Provides safe unit conversions for length, mass, temperature, and more.

import gleam/float
import gleam/int
import gleam/order.{type Order}

/// Length units.
pub type LengthUnit {
  Meter
  Kilometer
  Centimeter
  Millimeter
  Micrometer
  Nanometer
  Mile
  Yard
  Foot
  Inch
  NauticalMile
}

/// Mass units.
pub type MassUnit {
  Kilogram
  Gram
  Milligram
  Microgram
  MetricTon
  Pound
  Ounce
  Stone
}

/// Temperature units.
pub type TemperatureUnit {
  Celsius
  Fahrenheit
  Kelvin
}

/// Time units.
pub type TimeUnit {
  Second
  Millisecond
  Microsecond
  Nanosecond
  Minute
  Hour
  Day
  Week
}

/// Data size units.
pub type DataUnit {
  Byte
  Kilobyte
  Megabyte
  Gigabyte
  Terabyte
  Petabyte
  Kibibyte
  Mebibyte
  Gibibyte
  Tebibyte
}

/// A length value with unit.
pub opaque type Length {
  Length(meters: Float)
}

/// A mass value with unit.
pub opaque type Mass {
  Mass(kilograms: Float)
}

/// A temperature value.
pub opaque type Temperature {
  Temperature(kelvin: Float)
}

/// A time duration.
pub opaque type TimeDuration {
  TimeDuration(seconds: Float)
}

/// A data size.
pub opaque type DataSize {
  DataSize(bytes: Int)
}

/// Error type for unit operations.
pub type UnitError {
  InvalidValue(message: String)
  ConversionError(message: String)
}

// Length operations

/// Create a length from a value and unit.
pub fn length(value: Float, unit: LengthUnit) -> Length {
  Length(meters: value *. length_to_meters_factor(unit))
}

fn length_to_meters_factor(unit: LengthUnit) -> Float {
  case unit {
    Meter -> 1.0
    Kilometer -> 1000.0
    Centimeter -> 0.01
    Millimeter -> 0.001
    Micrometer -> 0.000001
    Nanometer -> 0.000000001
    Mile -> 1609.344
    Yard -> 0.9144
    Foot -> 0.3048
    Inch -> 0.0254
    NauticalMile -> 1852.0
  }
}

/// Get a length in a specific unit.
pub fn length_in(len: Length, unit: LengthUnit) -> Float {
  len.meters /. length_to_meters_factor(unit)
}

/// Add two lengths.
pub fn length_add(len_a: Length, len_b: Length) -> Length {
  Length(meters: len_a.meters +. len_b.meters)
}

/// Subtract two lengths.
pub fn length_subtract(len_a: Length, len_b: Length) -> Length {
  Length(meters: len_a.meters -. len_b.meters)
}

/// Multiply a length by a scalar.
pub fn length_multiply(len: Length, scalar: Float) -> Length {
  Length(meters: len.meters *. scalar)
}

/// Compare two lengths.
pub fn length_compare(len_a: Length, len_b: Length) -> Order {
  float.compare(len_a.meters, len_b.meters)
}

// Mass operations

/// Create a mass from a value and unit.
pub fn mass(value: Float, unit: MassUnit) -> Mass {
  Mass(kilograms: value *. mass_to_kg_factor(unit))
}

fn mass_to_kg_factor(unit: MassUnit) -> Float {
  case unit {
    Kilogram -> 1.0
    Gram -> 0.001
    Milligram -> 0.000001
    Microgram -> 0.000000001
    MetricTon -> 1000.0
    Pound -> 0.45359237
    Ounce -> 0.028349523125
    Stone -> 6.35029318
  }
}

/// Get a mass in a specific unit.
pub fn mass_in(m: Mass, unit: MassUnit) -> Float {
  m.kilograms /. mass_to_kg_factor(unit)
}

/// Add two masses.
pub fn mass_add(mass_a: Mass, mass_b: Mass) -> Mass {
  Mass(kilograms: mass_a.kilograms +. mass_b.kilograms)
}

/// Subtract two masses.
pub fn mass_subtract(mass_a: Mass, mass_b: Mass) -> Mass {
  Mass(kilograms: mass_a.kilograms -. mass_b.kilograms)
}

/// Multiply a mass by a scalar.
pub fn mass_multiply(m: Mass, scalar: Float) -> Mass {
  Mass(kilograms: m.kilograms *. scalar)
}

/// Compare two masses.
pub fn mass_compare(mass_a: Mass, mass_b: Mass) -> Order {
  float.compare(mass_a.kilograms, mass_b.kilograms)
}

// Temperature operations

/// Create a temperature from a value and unit.
pub fn temperature(value: Float, unit: TemperatureUnit) -> Temperature {
  case unit {
    Kelvin -> Temperature(kelvin: value)
    Celsius -> Temperature(kelvin: value +. 273.15)
    Fahrenheit -> Temperature(kelvin: { value +. 459.67 } *. 5.0 /. 9.0)
  }
}

/// Get a temperature in a specific unit.
pub fn temperature_in(temp: Temperature, unit: TemperatureUnit) -> Float {
  case unit {
    Kelvin -> temp.kelvin
    Celsius -> temp.kelvin -. 273.15
    Fahrenheit -> temp.kelvin *. 9.0 /. 5.0 -. 459.67
  }
}

/// Add a temperature difference.
pub fn temperature_add(temp: Temperature, diff: Float) -> Temperature {
  Temperature(kelvin: temp.kelvin +. diff)
}

/// Compare two temperatures.
pub fn temperature_compare(temp_a: Temperature, temp_b: Temperature) -> Order {
  float.compare(temp_a.kelvin, temp_b.kelvin)
}

/// Check if temperature is at absolute zero.
pub fn is_absolute_zero(temp: Temperature) -> Bool {
  temp.kelvin == 0.0
}

/// Absolute zero temperature.
pub fn absolute_zero() -> Temperature {
  Temperature(kelvin: 0.0)
}

/// Water freezing point.
pub fn water_freezing_point() -> Temperature {
  temperature(0.0, Celsius)
}

/// Water boiling point.
pub fn water_boiling_point() -> Temperature {
  temperature(100.0, Celsius)
}

// Time duration operations

/// Create a time duration from a value and unit.
pub fn time_duration(value: Float, unit: TimeUnit) -> TimeDuration {
  TimeDuration(seconds: value *. time_to_seconds_factor(unit))
}

fn time_to_seconds_factor(unit: TimeUnit) -> Float {
  case unit {
    Second -> 1.0
    Millisecond -> 0.001
    Microsecond -> 0.000001
    Nanosecond -> 0.000000001
    Minute -> 60.0
    Hour -> 3600.0
    Day -> 86_400.0
    Week -> 604_800.0
  }
}

/// Get a time duration in a specific unit.
pub fn time_duration_in(duration: TimeDuration, unit: TimeUnit) -> Float {
  duration.seconds /. time_to_seconds_factor(unit)
}

/// Add two time durations.
pub fn time_duration_add(
  duration_a: TimeDuration,
  duration_b: TimeDuration,
) -> TimeDuration {
  TimeDuration(seconds: duration_a.seconds +. duration_b.seconds)
}

/// Subtract two time durations.
pub fn time_duration_subtract(
  duration_a: TimeDuration,
  duration_b: TimeDuration,
) -> TimeDuration {
  TimeDuration(seconds: duration_a.seconds -. duration_b.seconds)
}

/// Multiply a time duration by a scalar.
pub fn time_duration_multiply(duration: TimeDuration, scalar: Float) -> TimeDuration {
  TimeDuration(seconds: duration.seconds *. scalar)
}

/// Compare two time durations.
pub fn time_duration_compare(
  duration_a: TimeDuration,
  duration_b: TimeDuration,
) -> Order {
  float.compare(duration_a.seconds, duration_b.seconds)
}

// Data size operations

/// Create a data size from a value and unit.
pub fn data_size(value: Int, unit: DataUnit) -> DataSize {
  DataSize(bytes: value * data_to_bytes_factor(unit))
}

fn data_to_bytes_factor(unit: DataUnit) -> Int {
  case unit {
    Byte -> 1
    Kilobyte -> 1000
    Megabyte -> 1_000_000
    Gigabyte -> 1_000_000_000
    Terabyte -> 1_000_000_000_000
    Petabyte -> 1_000_000_000_000_000
    Kibibyte -> 1024
    Mebibyte -> 1_048_576
    Gibibyte -> 1_073_741_824
    Tebibyte -> 1_099_511_627_776
  }
}

/// Get a data size in a specific unit.
pub fn data_size_in(size: DataSize, unit: DataUnit) -> Int {
  size.bytes / data_to_bytes_factor(unit)
}

/// Get exact data size in a specific unit as float.
pub fn data_size_in_exact(size: DataSize, unit: DataUnit) -> Float {
  int.to_float(size.bytes) /. int.to_float(data_to_bytes_factor(unit))
}

/// Add two data sizes.
pub fn data_size_add(size_a: DataSize, size_b: DataSize) -> DataSize {
  DataSize(bytes: size_a.bytes + size_b.bytes)
}

/// Subtract two data sizes.
pub fn data_size_subtract(size_a: DataSize, size_b: DataSize) -> DataSize {
  DataSize(bytes: size_a.bytes - size_b.bytes)
}

/// Multiply a data size by a scalar.
pub fn data_size_multiply(size: DataSize, scalar: Int) -> DataSize {
  DataSize(bytes: size.bytes * scalar)
}

/// Compare two data sizes.
pub fn data_size_compare(size_a: DataSize, size_b: DataSize) -> Order {
  int.compare(size_a.bytes, size_b.bytes)
}

/// Format data size to human-readable string (binary prefixes).
pub fn data_size_format_binary(size: DataSize) -> String {
  let bytes = size.bytes
  case bytes >= 1_099_511_627_776 {
    True -> float.to_string(data_size_in_exact(size, Tebibyte)) <> " TiB"
    False ->
      case bytes >= 1_073_741_824 {
        True -> float.to_string(data_size_in_exact(size, Gibibyte)) <> " GiB"
        False ->
          case bytes >= 1_048_576 {
            True -> float.to_string(data_size_in_exact(size, Mebibyte)) <> " MiB"
            False ->
              case bytes >= 1024 {
                True -> float.to_string(data_size_in_exact(size, Kibibyte)) <> " KiB"
                False -> int.to_string(bytes) <> " B"
              }
          }
      }
  }
}

/// Format data size to human-readable string (decimal prefixes).
pub fn data_size_format_decimal(size: DataSize) -> String {
  let bytes = size.bytes
  case bytes >= 1_000_000_000_000 {
    True -> float.to_string(data_size_in_exact(size, Terabyte)) <> " TB"
    False ->
      case bytes >= 1_000_000_000 {
        True -> float.to_string(data_size_in_exact(size, Gigabyte)) <> " GB"
        False ->
          case bytes >= 1_000_000 {
            True -> float.to_string(data_size_in_exact(size, Megabyte)) <> " MB"
            False ->
              case bytes >= 1000 {
                True -> float.to_string(data_size_in_exact(size, Kilobyte)) <> " KB"
                False -> int.to_string(bytes) <> " B"
              }
          }
      }
  }
}

/// Speed of light in meters per second.
pub fn speed_of_light() -> Float {
  299_792_458.0
}

/// Gravitational constant (m^3 kg^-1 s^-2).
pub fn gravitational_constant() -> Float {
  6.67430e-11
}

/// Planck constant (J*s).
pub fn planck_constant() -> Float {
  6.62607015e-34
}

/// Avogadro constant (mol^-1).
pub fn avogadro_constant() -> Float {
  6.02214076e23
}

/// Boltzmann constant (J/K).
pub fn boltzmann_constant() -> Float {
  1.380649e-23
}
