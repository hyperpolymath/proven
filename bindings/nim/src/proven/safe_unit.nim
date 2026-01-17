# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe physical unit conversions.

import std/[options]

type
  LengthUnit* = enum
    ## Length units.
    luMeters,
    luKilometers,
    luCentimeters,
    luMillimeters,
    luMiles,
    luYards,
    luFeet,
    luInches

  MassUnit* = enum
    ## Mass units.
    muKilograms,
    muGrams,
    muMilligrams,
    muPounds,
    muOunces,
    muStones

  TemperatureUnit* = enum
    ## Temperature units.
    tuCelsius,
    tuFahrenheit,
    tuKelvin

  TimeUnit* = enum
    ## Time units.
    tiSeconds,
    tiMilliseconds,
    tiMicroseconds,
    tiNanoseconds,
    tiMinutes,
    tiHours,
    tiDays,
    tiWeeks

  DataUnit* = enum
    ## Data size units.
    duBytes,
    duKilobytes,
    duMegabytes,
    duGigabytes,
    duTerabytes,
    duKibibytes,
    duMebibytes,
    duGibibytes,
    duTebibytes

# Length conversions
proc toMeters*(value: float64, unit: LengthUnit): float64 =
  ## Convert to meters (base unit).
  case unit
  of luMeters: value
  of luKilometers: value * 1000.0
  of luCentimeters: value / 100.0
  of luMillimeters: value / 1000.0
  of luMiles: value * 1609.344
  of luYards: value * 0.9144
  of luFeet: value * 0.3048
  of luInches: value * 0.0254

proc fromMeters*(meters: float64, unit: LengthUnit): float64 =
  ## Convert from meters.
  case unit
  of luMeters: meters
  of luKilometers: meters / 1000.0
  of luCentimeters: meters * 100.0
  of luMillimeters: meters * 1000.0
  of luMiles: meters / 1609.344
  of luYards: meters / 0.9144
  of luFeet: meters / 0.3048
  of luInches: meters / 0.0254

proc convertLength*(value: float64, fromUnit, toUnit: LengthUnit): float64 =
  ## Convert length between units.
  let meters = toMeters(value, fromUnit)
  fromMeters(meters, toUnit)

# Mass conversions
proc toKilograms*(value: float64, unit: MassUnit): float64 =
  ## Convert to kilograms (base unit).
  case unit
  of muKilograms: value
  of muGrams: value / 1000.0
  of muMilligrams: value / 1_000_000.0
  of muPounds: value * 0.453592
  of muOunces: value * 0.0283495
  of muStones: value * 6.35029

proc fromKilograms*(kg: float64, unit: MassUnit): float64 =
  ## Convert from kilograms.
  case unit
  of muKilograms: kg
  of muGrams: kg * 1000.0
  of muMilligrams: kg * 1_000_000.0
  of muPounds: kg / 0.453592
  of muOunces: kg / 0.0283495
  of muStones: kg / 6.35029

proc convertMass*(value: float64, fromUnit, toUnit: MassUnit): float64 =
  ## Convert mass between units.
  let kg = toKilograms(value, fromUnit)
  fromKilograms(kg, toUnit)

# Temperature conversions
proc toKelvin*(value: float64, unit: TemperatureUnit): float64 =
  ## Convert to Kelvin (base unit).
  case unit
  of tuCelsius: value + 273.15
  of tuFahrenheit: (value - 32.0) * 5.0 / 9.0 + 273.15
  of tuKelvin: value

proc fromKelvin*(kelvin: float64, unit: TemperatureUnit): float64 =
  ## Convert from Kelvin.
  case unit
  of tuCelsius: kelvin - 273.15
  of tuFahrenheit: (kelvin - 273.15) * 9.0 / 5.0 + 32.0
  of tuKelvin: kelvin

proc convertTemperature*(value: float64, fromUnit, toUnit: TemperatureUnit): float64 =
  ## Convert temperature between units.
  let kelvin = toKelvin(value, fromUnit)
  fromKelvin(kelvin, toUnit)

proc celsiusToFahrenheit*(c: float64): float64 =
  ## Convert Celsius to Fahrenheit.
  c * 9.0 / 5.0 + 32.0

proc fahrenheitToCelsius*(f: float64): float64 =
  ## Convert Fahrenheit to Celsius.
  (f - 32.0) * 5.0 / 9.0

# Time conversions
proc toSeconds*(value: float64, unit: TimeUnit): float64 =
  ## Convert to seconds (base unit).
  case unit
  of tiSeconds: value
  of tiMilliseconds: value / 1000.0
  of tiMicroseconds: value / 1_000_000.0
  of tiNanoseconds: value / 1_000_000_000.0
  of tiMinutes: value * 60.0
  of tiHours: value * 3600.0
  of tiDays: value * 86400.0
  of tiWeeks: value * 604800.0

proc fromSeconds*(secs: float64, unit: TimeUnit): float64 =
  ## Convert from seconds.
  case unit
  of tiSeconds: secs
  of tiMilliseconds: secs * 1000.0
  of tiMicroseconds: secs * 1_000_000.0
  of tiNanoseconds: secs * 1_000_000_000.0
  of tiMinutes: secs / 60.0
  of tiHours: secs / 3600.0
  of tiDays: secs / 86400.0
  of tiWeeks: secs / 604800.0

proc convertTime*(value: float64, fromUnit, toUnit: TimeUnit): float64 =
  ## Convert time between units.
  let secs = toSeconds(value, fromUnit)
  fromSeconds(secs, toUnit)

# Data size conversions
proc toBytes*(value: float64, unit: DataUnit): float64 =
  ## Convert to bytes.
  case unit
  of duBytes: value
  of duKilobytes: value * 1000.0
  of duMegabytes: value * 1_000_000.0
  of duGigabytes: value * 1_000_000_000.0
  of duTerabytes: value * 1_000_000_000_000.0
  of duKibibytes: value * 1024.0
  of duMebibytes: value * 1_048_576.0
  of duGibibytes: value * 1_073_741_824.0
  of duTebibytes: value * 1_099_511_627_776.0

proc fromBytes*(bytes: float64, unit: DataUnit): float64 =
  ## Convert from bytes.
  case unit
  of duBytes: bytes
  of duKilobytes: bytes / 1000.0
  of duMegabytes: bytes / 1_000_000.0
  of duGigabytes: bytes / 1_000_000_000.0
  of duTerabytes: bytes / 1_000_000_000_000.0
  of duKibibytes: bytes / 1024.0
  of duMebibytes: bytes / 1_048_576.0
  of duGibibytes: bytes / 1_073_741_824.0
  of duTebibytes: bytes / 1_099_511_627_776.0

proc convertData*(value: float64, fromUnit, toUnit: DataUnit): float64 =
  ## Convert data size between units.
  let bytes = toBytes(value, fromUnit)
  fromBytes(bytes, toUnit)
