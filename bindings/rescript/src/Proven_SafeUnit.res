// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeUnit - Safe physical unit conversions that cannot crash.
 *
 * Provides type-safe conversions for length, mass, temperature, time, and data sizes.
 */

/** Length units */
type lengthUnit =
  | Meters
  | Kilometers
  | Centimeters
  | Millimeters
  | Miles
  | Yards
  | Feet
  | Inches

/** Mass units */
type massUnit =
  | Kilograms
  | Grams
  | Milligrams
  | Pounds
  | Ounces
  | Stones

/** Temperature units */
type temperatureUnit =
  | Celsius
  | Fahrenheit
  | Kelvin

/** Time units */
type timeUnit =
  | Seconds
  | Milliseconds
  | Microseconds
  | Nanoseconds
  | Minutes
  | Hours
  | Days
  | Weeks

/** Data size units */
type dataUnit =
  | Bytes
  | Kilobytes
  | Megabytes
  | Gigabytes
  | Terabytes
  | Kibibytes
  | Mebibytes
  | Gibibytes
  | Tebibytes

/** Convert length to meters (base unit) */
let lengthToMeters = (value: float, unit: lengthUnit): float => {
  switch unit {
  | Meters => value
  | Kilometers => value *. 1000.0
  | Centimeters => value /. 100.0
  | Millimeters => value /. 1000.0
  | Miles => value *. 1609.344
  | Yards => value *. 0.9144
  | Feet => value *. 0.3048
  | Inches => value *. 0.0254
  }
}

/** Convert length from meters */
let lengthFromMeters = (meters: float, unit: lengthUnit): float => {
  switch unit {
  | Meters => meters
  | Kilometers => meters /. 1000.0
  | Centimeters => meters *. 100.0
  | Millimeters => meters *. 1000.0
  | Miles => meters /. 1609.344
  | Yards => meters /. 0.9144
  | Feet => meters /. 0.3048
  | Inches => meters /. 0.0254
  }
}

/** Convert length between units */
let convertLength = (value: float, ~from: lengthUnit, ~to_: lengthUnit): float => {
  let meters = lengthToMeters(value, from)
  lengthFromMeters(meters, to_)
}

/** Convert mass to kilograms (base unit) */
let massToKilograms = (value: float, unit: massUnit): float => {
  switch unit {
  | Kilograms => value
  | Grams => value /. 1000.0
  | Milligrams => value /. 1000000.0
  | Pounds => value *. 0.453592
  | Ounces => value *. 0.0283495
  | Stones => value *. 6.35029
  }
}

/** Convert mass from kilograms */
let massFromKilograms = (kg: float, unit: massUnit): float => {
  switch unit {
  | Kilograms => kg
  | Grams => kg *. 1000.0
  | Milligrams => kg *. 1000000.0
  | Pounds => kg /. 0.453592
  | Ounces => kg /. 0.0283495
  | Stones => kg /. 6.35029
  }
}

/** Convert mass between units */
let convertMass = (value: float, ~from: massUnit, ~to_: massUnit): float => {
  let kg = massToKilograms(value, from)
  massFromKilograms(kg, to_)
}

/** Convert temperature between units */
let convertTemperature = (value: float, ~from: temperatureUnit, ~to_: temperatureUnit): float => {
  // Convert to Kelvin first
  let kelvin = switch from {
  | Celsius => value +. 273.15
  | Fahrenheit => (value -. 32.0) *. 5.0 /. 9.0 +. 273.15
  | Kelvin => value
  }

  // Convert from Kelvin
  switch to_ {
  | Celsius => kelvin -. 273.15
  | Fahrenheit => (kelvin -. 273.15) *. 9.0 /. 5.0 +. 32.0
  | Kelvin => kelvin
  }
}

/** Convert time to seconds (base unit) */
let timeToSeconds = (value: float, unit: timeUnit): float => {
  switch unit {
  | Seconds => value
  | Milliseconds => value /. 1000.0
  | Microseconds => value /. 1000000.0
  | Nanoseconds => value /. 1000000000.0
  | Minutes => value *. 60.0
  | Hours => value *. 3600.0
  | Days => value *. 86400.0
  | Weeks => value *. 604800.0
  }
}

/** Convert time from seconds */
let timeFromSeconds = (secs: float, unit: timeUnit): float => {
  switch unit {
  | Seconds => secs
  | Milliseconds => secs *. 1000.0
  | Microseconds => secs *. 1000000.0
  | Nanoseconds => secs *. 1000000000.0
  | Minutes => secs /. 60.0
  | Hours => secs /. 3600.0
  | Days => secs /. 86400.0
  | Weeks => secs /. 604800.0
  }
}

/** Convert time between units */
let convertTime = (value: float, ~from: timeUnit, ~to_: timeUnit): float => {
  let secs = timeToSeconds(value, from)
  timeFromSeconds(secs, to_)
}

/** Convert data size to bytes (base unit) */
let dataToBytes = (value: float, unit: dataUnit): float => {
  switch unit {
  | Bytes => value
  | Kilobytes => value *. 1000.0
  | Megabytes => value *. 1000000.0
  | Gigabytes => value *. 1000000000.0
  | Terabytes => value *. 1000000000000.0
  | Kibibytes => value *. 1024.0
  | Mebibytes => value *. 1048576.0
  | Gibibytes => value *. 1073741824.0
  | Tebibytes => value *. 1099511627776.0
  }
}

/** Convert data size from bytes */
let dataFromBytes = (bytes: float, unit: dataUnit): float => {
  switch unit {
  | Bytes => bytes
  | Kilobytes => bytes /. 1000.0
  | Megabytes => bytes /. 1000000.0
  | Gigabytes => bytes /. 1000000000.0
  | Terabytes => bytes /. 1000000000000.0
  | Kibibytes => bytes /. 1024.0
  | Mebibytes => bytes /. 1048576.0
  | Gibibytes => bytes /. 1073741824.0
  | Tebibytes => bytes /. 1099511627776.0
  }
}

/** Convert data size between units */
let convertData = (value: float, ~from: dataUnit, ~to_: dataUnit): float => {
  let bytes = dataToBytes(value, from)
  dataFromBytes(bytes, to_)
}

/** Format bytes as human-readable string (decimal units) */
let formatBytesDecimal = (bytes: float): string => {
  if bytes < 1000.0 {
    `${Js.Float.toFixedWithPrecision(bytes, ~digits=0)} B`
  } else if bytes < 1000000.0 {
    `${Js.Float.toFixedWithPrecision(bytes /. 1000.0, ~digits=2)} KB`
  } else if bytes < 1000000000.0 {
    `${Js.Float.toFixedWithPrecision(bytes /. 1000000.0, ~digits=2)} MB`
  } else if bytes < 1000000000000.0 {
    `${Js.Float.toFixedWithPrecision(bytes /. 1000000000.0, ~digits=2)} GB`
  } else {
    `${Js.Float.toFixedWithPrecision(bytes /. 1000000000000.0, ~digits=2)} TB`
  }
}

/** Format bytes as human-readable string (binary units) */
let formatBytesBinary = (bytes: float): string => {
  if bytes < 1024.0 {
    `${Js.Float.toFixedWithPrecision(bytes, ~digits=0)} B`
  } else if bytes < 1048576.0 {
    `${Js.Float.toFixedWithPrecision(bytes /. 1024.0, ~digits=2)} KiB`
  } else if bytes < 1073741824.0 {
    `${Js.Float.toFixedWithPrecision(bytes /. 1048576.0, ~digits=2)} MiB`
  } else if bytes < 1099511627776.0 {
    `${Js.Float.toFixedWithPrecision(bytes /. 1073741824.0, ~digits=2)} GiB`
  } else {
    `${Js.Float.toFixedWithPrecision(bytes /. 1099511627776.0, ~digits=2)} TiB`
  }
}

/** Format duration as human-readable string */
let formatDuration = (seconds: float): string => {
  if seconds < 0.001 {
    `${Js.Float.toFixedWithPrecision(seconds *. 1000000.0, ~digits=0)} us`
  } else if seconds < 1.0 {
    `${Js.Float.toFixedWithPrecision(seconds *. 1000.0, ~digits=2)} ms`
  } else if seconds < 60.0 {
    `${Js.Float.toFixedWithPrecision(seconds, ~digits=2)} s`
  } else if seconds < 3600.0 {
    let mins = Js.Math.floor_float(seconds /. 60.0)
    let secs = mod_float(seconds, 60.0)
    `${Js.Float.toFixedWithPrecision(mins, ~digits=0)}m ${Js.Float.toFixedWithPrecision(secs, ~digits=0)}s`
  } else if seconds < 86400.0 {
    let hours = Js.Math.floor_float(seconds /. 3600.0)
    let mins = Js.Math.floor_float(mod_float(seconds, 3600.0) /. 60.0)
    `${Js.Float.toFixedWithPrecision(hours, ~digits=0)}h ${Js.Float.toFixedWithPrecision(mins, ~digits=0)}m`
  } else {
    let days = Js.Math.floor_float(seconds /. 86400.0)
    let hours = Js.Math.floor_float(mod_float(seconds, 86400.0) /. 3600.0)
    `${Js.Float.toFixedWithPrecision(days, ~digits=0)}d ${Js.Float.toFixedWithPrecision(hours, ~digits=0)}h`
  }
}
