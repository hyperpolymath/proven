# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeUnit - Unit handling and conversion for Nickel
#
# Provides type-safe unit conversions for length, mass,
# temperature, and other physical quantities.

{
  # Result type
  Result = {
    value | Number,
    ok | Bool,
  },

  # Quantity with unit
  Quantity = {
    value | Number,
    unit | String,
  },

  # Length conversion factors to meters
  LENGTH_TO_METERS = {
    m = 1,
    km = 1000,
    cm = 0.01,
    mm = 0.001,
    um = 0.000001,
    nm = 0.000000001,
    mi = 1609.344,
    yd = 0.9144,
    ft = 0.3048,
    inch = 0.0254,
    nmi = 1852,
  },

  # Mass conversion factors to kilograms
  MASS_TO_KG = {
    kg = 1,
    g = 0.001,
    mg = 0.000001,
    ug = 0.000000001,
    t = 1000,
    lb = 0.45359237,
    oz = 0.028349523125,
    st = 6.35029318,
  },

  # Time conversion factors to seconds
  TIME_TO_SECONDS = {
    s = 1,
    ms = 0.001,
    us = 0.000001,
    ns = 0.000000001,
    min = 60,
    h = 3600,
    d = 86400,
    wk = 604800,
  },

  # Volume conversion factors to liters
  VOLUME_TO_LITERS = {
    l = 1,
    ml = 0.001,
    kl = 1000,
    gal = 3.785411784,
    qt = 0.946352946,
    pt = 0.473176473,
    cup = 0.2365882365,
    floz = 0.0295735295625,
  },

  # Convert length between units
  convert_length
    : Number -> String -> String -> { value : Number, ok : Bool }
    = fun value from_unit to_unit =>
      let from_factor =
        if from_unit == "m" then 1 else if from_unit == "km" then 1000
        else if from_unit == "cm" then 0.01 else if from_unit == "mm" then 0.001
        else if from_unit == "mi" then 1609.344 else if from_unit == "yd" then 0.9144
        else if from_unit == "ft" then 0.3048 else if from_unit == "in" then 0.0254
        else -1
      in
      let to_factor =
        if to_unit == "m" then 1 else if to_unit == "km" then 1000
        else if to_unit == "cm" then 0.01 else if to_unit == "mm" then 0.001
        else if to_unit == "mi" then 1609.344 else if to_unit == "yd" then 0.9144
        else if to_unit == "ft" then 0.3048 else if to_unit == "in" then 0.0254
        else -1
      in
      if from_factor < 0 || to_factor < 0 then
        { value = 0, ok = false }
      else
        { value = value * from_factor / to_factor, ok = true },

  # Convert mass between units
  convert_mass
    : Number -> String -> String -> { value : Number, ok : Bool }
    = fun value from_unit to_unit =>
      let from_factor =
        if from_unit == "kg" then 1 else if from_unit == "g" then 0.001
        else if from_unit == "mg" then 0.000001 else if from_unit == "t" then 1000
        else if from_unit == "lb" then 0.45359237 else if from_unit == "oz" then 0.028349523125
        else -1
      in
      let to_factor =
        if to_unit == "kg" then 1 else if to_unit == "g" then 0.001
        else if to_unit == "mg" then 0.000001 else if to_unit == "t" then 1000
        else if to_unit == "lb" then 0.45359237 else if to_unit == "oz" then 0.028349523125
        else -1
      in
      if from_factor < 0 || to_factor < 0 then
        { value = 0, ok = false }
      else
        { value = value * from_factor / to_factor, ok = true },

  # Convert time between units
  convert_time
    : Number -> String -> String -> { value : Number, ok : Bool }
    = fun value from_unit to_unit =>
      let from_factor =
        if from_unit == "s" then 1 else if from_unit == "ms" then 0.001
        else if from_unit == "us" then 0.000001 else if from_unit == "ns" then 0.000000001
        else if from_unit == "min" then 60 else if from_unit == "h" then 3600
        else if from_unit == "d" then 86400 else if from_unit == "wk" then 604800
        else -1
      in
      let to_factor =
        if to_unit == "s" then 1 else if to_unit == "ms" then 0.001
        else if to_unit == "us" then 0.000001 else if to_unit == "ns" then 0.000000001
        else if to_unit == "min" then 60 else if to_unit == "h" then 3600
        else if to_unit == "d" then 86400 else if to_unit == "wk" then 604800
        else -1
      in
      if from_factor < 0 || to_factor < 0 then
        { value = 0, ok = false }
      else
        { value = value * from_factor / to_factor, ok = true },

  # Temperature conversions (special case - not linear)
  celsius_to_fahrenheit
    : Number -> Number
    = fun c => c * 9 / 5 + 32,

  fahrenheit_to_celsius
    : Number -> Number
    = fun f => (f - 32) * 5 / 9,

  celsius_to_kelvin
    : Number -> Number
    = fun c => c + 273.15,

  kelvin_to_celsius
    : Number -> Number
    = fun k => k - 273.15,

  fahrenheit_to_kelvin
    : Number -> Number
    = fun f => celsius_to_kelvin (fahrenheit_to_celsius f),

  kelvin_to_fahrenheit
    : Number -> Number
    = fun k => celsius_to_fahrenheit (kelvin_to_celsius k),

  # Convert temperature between units
  convert_temperature
    : Number -> String -> String -> { value : Number, ok : Bool }
    = fun value from_unit to_unit =>
      if from_unit == to_unit then
        { value = value, ok = true }
      else if from_unit == "C" && to_unit == "F" then
        { value = celsius_to_fahrenheit value, ok = true }
      else if from_unit == "F" && to_unit == "C" then
        { value = fahrenheit_to_celsius value, ok = true }
      else if from_unit == "C" && to_unit == "K" then
        { value = celsius_to_kelvin value, ok = true }
      else if from_unit == "K" && to_unit == "C" then
        { value = kelvin_to_celsius value, ok = true }
      else if from_unit == "F" && to_unit == "K" then
        { value = fahrenheit_to_kelvin value, ok = true }
      else if from_unit == "K" && to_unit == "F" then
        { value = kelvin_to_fahrenheit value, ok = true }
      else
        { value = 0, ok = false },

  # Data size conversion (binary, 1024-based)
  convert_data_size
    : Number -> String -> String -> { value : Number, ok : Bool }
    = fun value from_unit to_unit =>
      let from_bytes =
        if from_unit == "B" then 1
        else if from_unit == "KB" || from_unit == "KiB" then 1024
        else if from_unit == "MB" || from_unit == "MiB" then 1048576
        else if from_unit == "GB" || from_unit == "GiB" then 1073741824
        else if from_unit == "TB" || from_unit == "TiB" then 1099511627776
        else -1
      in
      let to_bytes =
        if to_unit == "B" then 1
        else if to_unit == "KB" || to_unit == "KiB" then 1024
        else if to_unit == "MB" || to_unit == "MiB" then 1048576
        else if to_unit == "GB" || to_unit == "GiB" then 1073741824
        else if to_unit == "TB" || to_unit == "TiB" then 1099511627776
        else -1
      in
      if from_bytes < 0 || to_bytes < 0 then
        { value = 0, ok = false }
      else
        { value = value * from_bytes / to_bytes, ok = true },

  # Speed conversions
  convert_speed
    : Number -> String -> String -> { value : Number, ok : Bool }
    = fun value from_unit to_unit =>
      let from_mps =
        if from_unit == "m/s" then 1
        else if from_unit == "km/h" then 0.277778
        else if from_unit == "mph" then 0.44704
        else if from_unit == "kn" then 0.514444
        else -1
      in
      let to_mps =
        if to_unit == "m/s" then 1
        else if to_unit == "km/h" then 0.277778
        else if to_unit == "mph" then 0.44704
        else if to_unit == "kn" then 0.514444
        else -1
      in
      if from_mps < 0 || to_mps < 0 then
        { value = 0, ok = false }
      else
        { value = value * from_mps / to_mps, ok = true },

  # Format quantity with unit
  format_quantity
    : Quantity -> String
    = fun q =>
      std.string.from_number q.value ++ " " ++ q.unit,

  # Contracts
  PositiveQuantity = std.contract.from_predicate (fun q => q.value > 0),
  NonNegativeQuantity = std.contract.from_predicate (fun q => q.value >= 0),

  # Temperature contracts
  AboveAbsoluteZero = std.contract.from_predicate (fun k => k >= 0),
}
