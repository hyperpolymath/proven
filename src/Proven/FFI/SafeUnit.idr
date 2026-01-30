-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeUnit operations
|||
||| This module exports physical unit operations to the C ABI
||| via Idris2's RefC backend. All functions are proven total and enforce dimensional analysis.
|||
||| Return conventions:
||| - Dimension → (L, M, T, I, Θ, N, J) as 7 integers
||| - Operations → (Int, value, dimension) where status 0 = success, 1 = incompatible dimensions
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
|||
||| CRITICAL: Dimensional analysis prevents unit errors (e.g., adding meters to seconds).
|||           All arithmetic operations enforce dimension compatibility.
|||
||| SI base dimensions:
||| - L: Length (meter)
||| - M: Mass (kilogram)
||| - T: Time (second)
||| - I: Electric current (ampere)
||| - Θ: Temperature (kelvin)
||| - N: Amount of substance (mole)
||| - J: Luminous intensity (candela)
|||
||| Example derived units:
||| - Velocity: L/T
||| - Force: MLT⁻²
||| - Energy: ML²T⁻²
module Proven.FFI.SafeUnit

import Proven.SafeUnit
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode dimension as 7-tuple
encodeDimension : Dimension -> (Int, Int, Int, Int, Int, Int, Int)
encodeDimension d = (
  cast d.length,
  cast d.mass,
  cast d.time,
  cast d.current,
  cast d.temperature,
  cast d.amount,
  cast d.luminosity
)

||| Decode 7-tuple to dimension
decodeDimension : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Dimension
decodeDimension l m t i th n j = MkDim
  (cast l) (cast m) (cast t) (cast i) (cast th) (cast n) (cast j)

--------------------------------------------------------------------------------
-- Base Dimensions
--------------------------------------------------------------------------------

%export
proven_idris_unit_dimensionless : (Int, Int, Int, Int, Int, Int, Int)
proven_idris_unit_dimensionless = encodeDimension dimensionless

%export
proven_idris_unit_length_dim : (Int, Int, Int, Int, Int, Int, Int)
proven_idris_unit_length_dim = encodeDimension lengthDim

%export
proven_idris_unit_mass_dim : (Int, Int, Int, Int, Int, Int, Int)
proven_idris_unit_mass_dim = encodeDimension massDim

%export
proven_idris_unit_time_dim : (Int, Int, Int, Int, Int, Int, Int)
proven_idris_unit_time_dim = encodeDimension timeDim

%export
proven_idris_unit_current_dim : (Int, Int, Int, Int, Int, Int, Int)
proven_idris_unit_current_dim = encodeDimension currentDim

%export
proven_idris_unit_temperature_dim : (Int, Int, Int, Int, Int, Int, Int)
proven_idris_unit_temperature_dim = encodeDimension temperatureDim

--------------------------------------------------------------------------------
-- Derived Dimensions
--------------------------------------------------------------------------------

%export
proven_idris_unit_velocity_dim : (Int, Int, Int, Int, Int, Int, Int)
proven_idris_unit_velocity_dim = encodeDimension velocityDim

%export
proven_idris_unit_acceleration_dim : (Int, Int, Int, Int, Int, Int, Int)
proven_idris_unit_acceleration_dim = encodeDimension accelerationDim

%export
proven_idris_unit_force_dim : (Int, Int, Int, Int, Int, Int, Int)
proven_idris_unit_force_dim = encodeDimension forceDim

%export
proven_idris_unit_energy_dim : (Int, Int, Int, Int, Int, Int, Int)
proven_idris_unit_energy_dim = encodeDimension energyDim

%export
proven_idris_unit_power_dim : (Int, Int, Int, Int, Int, Int, Int)
proven_idris_unit_power_dim = encodeDimension powerDim

%export
proven_idris_unit_pressure_dim : (Int, Int, Int, Int, Int, Int, Int)
proven_idris_unit_pressure_dim = encodeDimension pressureDim

--------------------------------------------------------------------------------
-- Dimension Operations
--------------------------------------------------------------------------------

%export
proven_idris_unit_mul_dim : Int -> Int -> Int -> Int -> Int -> Int -> Int ->
                             Int -> Int -> Int -> Int -> Int -> Int -> Int ->
                             (Int, Int, Int, Int, Int, Int, Int)
proven_idris_unit_mul_dim l1 m1 t1 i1 th1 n1 j1 l2 m2 t2 i2 th2 n2 j2 =
  let d1 = decodeDimension l1 m1 t1 i1 th1 n1 j1
      d2 = decodeDimension l2 m2 t2 i2 th2 n2 j2
  in encodeDimension (mulDim d1 d2)

%export
proven_idris_unit_div_dim : Int -> Int -> Int -> Int -> Int -> Int -> Int ->
                             Int -> Int -> Int -> Int -> Int -> Int -> Int ->
                             (Int, Int, Int, Int, Int, Int, Int)
proven_idris_unit_div_dim l1 m1 t1 i1 th1 n1 j1 l2 m2 t2 i2 th2 n2 j2 =
  let d1 = decodeDimension l1 m1 t1 i1 th1 n1 j1
      d2 = decodeDimension l2 m2 t2 i2 th2 n2 j2
  in encodeDimension (divDim d1 d2)

%export
proven_idris_unit_dims_equal : Int -> Int -> Int -> Int -> Int -> Int -> Int ->
                                Int -> Int -> Int -> Int -> Int -> Int -> Int ->
                                Int
proven_idris_unit_dims_equal l1 m1 t1 i1 th1 n1 j1 l2 m2 t2 i2 th2 n2 j2 =
  let d1 = decodeDimension l1 m1 t1 i1 th1 n1 j1
      d2 = decodeDimension l2 m2 t2 i2 th2 n2 j2
  in encodeBool (d1 == d2)

%export
proven_idris_unit_is_dimensionless : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
proven_idris_unit_is_dimensionless l m t i th n j =
  let d = decodeDimension l m t i th n j
  in encodeBool (d == dimensionless)

--------------------------------------------------------------------------------
-- Quantity Arithmetic
--------------------------------------------------------------------------------

%export
proven_idris_unit_add : Double -> Int -> Int -> Int -> Int -> Int -> Int -> Int ->
                         Double -> Int -> Int -> Int -> Int -> Int -> Int -> Int ->
                         (Int, Double)
proven_idris_unit_add v1 l1 m1 t1 i1 th1 n1 j1 v2 l2 m2 t2 i2 th2 n2 j2 =
  let d1 = decodeDimension l1 m1 t1 i1 th1 n1 j1
      d2 = decodeDimension l2 m2 t2 i2 th2 n2 j2
      q1 = MkQuantity v1 d1 ""
      q2 = MkQuantity v2 d2 ""
  in case addQuantity q1 q2 of
       Nothing => (1, 0.0)  -- Incompatible dimensions
       Just result => (0, result.value)

%export
proven_idris_unit_subtract : Double -> Int -> Int -> Int -> Int -> Int -> Int -> Int ->
                              Double -> Int -> Int -> Int -> Int -> Int -> Int -> Int ->
                              (Int, Double)
proven_idris_unit_subtract v1 l1 m1 t1 i1 th1 n1 j1 v2 l2 m2 t2 i2 th2 n2 j2 =
  let d1 = decodeDimension l1 m1 t1 i1 th1 n1 j1
      d2 = decodeDimension l2 m2 t2 i2 th2 n2 j2
      q1 = MkQuantity v1 d1 ""
      q2 = MkQuantity v2 d2 ""
  in case subQuantity q1 q2 of
       Nothing => (1, 0.0)
       Just result => (0, result.value)

%export
proven_idris_unit_multiply : Double -> Int -> Int -> Int -> Int -> Int -> Int -> Int ->
                              Double -> Int -> Int -> Int -> Int -> Int -> Int -> Int ->
                              (Double, Int, Int, Int, Int, Int, Int, Int)
proven_idris_unit_multiply v1 l1 m1 t1 i1 th1 n1 j1 v2 l2 m2 t2 i2 th2 n2 j2 =
  let d1 = decodeDimension l1 m1 t1 i1 th1 n1 j1
      d2 = decodeDimension l2 m2 t2 i2 th2 n2 j2
      q1 = MkQuantity v1 d1 ""
      q2 = MkQuantity v2 d2 ""
      result = mulQuantity q1 q2
      (l, m, t, i, th, n, j) = encodeDimension result.dimension
  in (result.value, l, m, t, i, th, n, j)

%export
proven_idris_unit_divide : Double -> Int -> Int -> Int -> Int -> Int -> Int -> Int ->
                            Double -> Int -> Int -> Int -> Int -> Int -> Int -> Int ->
                            (Int, Double, Int, Int, Int, Int, Int, Int, Int)
proven_idris_unit_divide v1 l1 m1 t1 i1 th1 n1 j1 v2 l2 m2 t2 i2 th2 n2 j2 =
  let d1 = decodeDimension l1 m1 t1 i1 th1 n1 j1
      d2 = decodeDimension l2 m2 t2 i2 th2 n2 j2
      q1 = MkQuantity v1 d1 ""
      q2 = MkQuantity v2 d2 ""
  in case divQuantity q1 q2 of
       Nothing => (1, 0.0, 0, 0, 0, 0, 0, 0, 0)  -- Division by zero
       Just result =>
         let (l, m, t, i, th, n, j) = encodeDimension result.dimension
         in (0, result.value, l, m, t, i, th, n, j)

%export
proven_idris_unit_scale : Double -> Double -> Int -> Int -> Int -> Int -> Int -> Int -> Int ->
                           Double
proven_idris_unit_scale scalar value l m t i th n j =
  let d = decodeDimension l m t i th n j
      q = MkQuantity value d ""
      result = scale scalar q
  in result.value

--------------------------------------------------------------------------------
-- Unit Constructors (Length)
--------------------------------------------------------------------------------

%export
proven_idris_unit_meters : Double -> Double
proven_idris_unit_meters v = (meters v).value

%export
proven_idris_unit_kilometers : Double -> Double
proven_idris_unit_kilometers v = (kilometers v).value

%export
proven_idris_unit_miles : Double -> Double
proven_idris_unit_miles v = (miles v).value

%export
proven_idris_unit_meters_to_km : Double -> Double
proven_idris_unit_meters_to_km m = m / 1000.0

%export
proven_idris_unit_km_to_meters : Double -> Double
proven_idris_unit_km_to_meters km = km * 1000.0

%export
proven_idris_unit_miles_to_meters : Double -> Double
proven_idris_unit_miles_to_meters mi = mi * 1609.344

%export
proven_idris_unit_meters_to_miles : Double -> Double
proven_idris_unit_meters_to_miles m = m / 1609.344

--------------------------------------------------------------------------------
-- Unit Constructors (Mass)
--------------------------------------------------------------------------------

%export
proven_idris_unit_kilograms : Double -> Double
proven_idris_unit_kilograms v = (kilograms v).value

%export
proven_idris_unit_pounds : Double -> Double
proven_idris_unit_pounds v = (pounds v).value

%export
proven_idris_unit_pounds_to_kg : Double -> Double
proven_idris_unit_pounds_to_kg lb = lb * 0.453592

%export
proven_idris_unit_kg_to_pounds : Double -> Double
proven_idris_unit_kg_to_pounds kg = kg / 0.453592

--------------------------------------------------------------------------------
-- Unit Constructors (Time)
--------------------------------------------------------------------------------

%export
proven_idris_unit_seconds : Double -> Double
proven_idris_unit_seconds v = (seconds v).value

%export
proven_idris_unit_hours : Double -> Double
proven_idris_unit_hours v = (hours v).value

%export
proven_idris_unit_hours_to_seconds : Double -> Double
proven_idris_unit_hours_to_seconds h = h * 3600.0

%export
proven_idris_unit_seconds_to_hours : Double -> Double
proven_idris_unit_seconds_to_hours s = s / 3600.0

--------------------------------------------------------------------------------
-- Unit Constructors (Temperature)
--------------------------------------------------------------------------------

%export
proven_idris_unit_kelvin : Double -> Double
proven_idris_unit_kelvin v = (kelvin v).value

%export
proven_idris_unit_celsius : Double -> Double
proven_idris_unit_celsius c = (celsius c).value

%export
proven_idris_unit_fahrenheit : Double -> Double
proven_idris_unit_fahrenheit f = (fahrenheit f).value

%export
proven_idris_unit_kelvin_to_celsius : Double -> Double
proven_idris_unit_kelvin_to_celsius k = k - 273.15

%export
proven_idris_unit_celsius_to_kelvin : Double -> Double
proven_idris_unit_celsius_to_kelvin c = c + 273.15

%export
proven_idris_unit_fahrenheit_to_celsius : Double -> Double
proven_idris_unit_fahrenheit_to_celsius f = (f - 32.0) * 5.0 / 9.0

%export
proven_idris_unit_celsius_to_fahrenheit : Double -> Double
proven_idris_unit_celsius_to_fahrenheit c = c * 9.0 / 5.0 + 32.0

--------------------------------------------------------------------------------
-- Conversion Factors
--------------------------------------------------------------------------------

%export
proven_idris_unit_meter_to_km_factor : Double
proven_idris_unit_meter_to_km_factor = 0.001

%export
proven_idris_unit_mile_to_meter_factor : Double
proven_idris_unit_mile_to_meter_factor = 1609.344

%export
proven_idris_unit_pound_to_kg_factor : Double
proven_idris_unit_pound_to_kg_factor = 0.453592

%export
proven_idris_unit_hour_to_second_factor : Double
proven_idris_unit_hour_to_second_factor = 3600.0

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_unit_friendly_error : String -> String
proven_idris_unit_friendly_error errorMsg =
  if isInfixOf "dimension" (toLower errorMsg) || isInfixOf "incompatible" (toLower errorMsg)
    then "Incompatible dimensions (cannot add/subtract different units)"
  else if isInfixOf "division" (toLower errorMsg) || isInfixOf "zero" (toLower errorMsg)
    then "Division by zero in unit operation"
  else if isInfixOf "conversion" (toLower errorMsg)
    then "Invalid unit conversion"
  else
    "Physical unit error"
