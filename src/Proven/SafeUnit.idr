-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeUnit - Safe physical unit handling and conversion
|||
||| This module provides type-safe physical unit operations
||| preventing invalid unit combinations and conversions.
module Proven.SafeUnit
import Data.String
import Data.List

import public Proven.Core
import public Proven.SafeFloat

%default total

--------------------------------------------------------------------------------
-- Unit Dimensions
--------------------------------------------------------------------------------

||| Physical dimensions (length, mass, time, etc.)
public export
record Dimension where
  constructor MkDim
  length : Integer      -- L
  mass : Integer        -- M
  time : Integer        -- T
  current : Integer     -- I
  temperature : Integer -- Θ
  amount : Integer      -- N
  luminosity : Integer  -- J

public export
Eq Dimension where
  a == b = a.length == b.length
        && a.mass == b.mass
        && a.time == b.time
        && a.current == b.current
        && a.temperature == b.temperature
        && a.amount == b.amount
        && a.luminosity == b.luminosity

||| Dimensionless quantity
public export
dimensionless : Dimension
dimensionless = MkDim 0 0 0 0 0 0 0

||| Base dimensions
public export
lengthDim : Dimension
lengthDim = MkDim 1 0 0 0 0 0 0

public export
massDim : Dimension
massDim = MkDim 0 1 0 0 0 0 0

public export
timeDim : Dimension
timeDim = MkDim 0 0 1 0 0 0 0

public export
currentDim : Dimension
currentDim = MkDim 0 0 0 1 0 0 0

public export
temperatureDim : Dimension
temperatureDim = MkDim 0 0 0 0 1 0 0

--------------------------------------------------------------------------------
-- Derived Dimensions
--------------------------------------------------------------------------------

||| Multiply dimensions (for derived units)
public export
mulDim : Dimension -> Dimension -> Dimension
mulDim a b = MkDim
  (a.length + b.length)
  (a.mass + b.mass)
  (a.time + b.time)
  (a.current + b.current)
  (a.temperature + b.temperature)
  (a.amount + b.amount)
  (a.luminosity + b.luminosity)

||| Divide dimensions
public export
divDim : Dimension -> Dimension -> Dimension
divDim a b = MkDim
  (a.length - b.length)
  (a.mass - b.mass)
  (a.time - b.time)
  (a.current - b.current)
  (a.temperature - b.temperature)
  (a.amount - b.amount)
  (a.luminosity - b.luminosity)

||| Velocity: L/T
public export
velocityDim : Dimension
velocityDim = divDim lengthDim timeDim

||| Acceleration: L/T²
public export
accelerationDim : Dimension
accelerationDim = MkDim 1 0 (-2) 0 0 0 0

||| Force: M·L/T² (Newton)
public export
forceDim : Dimension
forceDim = MkDim 1 1 (-2) 0 0 0 0

||| Energy: M·L²/T² (Joule)
public export
energyDim : Dimension
energyDim = MkDim 2 1 (-2) 0 0 0 0

||| Power: M·L²/T³ (Watt)
public export
powerDim : Dimension
powerDim = MkDim 2 1 (-3) 0 0 0 0

||| Pressure: M/(L·T²) (Pascal)
public export
pressureDim : Dimension
pressureDim = MkDim (-1) 1 (-2) 0 0 0 0

--------------------------------------------------------------------------------
-- Quantity Type
--------------------------------------------------------------------------------

||| A physical quantity with value and dimension
public export
record Quantity where
  constructor MkQuantity
  value : Double
  dimension : Dimension
  unitName : String

--------------------------------------------------------------------------------
-- Safe Operations
--------------------------------------------------------------------------------

||| Add quantities (only if dimensions match)
public export
addQuantity : Quantity -> Quantity -> Maybe Quantity
addQuantity a b =
  if a.dimension == b.dimension
    then Just (MkQuantity (a.value + b.value) a.dimension a.unitName)
    else Nothing

||| Subtract quantities (only if dimensions match)
public export
subQuantity : Quantity -> Quantity -> Maybe Quantity
subQuantity a b =
  if a.dimension == b.dimension
    then Just (MkQuantity (a.value - b.value) a.dimension a.unitName)
    else Nothing

||| Multiply quantities (always valid, dimensions combine)
public export
mulQuantity : Quantity -> Quantity -> Quantity
mulQuantity a b = MkQuantity
  (a.value * b.value)
  (mulDim a.dimension b.dimension)
  (a.unitName ++ "·" ++ b.unitName)

||| Divide quantities (only if divisor non-zero)
public export
divQuantity : Quantity -> Quantity -> Maybe Quantity
divQuantity a b =
  if b.value == 0.0 then Nothing
  else Just (MkQuantity
    (a.value / b.value)
    (divDim a.dimension b.dimension)
    (a.unitName ++ "/" ++ b.unitName))

||| Scale a quantity by a dimensionless factor
public export
scale : Double -> Quantity -> Quantity
scale s q = MkQuantity (s * q.value) q.dimension q.unitName

--------------------------------------------------------------------------------
-- Common Unit Conversions
--------------------------------------------------------------------------------

||| Create a length in meters
public export
meters : Double -> Quantity
meters v = MkQuantity v lengthDim "m"

||| Create a length in kilometers
public export
kilometers : Double -> Quantity
kilometers v = MkQuantity (v * 1000.0) lengthDim "km"

||| Create a length in miles
public export
miles : Double -> Quantity
miles v = MkQuantity (v * 1609.344) lengthDim "mi"

||| Create a mass in kilograms
public export
kilograms : Double -> Quantity
kilograms v = MkQuantity v massDim "kg"

||| Create a mass in pounds
public export
pounds : Double -> Quantity
pounds v = MkQuantity (v * 0.453592) massDim "lb"

||| Create a time in seconds
public export
seconds : Double -> Quantity
seconds v = MkQuantity v timeDim "s"

||| Create a time in hours
public export
hours : Double -> Quantity
hours v = MkQuantity (v * 3600.0) timeDim "h"

||| Create a temperature in Kelvin
public export
kelvin : Double -> Quantity
kelvin v = MkQuantity v temperatureDim "K"

||| Create a temperature from Celsius
public export
celsius : Double -> Quantity
celsius c = MkQuantity (c + 273.15) temperatureDim "°C"

||| Create a temperature from Fahrenheit
public export
fahrenheit : Double -> Quantity
fahrenheit f = MkQuantity ((f - 32.0) * 5.0 / 9.0 + 273.15) temperatureDim "°F"

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Show Quantity where
  show q = show q.value ++ " " ++ q.unitName
