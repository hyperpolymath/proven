-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeUnit - Verified physical unit operations
|||
||| Type-safe dimensional analysis preventing unit mismatch errors.
||| Inspired by the Mars Climate Orbiter failure (pound-seconds vs newton-seconds).
|||
||| Uses phantom types to encode dimensions at the type level.
module Proven.SafeUnit

import Proven.Core
import Data.So

%default total

-- ============================================================================
-- DIMENSION INDICES (SI Base Units)
-- ============================================================================

||| SI dimension exponents: (length, mass, time, current, temperature, amount, luminosity)
||| For example: velocity = m/s = (1, 0, -1, 0, 0, 0, 0)
public export
record Dimensions where
  constructor MkDim
  length : Integer      -- meter (m)
  mass : Integer        -- kilogram (kg)
  time : Integer        -- second (s)
  current : Integer     -- ampere (A)
  temperature : Integer -- kelvin (K)
  amount : Integer      -- mole (mol)
  luminosity : Integer  -- candela (cd)

-- ============================================================================
-- DIMENSION OPERATIONS
-- ============================================================================

||| Dimensionless (all exponents zero)
public export
Dimensionless : Dimensions
Dimensionless = MkDim 0 0 0 0 0 0 0

||| Multiply dimensions (add exponents)
public export
dimMul : Dimensions -> Dimensions -> Dimensions
dimMul a b = MkDim
  (a.length + b.length)
  (a.mass + b.mass)
  (a.time + b.time)
  (a.current + b.current)
  (a.temperature + b.temperature)
  (a.amount + b.amount)
  (a.luminosity + b.luminosity)

||| Divide dimensions (subtract exponents)
public export
dimDiv : Dimensions -> Dimensions -> Dimensions
dimDiv a b = MkDim
  (a.length - b.length)
  (a.mass - b.mass)
  (a.time - b.time)
  (a.current - b.current)
  (a.temperature - b.temperature)
  (a.amount - b.amount)
  (a.luminosity - b.luminosity)

||| Power of dimension
public export
dimPow : Dimensions -> Integer -> Dimensions
dimPow d n = MkDim
  (d.length * n)
  (d.mass * n)
  (d.time * n)
  (d.current * n)
  (d.temperature * n)
  (d.amount * n)
  (d.luminosity * n)

||| Square root of dimension (only valid if all exponents are even)
public export
dimSqrt : Dimensions -> Maybe Dimensions
dimSqrt d =
  if d.length `mod` 2 == 0 &&
     d.mass `mod` 2 == 0 &&
     d.time `mod` 2 == 0 &&
     d.current `mod` 2 == 0 &&
     d.temperature `mod` 2 == 0 &&
     d.amount `mod` 2 == 0 &&
     d.luminosity `mod` 2 == 0
  then Just (MkDim
    (d.length `div` 2)
    (d.mass `div` 2)
    (d.time `div` 2)
    (d.current `div` 2)
    (d.temperature `div` 2)
    (d.amount `div` 2)
    (d.luminosity `div` 2))
  else Nothing

||| Check if two dimensions are equal
public export
dimEq : Dimensions -> Dimensions -> Bool
dimEq a b =
  a.length == b.length &&
  a.mass == b.mass &&
  a.time == b.time &&
  a.current == b.current &&
  a.temperature == b.temperature &&
  a.amount == b.amount &&
  a.luminosity == b.luminosity

-- ============================================================================
-- COMMON DIMENSIONS
-- ============================================================================

public export Length : Dimensions
Length = MkDim 1 0 0 0 0 0 0

public export Mass : Dimensions
Mass = MkDim 0 1 0 0 0 0 0

public export Time : Dimensions
Time = MkDim 0 0 1 0 0 0 0

public export Current : Dimensions
Current = MkDim 0 0 0 1 0 0 0

public export Temperature : Dimensions
Temperature = MkDim 0 0 0 0 1 0 0

public export Amount : Dimensions
Amount = MkDim 0 0 0 0 0 1 0

public export Luminosity : Dimensions
Luminosity = MkDim 0 0 0 0 0 0 1

-- Derived dimensions
public export Velocity : Dimensions
Velocity = MkDim 1 0 (-1) 0 0 0 0  -- m/s

public export Acceleration : Dimensions
Acceleration = MkDim 1 0 (-2) 0 0 0 0  -- m/s²

public export Force : Dimensions
Force = MkDim 1 1 (-2) 0 0 0 0  -- N = kg·m/s²

public export Energy : Dimensions
Energy = MkDim 2 1 (-2) 0 0 0 0  -- J = kg·m²/s²

public export Power : Dimensions
Power = MkDim 2 1 (-3) 0 0 0 0  -- W = kg·m²/s³

public export Pressure : Dimensions
Pressure = MkDim (-1) 1 (-2) 0 0 0 0  -- Pa = kg/(m·s²)

public export Frequency : Dimensions
Frequency = MkDim 0 0 (-1) 0 0 0 0  -- Hz = 1/s

public export Area : Dimensions
Area = MkDim 2 0 0 0 0 0 0  -- m²

public export Volume : Dimensions
Volume = MkDim 3 0 0 0 0 0 0  -- m³

public export Density : Dimensions
Density = MkDim (-3) 1 0 0 0 0 0  -- kg/m³

public export ElectricCharge : Dimensions
ElectricCharge = MkDim 0 0 1 1 0 0 0  -- C = A·s

public export Voltage : Dimensions
Voltage = MkDim 2 1 (-3) (-1) 0 0 0  -- V = kg·m²/(A·s³)

public export Resistance : Dimensions
Resistance = MkDim 2 1 (-3) (-2) 0 0 0  -- Ω = kg·m²/(A²·s³)

-- ============================================================================
-- QUANTITY TYPE
-- ============================================================================

||| A physical quantity with value and dimension
public export
record Quantity (dim : Dimensions) where
  constructor MkQuantity
  value : Double
  -- The dimension is encoded in the type parameter

-- ============================================================================
-- CONSTRUCTORS
-- ============================================================================

||| Create a quantity from a raw value
export
quantity : Double -> Quantity dim
quantity v = MkQuantity v

||| Create a dimensionless quantity
export
dimensionless : Double -> Quantity Dimensionless
dimensionless = quantity

-- Common unit constructors
export meters : Double -> Quantity Length
meters = quantity

export kilograms : Double -> Quantity Mass
kilograms = quantity

export seconds : Double -> Quantity Time
seconds = quantity

export amperes : Double -> Quantity Current
amperes = quantity

export kelvins : Double -> Quantity Temperature
kelvins = quantity

export moles : Double -> Quantity Amount
moles = quantity

export candelas : Double -> Quantity Luminosity
candelas = quantity

-- Derived unit constructors
export metersPerSecond : Double -> Quantity Velocity
metersPerSecond = quantity

export metersPerSecondSquared : Double -> Quantity Acceleration
metersPerSecondSquared = quantity

export newtons : Double -> Quantity Force
newtons = quantity

export joules : Double -> Quantity Energy
joules = quantity

export watts : Double -> Quantity Power
watts = quantity

export pascals : Double -> Quantity Pressure
pascals = quantity

export hertz : Double -> Quantity Frequency
hertz = quantity

-- ============================================================================
-- ARITHMETIC
-- ============================================================================

||| Add two quantities of the same dimension
export
add : Quantity dim -> Quantity dim -> Quantity dim
add a b = MkQuantity (a.value + b.value)

||| Subtract two quantities of the same dimension
export
sub : Quantity dim -> Quantity dim -> Quantity dim
sub a b = MkQuantity (a.value - b.value)

||| Multiply two quantities (dimensions multiply)
export
mul : Quantity d1 -> Quantity d2 -> Quantity (dimMul d1 d2)
mul a b = MkQuantity (a.value * b.value)

||| Divide two quantities (dimensions divide)
export
div : Quantity d1 -> Quantity d2 -> Maybe (Quantity (dimDiv d1 d2))
div a b =
  if abs b.value < 1.0e-10 then Nothing
  else Just (MkQuantity (a.value / b.value))

||| Scale by dimensionless factor
export
scale : Double -> Quantity dim -> Quantity dim
scale k q = MkQuantity (k * q.value)

||| Negate a quantity
export
neg : Quantity dim -> Quantity dim
neg q = MkQuantity (negate q.value)

||| Absolute value
export
abs : Quantity dim -> Quantity dim
abs q = MkQuantity (Prelude.abs q.value)

||| Power (integer exponent)
export
pow : Quantity dim -> (n : Integer) -> Quantity (dimPow dim n)
pow q n = MkQuantity (Prelude.pow q.value (cast n))

||| Square root (only if dimension exponents are even)
export
sqrt : Quantity dim -> {auto prf : dimSqrt dim = Just dim'} -> Quantity dim'
sqrt q = MkQuantity (Prelude.sqrt q.value)

-- ============================================================================
-- COMPARISON
-- ============================================================================

||| Compare two quantities of same dimension
export
compare : Quantity dim -> Quantity dim -> Ordering
compare a b = Prelude.compare a.value b.value

export
lt : Quantity dim -> Quantity dim -> Bool
lt a b = a.value < b.value

export
gt : Quantity dim -> Quantity dim -> Bool
gt a b = a.value > b.value

export
eq : Quantity dim -> Quantity dim -> Bool
eq a b = a.value == b.value

export
approxEq : Quantity dim -> Quantity dim -> Double -> Bool
approxEq a b epsilon = Prelude.abs (a.value - b.value) < epsilon

-- ============================================================================
-- EXTRACTION
-- ============================================================================

||| Get the raw numeric value
export
getValue : Quantity dim -> Double
getValue q = q.value

-- ============================================================================
-- UNIT CONVERSIONS
-- ============================================================================

||| Conversion factor for a unit
public export
record UnitConversion (dim : Dimensions) where
  constructor MkConversion
  factor : Double  -- multiply SI value by this to get unit value
  offset : Double  -- add this after multiplication (for temperature)

||| Standard SI unit (factor = 1, offset = 0)
export
siUnit : UnitConversion dim
siUnit = MkConversion 1.0 0.0

||| Convert to different unit
export
convertTo : UnitConversion dim -> Quantity dim -> Double
convertTo conv q = q.value * conv.factor + conv.offset

||| Convert from different unit
export
convertFrom : UnitConversion dim -> Double -> Quantity dim
convertFrom conv v = MkQuantity ((v - conv.offset) / conv.factor)

-- Common conversions
export
centimeters : UnitConversion Length
centimeters = MkConversion 100.0 0.0

export
kilometers : UnitConversion Length
kilometers = MkConversion 0.001 0.0

export
miles : UnitConversion Length
miles = MkConversion 0.000621371 0.0

export
feet : UnitConversion Length
feet = MkConversion 3.28084 0.0

export
grams : UnitConversion Mass
grams = MkConversion 1000.0 0.0

export
pounds : UnitConversion Mass
pounds = MkConversion 2.20462 0.0

export
minutes : UnitConversion Time
minutes = MkConversion (1.0 / 60.0) 0.0

export
hours : UnitConversion Time
hours = MkConversion (1.0 / 3600.0) 0.0

export
celsius : UnitConversion Temperature
celsius = MkConversion 1.0 (-273.15)

export
fahrenheit : UnitConversion Temperature
fahrenheit = MkConversion 1.8 (-459.67)

-- ============================================================================
-- COMMON PHYSICS FORMULAS
-- ============================================================================

||| Kinetic energy: KE = 0.5 * m * v²
export
kineticEnergy : Quantity Mass -> Quantity Velocity -> Quantity Energy
kineticEnergy m v = scale 0.5 (mul m (mul v v))

||| Force: F = m * a
export
force : Quantity Mass -> Quantity Acceleration -> Quantity Force
force = mul

||| Work: W = F * d
export
work : Quantity Force -> Quantity Length -> Quantity Energy
work = mul

||| Power: P = W / t
export
power : Quantity Energy -> Quantity Time -> Maybe (Quantity Power)
power = div

||| Ohm's law: V = I * R
export
ohmsLaw : Quantity Current -> Quantity Resistance -> Quantity Voltage
ohmsLaw = mul

||| Density: ρ = m / V
export
density : Quantity Mass -> Quantity Volume -> Maybe (Quantity Density)
density = div
