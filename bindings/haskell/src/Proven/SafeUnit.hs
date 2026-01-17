{- SPDX-License-Identifier: PMPL-1.0 -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Safe unit conversion operations.
--
-- Provides secure unit conversions with validation for
-- length, mass, temperature, time, and data units.
module Proven.SafeUnit
  ( -- * Types
    LengthUnit(..)
  , MassUnit(..)
  , TemperatureUnit(..)
  , TimeUnit(..)
  , DataUnit(..)
  , Quantity(..)
    -- * Construction
  , quantity
  , meters
  , kilometers
  , miles
  , feet
  , inches
  , kilograms
  , pounds
  , ounces
  , grams
  , celsius
  , fahrenheit
  , kelvin
  , seconds
  , minutes
  , hours
  , days
  , bytes
  , kilobytes
  , megabytes
  , gigabytes
  , terabytes
    -- * Conversion
  , convertLength
  , convertMass
  , convertTemperature
  , convertTime
  , convertData
    -- * To Base Units
  , toMeters
  , toKilograms
  , toCelsius
  , toSeconds
  , toBytes
    -- * Arithmetic
  , addQuantities
  , subQuantities
  , mulQuantity
  , divQuantity
    -- * Comparison
  , quantityEq
  , quantityLt
  , quantityLte
  , quantityGt
  , quantityGte
  ) where

import Proven.Core (ProvenError(..), Result)

-- | Length units.
data LengthUnit
  = Meters
  | Kilometers
  | Centimeters
  | Millimeters
  | Miles
  | Yards
  | Feet
  | Inches
  | NauticalMiles
  deriving (Eq, Show)

-- | Mass units.
data MassUnit
  = Kilograms
  | Grams
  | Milligrams
  | Pounds
  | Ounces
  | Tons
  | MetricTons
  deriving (Eq, Show)

-- | Temperature units.
data TemperatureUnit
  = Celsius
  | Fahrenheit
  | Kelvin
  deriving (Eq, Show)

-- | Time units.
data TimeUnit
  = Seconds
  | Milliseconds
  | Microseconds
  | Nanoseconds
  | Minutes
  | Hours
  | Days
  | Weeks
  deriving (Eq, Show)

-- | Data size units.
data DataUnit
  = Bytes
  | Kilobytes
  | Megabytes
  | Gigabytes
  | Terabytes
  | Petabytes
  | Kibibytes
  | Mebibytes
  | Gibibytes
  | Tebibytes
  deriving (Eq, Show)

-- | A quantity with a numeric value.
data Quantity a = Quantity
  { quantityValue :: !Double
  , quantityUnit  :: !a
  } deriving (Eq, Show)

-- | Create a quantity.
quantity :: Double -> a -> Quantity a
quantity = Quantity

-- Length constructors
meters :: Double -> Quantity LengthUnit
meters v = Quantity v Meters

kilometers :: Double -> Quantity LengthUnit
kilometers v = Quantity v Kilometers

miles :: Double -> Quantity LengthUnit
miles v = Quantity v Miles

feet :: Double -> Quantity LengthUnit
feet v = Quantity v Feet

inches :: Double -> Quantity LengthUnit
inches v = Quantity v Inches

-- Mass constructors
kilograms :: Double -> Quantity MassUnit
kilograms v = Quantity v Kilograms

pounds :: Double -> Quantity MassUnit
pounds v = Quantity v Pounds

ounces :: Double -> Quantity MassUnit
ounces v = Quantity v Ounces

grams :: Double -> Quantity MassUnit
grams v = Quantity v Grams

-- Temperature constructors
celsius :: Double -> Quantity TemperatureUnit
celsius v = Quantity v Celsius

fahrenheit :: Double -> Quantity TemperatureUnit
fahrenheit v = Quantity v Fahrenheit

kelvin :: Double -> Quantity TemperatureUnit
kelvin v = Quantity v Kelvin

-- Time constructors
seconds :: Double -> Quantity TimeUnit
seconds v = Quantity v Seconds

minutes :: Double -> Quantity TimeUnit
minutes v = Quantity v Minutes

hours :: Double -> Quantity TimeUnit
hours v = Quantity v Hours

days :: Double -> Quantity TimeUnit
days v = Quantity v Days

-- Data constructors
bytes :: Double -> Quantity DataUnit
bytes v = Quantity v Bytes

kilobytes :: Double -> Quantity DataUnit
kilobytes v = Quantity v Kilobytes

megabytes :: Double -> Quantity DataUnit
megabytes v = Quantity v Megabytes

gigabytes :: Double -> Quantity DataUnit
gigabytes v = Quantity v Gigabytes

terabytes :: Double -> Quantity DataUnit
terabytes v = Quantity v Terabytes

-- | Convert length to base unit (meters).
toMeters :: Quantity LengthUnit -> Double
toMeters (Quantity v unit) = case unit of
  Meters -> v
  Kilometers -> v * 1000
  Centimeters -> v / 100
  Millimeters -> v / 1000
  Miles -> v * 1609.344
  Yards -> v * 0.9144
  Feet -> v * 0.3048
  Inches -> v * 0.0254
  NauticalMiles -> v * 1852

-- | Convert length between units.
convertLength :: LengthUnit -> Quantity LengthUnit -> Quantity LengthUnit
convertLength target q =
  let inMeters = toMeters q
      converted = case target of
        Meters -> inMeters
        Kilometers -> inMeters / 1000
        Centimeters -> inMeters * 100
        Millimeters -> inMeters * 1000
        Miles -> inMeters / 1609.344
        Yards -> inMeters / 0.9144
        Feet -> inMeters / 0.3048
        Inches -> inMeters / 0.0254
        NauticalMiles -> inMeters / 1852
  in Quantity converted target

-- | Convert mass to base unit (kilograms).
toKilograms :: Quantity MassUnit -> Double
toKilograms (Quantity v unit) = case unit of
  Kilograms -> v
  Grams -> v / 1000
  Milligrams -> v / 1000000
  Pounds -> v * 0.453592
  Ounces -> v * 0.0283495
  Tons -> v * 907.185
  MetricTons -> v * 1000

-- | Convert mass between units.
convertMass :: MassUnit -> Quantity MassUnit -> Quantity MassUnit
convertMass target q =
  let inKg = toKilograms q
      converted = case target of
        Kilograms -> inKg
        Grams -> inKg * 1000
        Milligrams -> inKg * 1000000
        Pounds -> inKg / 0.453592
        Ounces -> inKg / 0.0283495
        Tons -> inKg / 907.185
        MetricTons -> inKg / 1000
  in Quantity converted target

-- | Convert temperature to Celsius.
toCelsius :: Quantity TemperatureUnit -> Double
toCelsius (Quantity v unit) = case unit of
  Celsius -> v
  Fahrenheit -> (v - 32) * 5 / 9
  Kelvin -> v - 273.15

-- | Convert temperature between units.
convertTemperature :: TemperatureUnit -> Quantity TemperatureUnit -> Quantity TemperatureUnit
convertTemperature target q =
  let inCelsius = toCelsius q
      converted = case target of
        Celsius -> inCelsius
        Fahrenheit -> inCelsius * 9 / 5 + 32
        Kelvin -> inCelsius + 273.15
  in Quantity converted target

-- | Convert time to base unit (seconds).
toSeconds :: Quantity TimeUnit -> Double
toSeconds (Quantity v unit) = case unit of
  Seconds -> v
  Milliseconds -> v / 1000
  Microseconds -> v / 1000000
  Nanoseconds -> v / 1000000000
  Minutes -> v * 60
  Hours -> v * 3600
  Days -> v * 86400
  Weeks -> v * 604800

-- | Convert time between units.
convertTime :: TimeUnit -> Quantity TimeUnit -> Quantity TimeUnit
convertTime target q =
  let inSeconds = toSeconds q
      converted = case target of
        Seconds -> inSeconds
        Milliseconds -> inSeconds * 1000
        Microseconds -> inSeconds * 1000000
        Nanoseconds -> inSeconds * 1000000000
        Minutes -> inSeconds / 60
        Hours -> inSeconds / 3600
        Days -> inSeconds / 86400
        Weeks -> inSeconds / 604800
  in Quantity converted target

-- | Convert data size to base unit (bytes).
toBytes :: Quantity DataUnit -> Double
toBytes (Quantity v unit) = case unit of
  Bytes -> v
  Kilobytes -> v * 1000
  Megabytes -> v * 1000000
  Gigabytes -> v * 1000000000
  Terabytes -> v * 1000000000000
  Petabytes -> v * 1000000000000000
  Kibibytes -> v * 1024
  Mebibytes -> v * 1048576
  Gibibytes -> v * 1073741824
  Tebibytes -> v * 1099511627776

-- | Convert data size between units.
convertData :: DataUnit -> Quantity DataUnit -> Quantity DataUnit
convertData target q =
  let inBytes = toBytes q
      converted = case target of
        Bytes -> inBytes
        Kilobytes -> inBytes / 1000
        Megabytes -> inBytes / 1000000
        Gigabytes -> inBytes / 1000000000
        Terabytes -> inBytes / 1000000000000
        Petabytes -> inBytes / 1000000000000000
        Kibibytes -> inBytes / 1024
        Mebibytes -> inBytes / 1048576
        Gibibytes -> inBytes / 1073741824
        Tebibytes -> inBytes / 1099511627776
  in Quantity converted target

-- | Add two quantities of the same type.
addQuantities :: Eq a => Quantity a -> Quantity a -> Result (Quantity a)
addQuantities (Quantity v1 u1) (Quantity v2 u2)
  | u1 /= u2 = Left (InvalidInput "Cannot add quantities with different units")
  | otherwise = Right (Quantity (v1 + v2) u1)

-- | Subtract two quantities of the same type.
subQuantities :: Eq a => Quantity a -> Quantity a -> Result (Quantity a)
subQuantities (Quantity v1 u1) (Quantity v2 u2)
  | u1 /= u2 = Left (InvalidInput "Cannot subtract quantities with different units")
  | otherwise = Right (Quantity (v1 - v2) u1)

-- | Multiply quantity by scalar.
mulQuantity :: Double -> Quantity a -> Quantity a
mulQuantity scalar (Quantity v u) = Quantity (v * scalar) u

-- | Divide quantity by scalar.
divQuantity :: Quantity a -> Double -> Result (Quantity a)
divQuantity _ 0 = Left DivisionByZero
divQuantity (Quantity v u) scalar = Right (Quantity (v / scalar) u)

-- | Check if two quantities are equal (same unit and value).
quantityEq :: Eq a => Quantity a -> Quantity a -> Bool
quantityEq (Quantity v1 u1) (Quantity v2 u2) = u1 == u2 && abs (v1 - v2) < 1e-10

-- | Check if first quantity is less than second.
quantityLt :: Eq a => Quantity a -> Quantity a -> Maybe Bool
quantityLt (Quantity v1 u1) (Quantity v2 u2)
  | u1 /= u2 = Nothing
  | otherwise = Just (v1 < v2)

-- | Check if first quantity is less than or equal to second.
quantityLte :: Eq a => Quantity a -> Quantity a -> Maybe Bool
quantityLte (Quantity v1 u1) (Quantity v2 u2)
  | u1 /= u2 = Nothing
  | otherwise = Just (v1 <= v2)

-- | Check if first quantity is greater than second.
quantityGt :: Eq a => Quantity a -> Quantity a -> Maybe Bool
quantityGt (Quantity v1 u1) (Quantity v2 u2)
  | u1 /= u2 = Nothing
  | otherwise = Just (v1 > v2)

-- | Check if first quantity is greater than or equal to second.
quantityGte :: Eq a => Quantity a -> Quantity a -> Maybe Bool
quantityGte (Quantity v1 u1) (Quantity v2 u2)
  | u1 /= u2 = Nothing
  | otherwise = Just (v1 >= v2)
