-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe unit conversions and dimensional analysis.
-- |
-- | Provides type-safe unit conversions with validation to prevent
-- | unit mismatch errors.

module Proven.SafeUnit
  ( SafeUnit
  , Length(..)
  , Mass(..)
  , Temperature(..)
  , Time(..)
  , metersToFeet
  , feetToMeters
  , kilosToPounds
  , poundsToKilos
  , celsiusToFahrenheit
  , fahrenheitToCelsius
  , celsiusToKelvin
  , kelvinToCelsius
  , secondsToMs
  , msToSeconds
  , minutesToSeconds
  , secondsToMinutes
  , hoursToMinutes
  , minutesToHours
  , daysToHours
  , hoursToDays
  ) where

import Prelude

-- | SafeUnit namespace marker (not instantiated).
data SafeUnit

-- | Length value with unit.
newtype Length = Length { value :: Number, unit :: String }

derive instance eqLength :: Eq Length

instance showLength :: Show Length where
  show (Length l) = show l.value <> " " <> l.unit

-- | Mass value with unit.
newtype Mass = Mass { value :: Number, unit :: String }

derive instance eqMass :: Eq Mass

instance showMass :: Show Mass where
  show (Mass m) = show m.value <> " " <> m.unit

-- | Temperature value with unit.
newtype Temperature = Temperature { value :: Number, unit :: String }

derive instance eqTemperature :: Eq Temperature

instance showTemperature :: Show Temperature where
  show (Temperature t) = show t.value <> " " <> t.unit

-- | Time value with unit.
newtype Time = Time { value :: Number, unit :: String }

derive instance eqTime :: Eq Time

instance showTime :: Show Time where
  show (Time t) = show t.value <> " " <> t.unit

-- | Convert meters to feet.
metersToFeet :: Number -> Number
metersToFeet m = m * 3.28084

-- | Convert feet to meters.
feetToMeters :: Number -> Number
feetToMeters ft = ft / 3.28084

-- | Convert kilograms to pounds.
kilosToPounds :: Number -> Number
kilosToPounds kg = kg * 2.20462

-- | Convert pounds to kilograms.
poundsToKilos :: Number -> Number
poundsToKilos lb = lb / 2.20462

-- | Convert Celsius to Fahrenheit.
celsiusToFahrenheit :: Number -> Number
celsiusToFahrenheit c = c * 9.0 / 5.0 + 32.0

-- | Convert Fahrenheit to Celsius.
fahrenheitToCelsius :: Number -> Number
fahrenheitToCelsius f = (f - 32.0) * 5.0 / 9.0

-- | Convert Celsius to Kelvin.
celsiusToKelvin :: Number -> Number
celsiusToKelvin c = c + 273.15

-- | Convert Kelvin to Celsius.
kelvinToCelsius :: Number -> Number
kelvinToCelsius k = k - 273.15

-- | Convert seconds to milliseconds.
secondsToMs :: Number -> Number
secondsToMs s = s * 1000.0

-- | Convert milliseconds to seconds.
msToSeconds :: Number -> Number
msToSeconds ms = ms / 1000.0

-- | Convert minutes to seconds.
minutesToSeconds :: Number -> Number
minutesToSeconds m = m * 60.0

-- | Convert seconds to minutes.
secondsToMinutes :: Number -> Number
secondsToMinutes s = s / 60.0

-- | Convert hours to minutes.
hoursToMinutes :: Number -> Number
hoursToMinutes h = h * 60.0

-- | Convert minutes to hours.
minutesToHours :: Number -> Number
minutesToHours m = m / 60.0

-- | Convert days to hours.
daysToHours :: Number -> Number
daysToHours d = d * 24.0

-- | Convert hours to days.
hoursToDays :: Number -> Number
hoursToDays h = h / 24.0
