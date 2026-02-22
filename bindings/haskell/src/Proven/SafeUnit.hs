{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe unit conversion operations via libproven FFI.
--
-- Physical unit conversions are performed by the Idris 2 verified core.
module Proven.SafeUnit
  ( LengthUnit(..)
  , TempUnit(..)
  , convertLength
  , convertTemperature
  ) where

import Proven.FFI (c_proven_unit_convert_length, c_proven_unit_convert_temp)
import Proven.FFI.Types (LengthUnitC(..), TempUnitC(..))
import Proven.Core (floatResultToMaybe)

-- | Length units supported by libproven.
data LengthUnit
  = Meters
  | Kilometers
  | Centimeters
  | Millimeters
  | Feet
  | Inches
  | Miles
  | Yards
  deriving (Eq, Show, Enum)

-- | Temperature units supported by libproven.
data TempUnit
  = Celsius
  | Fahrenheit
  | Kelvin
  deriving (Eq, Show, Enum)

-- | Convert length between units.
-- Delegates to @proven_unit_convert_length@ in libproven.
convertLength :: Double -> LengthUnit -> LengthUnit -> IO (Maybe Double)
convertLength value fromUnit toUnit =
  floatResultToMaybe <$>
    c_proven_unit_convert_length
      (realToFrac value)
      (LengthUnitC (fromIntegral (fromEnum fromUnit)))
      (LengthUnitC (fromIntegral (fromEnum toUnit)))

-- | Convert temperature between units.
-- Delegates to @proven_unit_convert_temp@ in libproven.
convertTemperature :: Double -> TempUnit -> TempUnit -> IO (Maybe Double)
convertTemperature value fromUnit toUnit =
  floatResultToMaybe <$>
    c_proven_unit_convert_temp
      (realToFrac value)
      (TempUnitC (fromIntegral (fromEnum fromUnit)))
      (TempUnitC (fromIntegral (fromEnum toUnit)))
