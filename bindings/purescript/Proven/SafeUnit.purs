-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafeUnit - FFI bindings to libproven unit conversion operations
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafeUnit
  ( convertLength
  , convertTemp
  , LengthUnit
  , TempUnit
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))

-- | Length unit identifiers matching the Zig FFI enum.
type LengthUnit = Int

-- | Temperature unit identifiers matching the Zig FFI enum.
type TempUnit = Int

-- | Convert between length units (delegates to Idris 2).
foreign import convertLengthImpl :: Number -> Int -> Int -> { status :: Int, value :: Number }

convertLength :: Number -> LengthUnit -> LengthUnit -> Result Number ProvenError
convertLength value from to =
  let r = convertLengthImpl value from to
  in if r.status == 0 then Ok r.value else Err (InvalidInput "Length conversion failed")

-- | Convert between temperature units (delegates to Idris 2).
foreign import convertTempImpl :: Number -> Int -> Int -> { status :: Int, value :: Number }

convertTemp :: Number -> TempUnit -> TempUnit -> Result Number ProvenError
convertTemp value from to =
  let r = convertTempImpl value from to
  in if r.status == 0 then Ok r.value else Err (InvalidInput "Temperature conversion failed")

-- | Length unit constants (exported from JS FFI).
foreign import lengthUnits ::
  { meters :: Int
  , feet :: Int
  , inches :: Int
  , centimeters :: Int
  , millimeters :: Int
  , kilometers :: Int
  , miles :: Int
  , yards :: Int
  }

-- | Temperature unit constants (exported from JS FFI).
foreign import tempUnits ::
  { celsius :: Int
  , fahrenheit :: Int
  , kelvin :: Int
  }
