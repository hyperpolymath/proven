-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafeAngle - FFI bindings to libproven angle conversion operations
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafeAngle
  ( degToRad
  , radToDeg
  , normalizeDegrees
  , normalizeRadians
  ) where

import Prelude

-- | Convert degrees to radians (delegates to Idris 2).
foreign import degToRadImpl :: Number -> Number

degToRad :: Number -> Number
degToRad = degToRadImpl

-- | Convert radians to degrees (delegates to Idris 2).
foreign import radToDegImpl :: Number -> Number

radToDeg :: Number -> Number
radToDeg = radToDegImpl

-- | Normalize degrees to [0, 360) range (delegates to Idris 2).
foreign import normalizeDegreesImpl :: Number -> Number

normalizeDegrees :: Number -> Number
normalizeDegrees = normalizeDegreesImpl

-- | Normalize radians to [0, 2*pi) range (delegates to Idris 2).
foreign import normalizeRadiansImpl :: Number -> Number

normalizeRadians :: Number -> Number
normalizeRadians = normalizeRadiansImpl
