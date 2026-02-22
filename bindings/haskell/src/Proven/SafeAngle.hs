{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe angle conversion operations via libproven FFI.
--
-- Angle conversions and normalization are performed by the
-- Idris 2 verified core.
module Proven.SafeAngle
  ( degToRad
  , radToDeg
  , normalizeDegrees
  , normalizeRadians
  ) where

import Proven.FFI (c_proven_angle_deg_to_rad, c_proven_angle_rad_to_deg,
                   c_proven_angle_normalize_degrees, c_proven_angle_normalize_radians)

-- | Convert degrees to radians.
-- Delegates to @proven_angle_deg_to_rad@ in libproven.
degToRad :: Double -> IO Double
degToRad d = realToFrac <$> c_proven_angle_deg_to_rad (realToFrac d)

-- | Convert radians to degrees.
-- Delegates to @proven_angle_rad_to_deg@ in libproven.
radToDeg :: Double -> IO Double
radToDeg r = realToFrac <$> c_proven_angle_rad_to_deg (realToFrac r)

-- | Normalize angle to [0, 360) degrees.
-- Delegates to @proven_angle_normalize_degrees@ in libproven.
normalizeDegrees :: Double -> IO Double
normalizeDegrees d = realToFrac <$> c_proven_angle_normalize_degrees (realToFrac d)

-- | Normalize angle to [0, 2*pi) radians.
-- Delegates to @proven_angle_normalize_radians@ in libproven.
normalizeRadians :: Double -> IO Double
normalizeRadians r = realToFrac <$> c_proven_angle_normalize_radians (realToFrac r)
