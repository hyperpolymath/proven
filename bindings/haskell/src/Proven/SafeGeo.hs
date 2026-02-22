{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe geographic coordinate operations via libproven FFI.
--
-- Coordinate validation and distance calculation (Haversine) are performed
-- by the Idris 2 verified core.
module Proven.SafeGeo
  ( Coordinate(..)
  , validateCoordinate
  , geoDistance
  , inBounds
  ) where

import Foreign
import Foreign.C.Types
import Proven.FFI (c_proven_geo_validate, c_proven_geo_distance,
                   c_proven_geo_in_bounds)
import Proven.FFI.Types (GeoCoordinate(..), GeoResult(..))
import Proven.Core (floatResultToMaybe)

-- | A validated geographic coordinate.
data Coordinate = Coordinate
  { coordLat :: !Double  -- ^ Latitude (-90 to 90)
  , coordLon :: !Double  -- ^ Longitude (-180 to 180)
  } deriving (Eq, Show)

-- | Validate and normalize a geographic coordinate.
-- Delegates to @proven_geo_validate@ in libproven.
validateCoordinate :: Double -> Double -> IO (Maybe Coordinate)
validateCoordinate lat lon = do
  result <- c_proven_geo_validate (realToFrac lat) (realToFrac lon)
  if grStatusRaw result == 0
    then let c = grCoordinate result
         in return (Just (Coordinate (realToFrac (geoLat c))
                                     (realToFrac (geoLon c))))
    else return Nothing

-- | Calculate distance between two coordinates in meters (Haversine formula).
-- Delegates to @proven_geo_distance@ in libproven.
geoDistance :: Coordinate -> Coordinate -> IO (Maybe Double)
geoDistance (Coordinate lat1 lon1) (Coordinate lat2 lon2) =
  alloca $ \ptrA -> alloca $ \ptrB -> do
    poke ptrA (GeoCoordinate (realToFrac lat1) (realToFrac lon1))
    poke ptrB (GeoCoordinate (realToFrac lat2) (realToFrac lon2))
    floatResultToMaybe <$> c_proven_geo_distance ptrA ptrB

-- | Check if a coordinate is within a bounding box.
-- Delegates to @proven_geo_in_bounds@ in libproven.
inBounds :: Coordinate -> Double -> Double -> Double -> Double -> IO Bool
inBounds (Coordinate lat lon) minLat maxLat minLon maxLon =
  alloca $ \ptr -> do
    poke ptr (GeoCoordinate (realToFrac lat) (realToFrac lon))
    result <- c_proven_geo_in_bounds ptr
                (realToFrac minLat) (realToFrac maxLat)
                (realToFrac minLon) (realToFrac maxLon)
    return (result /= 0)
