-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeGeo - Safe geographic coordinate handling
|||
||| This module provides safe operations for geographic coordinates
||| including validation, distance calculation, and bounding boxes.
module Proven.SafeGeo
import Data.String
import Data.List

import public Proven.Core
import public Proven.SafeFloat

%default total

--------------------------------------------------------------------------------
-- Geographic Types
--------------------------------------------------------------------------------

||| A geographic coordinate (latitude, longitude in degrees)
public export
record GeoCoord where
  constructor MkGeoCoord
  latitude : Double   -- -90 to 90
  longitude : Double  -- -180 to 180

||| A geographic bounding box
public export
record GeoBBox where
  constructor MkGeoBBox
  minLat : Double
  minLon : Double
  maxLat : Double
  maxLon : Double

public export
Eq GeoCoord where
  a == b = approxEqual 0.000001 a.latitude b.latitude
        && approxEqual 0.000001 a.longitude b.longitude

--------------------------------------------------------------------------------
-- Coordinate Validation
--------------------------------------------------------------------------------

||| Check if latitude is valid (-90 to 90)
public export
isValidLatitude : Double -> Bool
isValidLatitude lat = lat >= -90.0 && lat <= 90.0

||| Check if longitude is valid (-180 to 180)
public export
isValidLongitude : Double -> Bool
isValidLongitude lon = lon >= -180.0 && lon <= 180.0

||| Create a validated geographic coordinate
public export
mkGeoCoord : Double -> Double -> Maybe GeoCoord
mkGeoCoord lat lon =
  if isValidLatitude lat && isValidLongitude lon
    then Just (MkGeoCoord lat lon)
    else Nothing

||| Normalize longitude to -180 to 180 range
public export
normalizeLongitude : Double -> Double
normalizeLongitude lon =
  let normalized = lon - 360.0 * floor ((lon + 180.0) / 360.0)
  in if normalized == 180.0 then -180.0 else normalized

||| Create a coordinate with normalized longitude
public export
mkGeoCoordNormalized : Double -> Double -> Maybe GeoCoord
mkGeoCoordNormalized lat lon =
  if isValidLatitude lat
    then Just (MkGeoCoord lat (normalizeLongitude lon))
    else Nothing

--------------------------------------------------------------------------------
-- Distance Calculations
--------------------------------------------------------------------------------

||| Earth's radius in kilometers
earthRadiusKm : Double
earthRadiusKm = 6371.0

||| Convert degrees to radians
toRadians : Double -> Double
toRadians deg = deg * pi / 180.0

||| Haversine distance between two coordinates (in kilometers)
public export
haversineDistance : GeoCoord -> GeoCoord -> Double
haversineDistance c1 c2 =
  let lat1 = toRadians c1.latitude
      lat2 = toRadians c2.latitude
      dlat = toRadians (c2.latitude - c1.latitude)
      dlon = toRadians (c2.longitude - c1.longitude)
      a = sin (dlat / 2.0) * sin (dlat / 2.0) +
          cos lat1 * cos lat2 * sin (dlon / 2.0) * sin (dlon / 2.0)
      c = 2.0 * atan2 (sqrt a) (sqrt (1.0 - a))
  in earthRadiusKm * c

||| Equirectangular approximation distance (faster but less accurate)
public export
equirectangularDistance : GeoCoord -> GeoCoord -> Double
equirectangularDistance c1 c2 =
  let lat1 = toRadians c1.latitude
      lat2 = toRadians c2.latitude
      lon1 = toRadians c1.longitude
      lon2 = toRadians c2.longitude
      x = (lon2 - lon1) * cos ((lat1 + lat2) / 2.0)
      y = lat2 - lat1
  in earthRadiusKm * sqrt (x * x + y * y)

--------------------------------------------------------------------------------
-- Bounding Box Operations
--------------------------------------------------------------------------------

||| Create a bounding box from two corner points
public export
mkBBox : GeoCoord -> GeoCoord -> GeoBBox
mkBBox c1 c2 = MkGeoBBox
  (min c1.latitude c2.latitude)
  (min c1.longitude c2.longitude)
  (max c1.latitude c2.latitude)
  (max c1.longitude c2.longitude)

||| Check if a coordinate is within a bounding box
public export
inBBox : GeoCoord -> GeoBBox -> Bool
inBBox c bbox =
  c.latitude >= bbox.minLat &&
  c.latitude <= bbox.maxLat &&
  c.longitude >= bbox.minLon &&
  c.longitude <= bbox.maxLon

||| Get the center of a bounding box
public export
bboxCenter : GeoBBox -> GeoCoord
bboxCenter bbox = MkGeoCoord
  ((bbox.minLat + bbox.maxLat) / 2.0)
  ((bbox.minLon + bbox.maxLon) / 2.0)

||| Expand a bounding box by a distance in kilometers
public export
expandBBox : Double -> GeoBBox -> GeoBBox
expandBBox km bbox =
  let latDelta = km / 111.0  -- Approximate km per degree latitude
      lonDelta = km / (111.0 * cos (toRadians ((bbox.minLat + bbox.maxLat) / 2.0)))
  in MkGeoBBox
       (max (-90.0) (bbox.minLat - latDelta))
       (max (-180.0) (bbox.minLon - lonDelta))
       (min 90.0 (bbox.maxLat + latDelta))
       (min 180.0 (bbox.maxLon + lonDelta))

--------------------------------------------------------------------------------
-- Bearing and Direction
--------------------------------------------------------------------------------

||| Calculate initial bearing from one point to another (in degrees)
public export
bearing : GeoCoord -> GeoCoord -> Double
bearing c1 c2 =
  let lat1 = toRadians c1.latitude
      lat2 = toRadians c2.latitude
      dlon = toRadians (c2.longitude - c1.longitude)
      y = sin dlon * cos lat2
      x = cos lat1 * sin lat2 - sin lat1 * cos lat2 * cos dlon
      brng = atan2 y x
  in normalizeBearing (brng * 180.0 / pi)
  where
    normalizeBearing : Double -> Double
    normalizeBearing b = if b < 0.0 then b + 360.0 else b

||| Calculate destination point given bearing and distance
public export
destination : GeoCoord -> Double -> Double -> GeoCoord
destination start bearingDeg distKm =
  let lat1 = toRadians start.latitude
      lon1 = toRadians start.longitude
      brng = toRadians bearingDeg
      d = distKm / earthRadiusKm
      lat2 = asin (sin lat1 * cos d + cos lat1 * sin d * cos brng)
      lon2 = lon1 + atan2 (sin brng * sin d * cos lat1) (cos d - sin lat1 * sin lat2)
  in MkGeoCoord (lat2 * 180.0 / pi) (normalizeLongitude (lon2 * 180.0 / pi))

--------------------------------------------------------------------------------
-- Well-Known Coordinates
--------------------------------------------------------------------------------

||| Null Island (0, 0)
public export
nullIsland : GeoCoord
nullIsland = MkGeoCoord 0.0 0.0

public export
Show GeoCoord where
  show c = "(" ++ show c.latitude ++ ", " ++ show c.longitude ++ ")"
