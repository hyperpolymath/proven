-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeGeo operations
|||
||| This module exports geographic coordinate operations to the C ABI
||| via Idris2's RefC backend. All functions are proven total and validate coordinates.
|||
||| Return conventions:
||| - Coordinate creation → (Int, lat, lon) where status 0 = success, 1 = invalid
||| - Distance → Double (kilometers)
||| - Bearing → Double (degrees 0-360, clockwise from north)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
|||
||| CRITICAL: Coordinate validation prevents invalid geographic calculations.
|||           Latitude: -90 to 90 degrees (south to north)
|||           Longitude: -180 to 180 degrees (west to east, wraps around)
|||
||| Distance calculations:
||| - Haversine: Accurate great-circle distance (slower)
||| - Equirectangular: Fast approximation (less accurate, good for short distances)
|||
||| Earth radius: 6371 km (mean radius)
module Proven.FFI.SafeGeo

import Proven.SafeGeo
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

export
proven_idris_geo_earth_radius_km : Double
proven_idris_geo_earth_radius_km = earthRadiusKm

export
proven_idris_geo_lat_min : Double
proven_idris_geo_lat_min = -90.0

export
proven_idris_geo_lat_max : Double
proven_idris_geo_lat_max = 90.0

export
proven_idris_geo_lon_min : Double
proven_idris_geo_lon_min = -180.0

export
proven_idris_geo_lon_max : Double
proven_idris_geo_lon_max = 180.0

export
proven_idris_geo_km_per_degree_lat : Double
proven_idris_geo_km_per_degree_lat = 111.0

--------------------------------------------------------------------------------
-- Coordinate Validation
--------------------------------------------------------------------------------

export
proven_idris_geo_is_valid_latitude : Double -> Int
proven_idris_geo_is_valid_latitude lat =
  encodeBool (isValidLatitude lat)

export
proven_idris_geo_is_valid_longitude : Double -> Int
proven_idris_geo_is_valid_longitude lon =
  encodeBool (isValidLongitude lon)

export
proven_idris_geo_is_valid_coord : Double -> Double -> Int
proven_idris_geo_is_valid_coord lat lon =
  encodeBool (isValidLatitude lat && isValidLongitude lon)

export
proven_idris_geo_normalize_longitude : Double -> Double
proven_idris_geo_normalize_longitude lon =
  normalizeLongitude lon

export
proven_idris_geo_clamp_latitude : Double -> Double
proven_idris_geo_clamp_latitude lat =
  if lat < -90.0 then -90.0
  else if lat > 90.0 then 90.0
  else lat

--------------------------------------------------------------------------------
-- Coordinate Creation
--------------------------------------------------------------------------------

export
proven_idris_geo_create_coord : Double -> Double -> (Int, Double, Double)
proven_idris_geo_create_coord lat lon =
  case mkGeoCoord lat lon of
    Nothing => (1, 0.0, 0.0)  -- Invalid
    Just c => (0, c.latitude, c.longitude)

export
proven_idris_geo_create_coord_normalized : Double -> Double -> (Int, Double, Double)
proven_idris_geo_create_coord_normalized lat lon =
  case mkGeoCoordNormalized lat lon of
    Nothing => (1, 0.0, 0.0)
    Just c => (0, c.latitude, c.longitude)

--------------------------------------------------------------------------------
-- Distance Calculations
--------------------------------------------------------------------------------

export
proven_idris_geo_haversine_distance : Double -> Double -> Double -> Double -> Double
proven_idris_geo_haversine_distance lat1 lon1 lat2 lon2 =
  let c1 = MkGeoCoord lat1 lon1
      c2 = MkGeoCoord lat2 lon2
  in haversineDistance c1 c2

export
proven_idris_geo_equirectangular_distance : Double -> Double -> Double -> Double -> Double
proven_idris_geo_equirectangular_distance lat1 lon1 lat2 lon2 =
  let c1 = MkGeoCoord lat1 lon1
      c2 = MkGeoCoord lat2 lon2
  in equirectangularDistance c1 c2

export
proven_idris_geo_distance : Double -> Double -> Double -> Double -> Double
proven_idris_geo_distance lat1 lon1 lat2 lon2 =
  proven_idris_geo_haversine_distance lat1 lon1 lat2 lon2

export
proven_idris_geo_distance_meters : Double -> Double -> Double -> Double -> Double
proven_idris_geo_distance_meters lat1 lon1 lat2 lon2 =
  proven_idris_geo_haversine_distance lat1 lon1 lat2 lon2 * 1000.0

--------------------------------------------------------------------------------
-- Bearing and Direction
--------------------------------------------------------------------------------

export
proven_idris_geo_bearing : Double -> Double -> Double -> Double -> Double
proven_idris_geo_bearing lat1 lon1 lat2 lon2 =
  let c1 = MkGeoCoord lat1 lon1
      c2 = MkGeoCoord lat2 lon2
  in bearing c1 c2

export
proven_idris_geo_destination : Double -> Double -> Double -> Double -> (Double, Double)
proven_idris_geo_destination lat lon bearingDeg distKm =
  let start = MkGeoCoord lat lon
      dest = destination start bearingDeg distKm
  in (dest.latitude, dest.longitude)

export
proven_idris_geo_is_valid_bearing : Double -> Int
proven_idris_geo_is_valid_bearing brng =
  encodeBool (brng >= 0.0 && brng < 360.0)

export
proven_idris_geo_normalize_bearing : Double -> Double
proven_idris_geo_normalize_bearing brng =
  let normalized = brng - 360.0 * floor (brng / 360.0)
  in if normalized < 0.0 then normalized + 360.0 else normalized

--------------------------------------------------------------------------------
-- Bounding Box Operations
--------------------------------------------------------------------------------

export
proven_idris_geo_create_bbox : Double -> Double -> Double -> Double -> (Double, Double, Double, Double)
proven_idris_geo_create_bbox lat1 lon1 lat2 lon2 =
  let c1 = MkGeoCoord lat1 lon1
      c2 = MkGeoCoord lat2 lon2
      bbox = mkBBox c1 c2
  in (bbox.minLat, bbox.minLon, bbox.maxLat, bbox.maxLon)

export
proven_idris_geo_in_bbox : Double -> Double -> Double -> Double -> Double -> Double -> Int
proven_idris_geo_in_bbox lat lon minLat minLon maxLat maxLon =
  let coord = MkGeoCoord lat lon
      bbox = MkGeoBBox minLat minLon maxLat maxLon
  in encodeBool (inBBox coord bbox)

export
proven_idris_geo_bbox_center : Double -> Double -> Double -> Double -> (Double, Double)
proven_idris_geo_bbox_center minLat minLon maxLat maxLon =
  let bbox = MkGeoBBox minLat minLon maxLat maxLon
      center = bboxCenter bbox
  in (center.latitude, center.longitude)

export
proven_idris_geo_expand_bbox : Double -> Double -> Double -> Double -> Double -> (Double, Double, Double, Double)
proven_idris_geo_expand_bbox minLat minLon maxLat maxLon km =
  let bbox = MkGeoBBox minLat minLon maxLat maxLon
      expanded = expandBBox km bbox
  in (expanded.minLat, expanded.minLon, expanded.maxLat, expanded.maxLon)

export
proven_idris_geo_bbox_width_km : Double -> Double -> Double -> Double -> Double
proven_idris_geo_bbox_width_km minLat minLon maxLat maxLon =
  let centerLat = (minLat + maxLat) / 2.0
      west = MkGeoCoord centerLat minLon
      east = MkGeoCoord centerLat maxLon
  in haversineDistance west east

export
proven_idris_geo_bbox_height_km : Double -> Double -> Double -> Double -> Double
proven_idris_geo_bbox_height_km minLat minLon maxLat maxLon =
  let centerLon = (minLon + maxLon) / 2.0
      south = MkGeoCoord minLat centerLon
      north = MkGeoCoord maxLat centerLon
  in haversineDistance south north

--------------------------------------------------------------------------------
-- Coordinate Comparison
--------------------------------------------------------------------------------

export
proven_idris_geo_coords_equal : Double -> Double -> Double -> Double -> Int
proven_idris_geo_coords_equal lat1 lon1 lat2 lon2 =
  let c1 = MkGeoCoord lat1 lon1
      c2 = MkGeoCoord lat2 lon2
  in encodeBool (c1 == c2)

export
proven_idris_geo_coords_approx_equal : Double -> Double -> Double -> Double -> Double -> Int
proven_idris_geo_coords_approx_equal lat1 lon1 lat2 lon2 tolerance =
  encodeBool (
    abs (lat1 - lat2) <= tolerance &&
    abs (lon1 - lon2) <= tolerance
  )

--------------------------------------------------------------------------------
-- Conversion Helpers
--------------------------------------------------------------------------------

export
proven_idris_geo_degrees_to_radians : Double -> Double
proven_idris_geo_degrees_to_radians deg =
  toRadians deg

export
proven_idris_geo_radians_to_degrees : Double -> Double
proven_idris_geo_radians_to_degrees rad =
  rad * 180.0 / pi

--------------------------------------------------------------------------------
-- Well-Known Coordinates
--------------------------------------------------------------------------------

export
proven_idris_geo_null_island_lat : Double
proven_idris_geo_null_island_lat = nullIsland.latitude

export
proven_idris_geo_null_island_lon : Double
proven_idris_geo_null_island_lon = nullIsland.longitude

export
proven_idris_geo_is_null_island : Double -> Double -> Int
proven_idris_geo_is_null_island lat lon =
  encodeBool (lat == 0.0 && lon == 0.0)

--------------------------------------------------------------------------------
-- Cardinal Direction Helpers
--------------------------------------------------------------------------------

export
proven_idris_geo_bearing_to_cardinal : Double -> String
proven_idris_geo_bearing_to_cardinal brng =
  let b = proven_idris_geo_normalize_bearing brng
  in if b < 22.5 then "N"
     else if b < 67.5 then "NE"
     else if b < 112.5 then "E"
     else if b < 157.5 then "SE"
     else if b < 202.5 then "S"
     else if b < 247.5 then "SW"
     else if b < 292.5 then "W"
     else if b < 337.5 then "NW"
     else "N"

export
proven_idris_geo_is_northward : Double -> Int
proven_idris_geo_is_northward brng =
  let b = proven_idris_geo_normalize_bearing brng
  in encodeBool (b < 90.0 || b >= 270.0)

export
proven_idris_geo_is_eastward : Double -> Int
proven_idris_geo_is_eastward brng =
  let b = proven_idris_geo_normalize_bearing brng
  in encodeBool (b >= 0.0 && b < 180.0)

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_geo_friendly_error : String -> String
proven_idris_geo_friendly_error errorMsg =
  if isInfixOf "latitude" (toLower errorMsg)
    then "Invalid latitude (must be -90 to 90 degrees)"
  else if isInfixOf "longitude" (toLower errorMsg)
    then "Invalid longitude (must be -180 to 180 degrees)"
  else if isInfixOf "bearing" (toLower errorMsg)
    then "Invalid bearing (must be 0-360 degrees)"
  else if isInfixOf "distance" (toLower errorMsg)
    then "Invalid distance calculation"
  else if isInfixOf "bbox" (toLower errorMsg) || isInfixOf "bounding" (toLower errorMsg)
    then "Invalid bounding box coordinates"
  else if isInfixOf "coord" (toLower errorMsg)
    then "Invalid geographic coordinate"
  else
    "Geographic operation error"
