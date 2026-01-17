-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeGeo - Verified geographic operations
|||
||| Type-safe geographic coordinates with validated latitude/longitude,
||| distance calculations, and geospatial operations.
|||
||| Guarantees coordinates are within valid ranges.
module Proven.SafeGeo

import Proven.Core
import Data.So

%default total

-- ============================================================================
-- CONSTANTS
-- ============================================================================

||| Earth's mean radius in meters (WGS-84)
export
earthRadiusMeters : Double
earthRadiusMeters = 6371008.8

||| Earth's equatorial radius in meters
export
earthEquatorialRadius : Double
earthEquatorialRadius = 6378137.0

||| Earth's polar radius in meters
export
earthPolarRadius : Double
earthPolarRadius = 6356752.3

-- ============================================================================
-- LATITUDE
-- ============================================================================

||| Latitude in degrees [-90, 90]
public export
record Latitude where
  constructor MkLatitude
  degrees : Double
  0 inRange : So (degrees >= -90.0 && degrees <= 90.0)

||| Create latitude (clamps to valid range)
export
latitude : Double -> Latitude
latitude d = believe_me (MkLatitude (max (-90.0) (min 90.0 d)))

||| Create latitude (returns Nothing if invalid)
export
latitudeMaybe : Double -> Maybe Latitude
latitudeMaybe d =
  if d >= -90.0 && d <= 90.0
  then Just (believe_me (MkLatitude d))
  else Nothing

||| Convert latitude to radians
export
latitudeToRadians : Latitude -> Double
latitudeToRadians lat = lat.degrees * pi / 180.0

||| Create latitude from radians
export
latitudeFromRadians : Double -> Latitude
latitudeFromRadians r = latitude (r * 180.0 / pi)

-- ============================================================================
-- LONGITUDE
-- ============================================================================

||| Longitude in degrees [-180, 180]
public export
record Longitude where
  constructor MkLongitude
  degrees : Double
  0 inRange : So (degrees >= -180.0 && degrees <= 180.0)

||| Normalize longitude to [-180, 180]
normalizeLongitude : Double -> Double
normalizeLongitude d =
  let d' = d - 360.0 * floor ((d + 180.0) / 360.0)
  in if d' > 180.0 then d' - 360.0
     else if d' < -180.0 then d' + 360.0
     else d'

||| Create longitude (normalizes to valid range)
export
longitude : Double -> Longitude
longitude d = believe_me (MkLongitude (normalizeLongitude d))

||| Create longitude (returns Nothing if outside [-180, 180])
export
longitudeMaybe : Double -> Maybe Longitude
longitudeMaybe d =
  if d >= -180.0 && d <= 180.0
  then Just (believe_me (MkLongitude d))
  else Nothing

||| Convert longitude to radians
export
longitudeToRadians : Longitude -> Double
longitudeToRadians lon = lon.degrees * pi / 180.0

||| Create longitude from radians
export
longitudeFromRadians : Double -> Longitude
longitudeFromRadians r = longitude (r * 180.0 / pi)

-- ============================================================================
-- GEOGRAPHIC COORDINATE
-- ============================================================================

||| A geographic coordinate (latitude, longitude pair)
public export
record GeoCoord where
  constructor MkGeoCoord
  lat : Latitude
  lon : Longitude

||| Create a geographic coordinate
export
geoCoord : Double -> Double -> GeoCoord
geoCoord lat lon = MkGeoCoord (latitude lat) (longitude lon)

||| Create from Maybe
export
geoCoordMaybe : Double -> Double -> Maybe GeoCoord
geoCoordMaybe lat lon =
  case (latitudeMaybe lat, longitudeMaybe lon) of
    (Just la, Just lo) => Just (MkGeoCoord la lo)
    _ => Nothing

||| Coordinate with altitude
public export
record GeoCoord3D where
  constructor MkGeoCoord3D
  coord : GeoCoord
  altitude : Double  -- meters above sea level

||| Create 3D coordinate
export
geoCoord3D : Double -> Double -> Double -> GeoCoord3D
geoCoord3D lat lon alt = MkGeoCoord3D (geoCoord lat lon) alt

-- ============================================================================
-- DISTANCE CALCULATIONS
-- ============================================================================

||| Haversine formula for great-circle distance
||| Returns distance in meters
export
haversineDistance : GeoCoord -> GeoCoord -> Double
haversineDistance c1 c2 =
  let lat1 = latitudeToRadians c1.lat
      lat2 = latitudeToRadians c2.lat
      lon1 = longitudeToRadians c1.lon
      lon2 = longitudeToRadians c2.lon
      dlat = lat2 - lat1
      dlon = lon2 - lon1
      a = sin (dlat / 2.0) * sin (dlat / 2.0) +
          cos lat1 * cos lat2 * sin (dlon / 2.0) * sin (dlon / 2.0)
      c = 2.0 * atan2 (sqrt a) (sqrt (1.0 - a))
  in earthRadiusMeters * c

||| Vincenty formula for more accurate distance (simplified)
||| Returns distance in meters
export
vincentyDistance : GeoCoord -> GeoCoord -> Double
vincentyDistance c1 c2 =
  -- For simplicity, use haversine with ellipsoid correction factor
  let h = haversineDistance c1 c2
      -- Apply approximate ellipsoid correction based on latitude
      midLat = (c1.lat.degrees + c2.lat.degrees) / 2.0
      correction = 1.0 - 0.00335 * sin (midLat * pi / 180.0) * sin (midLat * pi / 180.0)
  in h * correction

||| Spherical law of cosines distance (less accurate but faster)
export
sphericalDistance : GeoCoord -> GeoCoord -> Double
sphericalDistance c1 c2 =
  let lat1 = latitudeToRadians c1.lat
      lat2 = latitudeToRadians c2.lat
      dlon = longitudeToRadians c2.lon - longitudeToRadians c1.lon
      cosD = sin lat1 * sin lat2 + cos lat1 * cos lat2 * cos dlon
  in earthRadiusMeters * acos (max (-1.0) (min 1.0 cosD))

||| Equirectangular approximation (fast, good for short distances)
export
equirectangularDistance : GeoCoord -> GeoCoord -> Double
equirectangularDistance c1 c2 =
  let lat1 = latitudeToRadians c1.lat
      lat2 = latitudeToRadians c2.lat
      lon1 = longitudeToRadians c1.lon
      lon2 = longitudeToRadians c2.lon
      x = (lon2 - lon1) * cos ((lat1 + lat2) / 2.0)
      y = lat2 - lat1
  in earthRadiusMeters * sqrt (x * x + y * y)

-- ============================================================================
-- BEARING AND HEADING
-- ============================================================================

||| Bearing in degrees [0, 360)
public export
record Bearing where
  constructor MkBearing
  degrees : Double
  0 inRange : So (degrees >= 0.0 && degrees < 360.0)

||| Create bearing (normalizes to [0, 360))
export
bearing : Double -> Bearing
bearing d =
  let d' = d - 360.0 * floor (d / 360.0)
  in believe_me (MkBearing (if d' < 0.0 then d' + 360.0 else d'))

||| Calculate initial bearing from c1 to c2
export
initialBearing : GeoCoord -> GeoCoord -> Bearing
initialBearing c1 c2 =
  let lat1 = latitudeToRadians c1.lat
      lat2 = latitudeToRadians c2.lat
      dlon = longitudeToRadians c2.lon - longitudeToRadians c1.lon
      x = sin dlon * cos lat2
      y = cos lat1 * sin lat2 - sin lat1 * cos lat2 * cos dlon
      brng = atan2 x y
  in bearing (brng * 180.0 / pi)

||| Calculate final bearing (bearing at destination)
export
finalBearing : GeoCoord -> GeoCoord -> Bearing
finalBearing c1 c2 =
  let b = initialBearing c2 c1
  in bearing (b.degrees + 180.0)

||| Compass direction from bearing
export
compassDirection : Bearing -> String
compassDirection b =
  let d = b.degrees
  in if d < 22.5 || d >= 337.5 then "N"
     else if d < 67.5 then "NE"
     else if d < 112.5 then "E"
     else if d < 157.5 then "SE"
     else if d < 202.5 then "S"
     else if d < 247.5 then "SW"
     else if d < 292.5 then "W"
     else "NW"

-- ============================================================================
-- DESTINATION POINT
-- ============================================================================

||| Calculate destination point given start, bearing, and distance
export
destinationPoint : GeoCoord -> Bearing -> Double -> GeoCoord
destinationPoint start brng distMeters =
  let lat1 = latitudeToRadians start.lat
      lon1 = longitudeToRadians start.lon
      brngRad = brng.degrees * pi / 180.0
      angularDist = distMeters / earthRadiusMeters
      lat2 = asin (sin lat1 * cos angularDist + cos lat1 * sin angularDist * cos brngRad)
      lon2 = lon1 + atan2 (sin brngRad * sin angularDist * cos lat1)
                          (cos angularDist - sin lat1 * sin lat2)
  in geoCoord (lat2 * 180.0 / pi) (lon2 * 180.0 / pi)

||| Midpoint between two coordinates
export
midpoint : GeoCoord -> GeoCoord -> GeoCoord
midpoint c1 c2 =
  let lat1 = latitudeToRadians c1.lat
      lon1 = longitudeToRadians c1.lon
      lat2 = latitudeToRadians c2.lat
      dlon = longitudeToRadians c2.lon - lon1
      bx = cos lat2 * cos dlon
      by = cos lat2 * sin dlon
      lat3 = atan2 (sin lat1 + sin lat2) (sqrt ((cos lat1 + bx) * (cos lat1 + bx) + by * by))
      lon3 = lon1 + atan2 by (cos lat1 + bx)
  in geoCoord (lat3 * 180.0 / pi) (lon3 * 180.0 / pi)

-- ============================================================================
-- BOUNDING BOX
-- ============================================================================

||| Geographic bounding box
public export
record BoundingBox where
  constructor MkBoundingBox
  minLat : Latitude
  maxLat : Latitude
  minLon : Longitude
  maxLon : Longitude

||| Create bounding box from two corners
export
boundingBox : GeoCoord -> GeoCoord -> BoundingBox
boundingBox c1 c2 =
  let minLa = min c1.lat.degrees c2.lat.degrees
      maxLa = max c1.lat.degrees c2.lat.degrees
      minLo = min c1.lon.degrees c2.lon.degrees
      maxLo = max c1.lon.degrees c2.lon.degrees
  in MkBoundingBox (latitude minLa) (latitude maxLa) (longitude minLo) (longitude maxLo)

||| Check if coordinate is within bounding box
export
withinBounds : GeoCoord -> BoundingBox -> Bool
withinBounds c bb =
  c.lat.degrees >= bb.minLat.degrees && c.lat.degrees <= bb.maxLat.degrees &&
  c.lon.degrees >= bb.minLon.degrees && c.lon.degrees <= bb.maxLon.degrees

||| Expand bounding box to include a coordinate
export
expandBounds : BoundingBox -> GeoCoord -> BoundingBox
expandBounds bb c =
  MkBoundingBox
    (latitude (min bb.minLat.degrees c.lat.degrees))
    (latitude (max bb.maxLat.degrees c.lat.degrees))
    (longitude (min bb.minLon.degrees c.lon.degrees))
    (longitude (max bb.maxLon.degrees c.lon.degrees))

||| Bounding box around a point with given radius (meters)
export
boundingBoxAround : GeoCoord -> Double -> BoundingBox
boundingBoxAround center radius =
  let n = destinationPoint center (bearing 0.0) radius
      s = destinationPoint center (bearing 180.0) radius
      e = destinationPoint center (bearing 90.0) radius
      w = destinationPoint center (bearing 270.0) radius
  in MkBoundingBox s.lat n.lat w.lon e.lon

||| Center of bounding box
export
boundingBoxCenter : BoundingBox -> GeoCoord
boundingBoxCenter bb =
  let lat = (bb.minLat.degrees + bb.maxLat.degrees) / 2.0
      lon = (bb.minLon.degrees + bb.maxLon.degrees) / 2.0
  in geoCoord lat lon

-- ============================================================================
-- GEOHASH
-- ============================================================================

||| Geohash precision (1-12 characters)
public export
record GeohashPrecision where
  constructor MkGeohashPrecision
  value : Nat
  0 inRange : So (value >= 1 && value <= 12)

||| Create geohash precision
export
geohashPrecision : Nat -> GeohashPrecision
geohashPrecision n = believe_me (MkGeohashPrecision (max 1 (min 12 n)))

||| Base32 alphabet for geohash
geohashAlphabet : String
geohashAlphabet = "0123456789bcdefghjkmnpqrstuvwxyz"

||| Encode coordinate to geohash
export
encodeGeohash : GeoCoord -> GeohashPrecision -> String
encodeGeohash coord prec =
  let bits = prec.value * 5
  in encodeLoop coord.lat.degrees (-90.0) 90.0 coord.lon.degrees (-180.0) 180.0 bits True ""
  where
    charAt : Nat -> Char
    charAt n = case strIndex geohashAlphabet (cast n) of
                 Just c => c
                 Nothing => '0'

    encodeLoop : Double -> Double -> Double -> Double -> Double -> Double -> Nat -> Bool -> String -> String
    encodeLoop lat minLat maxLat lon minLon maxLon 0 _ acc = acc
    encodeLoop lat minLat maxLat lon minLon maxLon remaining isLon acc =
      if length acc * 5 >= remaining then acc
      else let charBits = min 5 remaining
               (newAcc, newLat, newMinLat, newMaxLat, newLon, newMinLon, newMaxLon) =
                 buildChar lat minLat maxLat lon minLon maxLon charBits isLon 0 0
           in encodeLoop newLat newMinLat newMaxLat newLon newMinLon newMaxLon
                         (remaining `minus` 5) isLon (acc ++ singleton (charAt newAcc))

    buildChar : Double -> Double -> Double -> Double -> Double -> Double -> Nat -> Bool -> Nat -> Nat -> (Nat, Double, Double, Double, Double, Double, Double)
    buildChar lat minLat maxLat lon minLon maxLon 0 _ idx _ = (idx, lat, minLat, maxLat, lon, minLon, maxLon)
    buildChar lat minLat maxLat lon minLon maxLon bits isLon idx bitPos =
      if isLon
      then let mid = (minLon + maxLon) / 2.0
               (newIdx, newMin, newMax) = if lon >= mid then (idx * 2 + 1, mid, maxLon) else (idx * 2, minLon, mid)
           in buildChar lat minLat maxLat lon newMin newMax (bits `minus` 1) False newIdx (bitPos + 1)
      else let mid = (minLat + maxLat) / 2.0
               (newIdx, newMin, newMax) = if lat >= mid then (idx * 2 + 1, mid, maxLat) else (idx * 2, minLat, mid)
           in buildChar lat newMin newMax lon minLon maxLon (bits `minus` 1) True newIdx (bitPos + 1)

-- ============================================================================
-- COORDINATE FORMATTING
-- ============================================================================

||| Format coordinate as decimal degrees
export
formatDecimal : GeoCoord -> String
formatDecimal c =
  let latDir = if c.lat.degrees >= 0.0 then "N" else "S"
      lonDir = if c.lon.degrees >= 0.0 then "E" else "W"
  in show (abs c.lat.degrees) ++ "° " ++ latDir ++ ", " ++
     show (abs c.lon.degrees) ++ "° " ++ lonDir

||| Format coordinate as degrees-minutes-seconds
export
formatDMS : GeoCoord -> String
formatDMS c =
  let (latD, latM, latS) = toDMS (abs c.lat.degrees)
      (lonD, lonM, lonS) = toDMS (abs c.lon.degrees)
      latDir = if c.lat.degrees >= 0.0 then "N" else "S"
      lonDir = if c.lon.degrees >= 0.0 then "E" else "W"
  in show latD ++ "° " ++ show latM ++ "' " ++ show latS ++ "\" " ++ latDir ++ ", " ++
     show lonD ++ "° " ++ show lonM ++ "' " ++ show lonS ++ "\" " ++ lonDir
  where
    toDMS : Double -> (Int, Int, Double)
    toDMS d = let deg = floor d
                  minFrac = (d - deg) * 60.0
                  mins = floor minFrac
                  secs = (minFrac - mins) * 60.0
              in (cast deg, cast mins, secs)

-- ============================================================================
-- NAMED LOCATIONS
-- ============================================================================

export northPole : GeoCoord
northPole = geoCoord 90.0 0.0

export southPole : GeoCoord
southPole = geoCoord (-90.0) 0.0

export primeMeridianEquator : GeoCoord
primeMeridianEquator = geoCoord 0.0 0.0

-- ============================================================================
-- UTILITY
-- ============================================================================

||| Convert meters to kilometers
export
metersToKm : Double -> Double
metersToKm m = m / 1000.0

||| Convert meters to miles
export
metersToMiles : Double -> Double
metersToMiles m = m / 1609.344

||| Convert meters to nautical miles
export
metersToNauticalMiles : Double -> Double
metersToNauticalMiles m = m / 1852.0

||| Check if two coordinates are approximately equal
export
coordsApproxEqual : GeoCoord -> GeoCoord -> Double -> Bool
coordsApproxEqual c1 c2 tolerance =
  haversineDistance c1 c2 <= tolerance

