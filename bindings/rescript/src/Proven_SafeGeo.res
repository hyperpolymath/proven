// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeGeo - Geographic coordinate operations that cannot crash.
 *
 * Provides safe geographic calculations including distance (Haversine formula),
 * bearing, and bounding box operations with proper validation.
 */

/** Error types for geographic operations */
type geoError =
  | InvalidLatitude
  | InvalidLongitude
  | InvalidCoordinate

/** Geographic coordinate (latitude, longitude in degrees) */
type coordinate = {
  lat: float, // -90 to 90
  lon: float, // -180 to 180
}

/** Earth radius in meters */
let earthRadiusMeters = 6371000.0

/** Earth radius in kilometers */
let earthRadiusKm = 6371.0

/** Convert degrees to radians */
let toRadians = (degrees: float): float => {
  degrees *. Js.Math._PI /. 180.0
}

/** Convert radians to degrees */
let toDegrees = (radians: float): float => {
  radians *. 180.0 /. Js.Math._PI
}

/** Validate latitude is within range [-90, 90] */
let isValidLatitude = (lat: float): bool => {
  lat >= -90.0 && lat <= 90.0
}

/** Validate longitude is within range [-180, 180] */
let isValidLongitude = (lon: float): bool => {
  lon >= -180.0 && lon <= 180.0
}

/** Create a coordinate with validation */
let makeCoordinate = (lat: float, lon: float): result<coordinate, geoError> => {
  if !isValidLatitude(lat) {
    Error(InvalidLatitude)
  } else if !isValidLongitude(lon) {
    Error(InvalidLongitude)
  } else {
    Ok({lat: lat, lon: lon})
  }
}

/** Create a coordinate, clamping values to valid ranges */
let makeCoordinateClamped = (lat: float, lon: float): coordinate => {
  let clampedLat = Js.Math.max_float(-90.0, Js.Math.min_float(90.0, lat))
  let clampedLon = Js.Math.max_float(-180.0, Js.Math.min_float(180.0, lon))
  {lat: clampedLat, lon: clampedLon}
}

/** Create a coordinate without validation (unsafe but convenient for known-valid data) */
let makeCoordinateUnsafe = (lat: float, lon: float): coordinate => {
  {lat: lat, lon: lon}
}

/** Haversine distance between two coordinates (in meters) */
let distanceMeters = (coord1: coordinate, coord2: coordinate): float => {
  let lat1 = toRadians(coord1.lat)
  let lat2 = toRadians(coord2.lat)
  let deltaLat = toRadians(coord2.lat -. coord1.lat)
  let deltaLon = toRadians(coord2.lon -. coord1.lon)

  let a =
    Js.Math.sin(deltaLat /. 2.0) *.
    Js.Math.sin(deltaLat /. 2.0) +.
    Js.Math.cos(lat1) *.
    Js.Math.cos(lat2) *.
    Js.Math.sin(deltaLon /. 2.0) *.
    Js.Math.sin(deltaLon /. 2.0)

  let c = 2.0 *. Js.Math.atan2(~y=Js.Math.sqrt(a), ~x=Js.Math.sqrt(1.0 -. a), ())

  earthRadiusMeters *. c
}

/** Haversine distance between two coordinates (in kilometers) */
let distanceKm = (coord1: coordinate, coord2: coordinate): float => {
  distanceMeters(coord1, coord2) /. 1000.0
}

/** Haversine distance between two coordinates (in miles) */
let distanceMiles = (coord1: coordinate, coord2: coordinate): float => {
  distanceMeters(coord1, coord2) /. 1609.344
}

/** Initial bearing from coord1 to coord2 (in degrees, 0-360) */
let bearingTo = (coord1: coordinate, coord2: coordinate): float => {
  let lat1 = toRadians(coord1.lat)
  let lat2 = toRadians(coord2.lat)
  let deltaLon = toRadians(coord2.lon -. coord1.lon)

  let y = Js.Math.sin(deltaLon) *. Js.Math.cos(lat2)
  let x =
    Js.Math.cos(lat1) *. Js.Math.sin(lat2) -.
    Js.Math.sin(lat1) *. Js.Math.cos(lat2) *. Js.Math.cos(deltaLon)

  let bearing = toDegrees(Js.Math.atan2(~y, ~x, ()))
  mod_float(bearing +. 360.0, 360.0)
}

/** Midpoint between two coordinates */
let midpoint = (coord1: coordinate, coord2: coordinate): coordinate => {
  {
    lat: (coord1.lat +. coord2.lat) /. 2.0,
    lon: (coord1.lon +. coord2.lon) /. 2.0,
  }
}

/** Calculate destination point given start, bearing (degrees), and distance (meters) */
let destination = (
  start: coordinate,
  bearing: float,
  distanceM: float,
): coordinate => {
  let lat1 = toRadians(start.lat)
  let lon1 = toRadians(start.lon)
  let brng = toRadians(bearing)
  let angularDistance = distanceM /. earthRadiusMeters

  let lat2 =
    Js.Math.asin(
      Js.Math.sin(lat1) *. Js.Math.cos(angularDistance) +.
      Js.Math.cos(lat1) *. Js.Math.sin(angularDistance) *. Js.Math.cos(brng),
    )

  let lon2 =
    lon1 +.
    Js.Math.atan2(
      ~y=Js.Math.sin(brng) *. Js.Math.sin(angularDistance) *. Js.Math.cos(lat1),
      ~x=Js.Math.cos(angularDistance) -. Js.Math.sin(lat1) *. Js.Math.sin(lat2),
      (),
    )

  // Normalize longitude to -180 to 180
  let normalizedLon = mod_float(toDegrees(lon2) +. 540.0, 360.0) -. 180.0

  {lat: toDegrees(lat2), lon: normalizedLon}
}

/** Bounding box defined by southwest and northeast corners */
type boundingBox = {
  minLat: float,
  minLon: float,
  maxLat: float,
  maxLon: float,
}

/** Create a bounding box with validation */
let makeBoundingBox = (
  minLat: float,
  minLon: float,
  maxLat: float,
  maxLon: float,
): result<boundingBox, geoError> => {
  if !isValidLatitude(minLat) || !isValidLatitude(maxLat) {
    Error(InvalidLatitude)
  } else if !isValidLongitude(minLon) || !isValidLongitude(maxLon) {
    Error(InvalidLongitude)
  } else {
    Ok({
      minLat: minLat,
      minLon: minLon,
      maxLat: maxLat,
      maxLon: maxLon,
    })
  }
}

/** Create a bounding box from two coordinates */
let boundingBoxFromCoordinates = (coord1: coordinate, coord2: coordinate): boundingBox => {
  {
    minLat: Js.Math.min_float(coord1.lat, coord2.lat),
    minLon: Js.Math.min_float(coord1.lon, coord2.lon),
    maxLat: Js.Math.max_float(coord1.lat, coord2.lat),
    maxLon: Js.Math.max_float(coord1.lon, coord2.lon),
  }
}

/** Check if a coordinate is within a bounding box */
let containsCoordinate = (bbox: boundingBox, coord: coordinate): bool => {
  coord.lat >= bbox.minLat &&
  coord.lat <= bbox.maxLat &&
  coord.lon >= bbox.minLon &&
  coord.lon <= bbox.maxLon
}

/** Get the center of a bounding box */
let boundingBoxCenter = (bbox: boundingBox): coordinate => {
  {
    lat: (bbox.minLat +. bbox.maxLat) /. 2.0,
    lon: (bbox.minLon +. bbox.maxLon) /. 2.0,
  }
}

/** Get the southwest corner of a bounding box */
let southwestCorner = (bbox: boundingBox): coordinate => {
  {lat: bbox.minLat, lon: bbox.minLon}
}

/** Get the northeast corner of a bounding box */
let northeastCorner = (bbox: boundingBox): coordinate => {
  {lat: bbox.maxLat, lon: bbox.maxLon}
}

/** Check if two bounding boxes overlap */
let boundingBoxesOverlap = (bbox1: boundingBox, bbox2: boundingBox): bool => {
  !(
    bbox1.maxLat < bbox2.minLat ||
    bbox1.minLat > bbox2.maxLat ||
    bbox1.maxLon < bbox2.minLon ||
    bbox1.minLon > bbox2.maxLon
  )
}

/** Expand a bounding box by a margin (in degrees) */
let expandBoundingBox = (bbox: boundingBox, marginDegrees: float): boundingBox => {
  {
    minLat: Js.Math.max_float(-90.0, bbox.minLat -. marginDegrees),
    minLon: Js.Math.max_float(-180.0, bbox.minLon -. marginDegrees),
    maxLat: Js.Math.min_float(90.0, bbox.maxLat +. marginDegrees),
    maxLon: Js.Math.min_float(180.0, bbox.maxLon +. marginDegrees),
  }
}

/** Create a bounding box that encompasses multiple coordinates */
let boundingBoxFromCoordinateList = (coords: array<coordinate>): option<boundingBox> => {
  if Belt.Array.length(coords) == 0 {
    None
  } else {
    let first = Belt.Array.getUnsafe(coords, 0)
    let minLat = ref(first.lat)
    let maxLat = ref(first.lat)
    let minLon = ref(first.lon)
    let maxLon = ref(first.lon)

    Belt.Array.forEach(coords, coord => {
      if coord.lat < minLat.contents {
        minLat := coord.lat
      }
      if coord.lat > maxLat.contents {
        maxLat := coord.lat
      }
      if coord.lon < minLon.contents {
        minLon := coord.lon
      }
      if coord.lon > maxLon.contents {
        maxLon := coord.lon
      }
    })

    Some({
      minLat: minLat.contents,
      minLon: minLon.contents,
      maxLat: maxLat.contents,
      maxLon: maxLon.contents,
    })
  }
}

/** Convert coordinate to string representation */
let coordinateToString = (coord: coordinate): string => {
  let latDir = if coord.lat >= 0.0 {
    "N"
  } else {
    "S"
  }
  let lonDir = if coord.lon >= 0.0 {
    "E"
  } else {
    "W"
  }
  let latAbs = Js.Math.abs_float(coord.lat)
  let lonAbs = Js.Math.abs_float(coord.lon)
  `${Belt.Float.toString(latAbs)}${latDir}, ${Belt.Float.toString(lonAbs)}${lonDir}`
}

/** Parse a coordinate from string (supports "lat,lon" format) */
let parseCoordinate = (str: string): result<coordinate, geoError> => {
  let parts = Js.String2.split(str, ",")
  if Belt.Array.length(parts) != 2 {
    Error(InvalidCoordinate)
  } else {
    let latStr = Js.String2.trim(Belt.Array.getUnsafe(parts, 0))
    let lonStr = Js.String2.trim(Belt.Array.getUnsafe(parts, 1))

    switch (Belt.Float.fromString(latStr), Belt.Float.fromString(lonStr)) {
    | (Some(lat), Some(lon)) => makeCoordinate(lat, lon)
    | _ => Error(InvalidCoordinate)
    }
  }
}

/** Check if coordinate is on the equator */
let isOnEquator = (coord: coordinate): bool => {
  Js.Math.abs_float(coord.lat) < 0.0001
}

/** Check if coordinate is at the prime meridian */
let isOnPrimeMeridian = (coord: coordinate): bool => {
  Js.Math.abs_float(coord.lon) < 0.0001
}

/** Check if coordinate is in the Northern Hemisphere */
let isNorthernHemisphere = (coord: coordinate): bool => {
  coord.lat > 0.0
}

/** Check if coordinate is in the Southern Hemisphere */
let isSouthernHemisphere = (coord: coordinate): bool => {
  coord.lat < 0.0
}

/** Check if coordinate is in the Eastern Hemisphere */
let isEasternHemisphere = (coord: coordinate): bool => {
  coord.lon > 0.0
}

/** Check if coordinate is in the Western Hemisphere */
let isWesternHemisphere = (coord: coordinate): bool => {
  coord.lon < 0.0
}

/** Convert error to human-readable string */
let errorToString = (error: geoError): string => {
  switch error {
  | InvalidLatitude => "Invalid latitude (must be between -90 and 90)"
  | InvalidLongitude => "Invalid longitude (must be between -180 and 180)"
  | InvalidCoordinate => "Invalid coordinate format"
  }
}
