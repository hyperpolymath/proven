// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeGeo - Geographic coordinates and calculations.
 *
 * Provides safe coordinate parsing, Haversine distance, and bearing calculations.
 * @module
 */

import { ok, err } from './result.js';

/** @type {number} Earth's mean radius in meters */
const EARTH_RADIUS = 6371000;

/**
 * Geographic coordinate.
 * @typedef {Object} Coordinate
 * @property {number} latitude - Latitude (-90 to 90)
 * @property {number} longitude - Longitude (-180 to 180)
 */

/**
 * Bounding box.
 * @typedef {Object} BoundingBox
 * @property {number} minLat - Minimum latitude
 * @property {number} maxLat - Maximum latitude
 * @property {number} minLon - Minimum longitude
 * @property {number} maxLon - Maximum longitude
 */

/**
 * Safe geographic operations.
 */
export class SafeGeo {
  /**
   * Create and validate a coordinate.
   *
   * @param {number} latitude - Latitude
   * @param {number} longitude - Longitude
   * @returns {{ ok: true, value: Coordinate } | { ok: false, error: string }}
   *
   * @example
   * SafeGeo.coordinate(37.7749, -122.4194)  // San Francisco
   */
  static coordinate(latitude, longitude) {
    if (!Number.isFinite(latitude) || !Number.isFinite(longitude)) {
      return err('Coordinates must be finite numbers');
    }

    if (latitude < -90 || latitude > 90) {
      return err('Latitude must be between -90 and 90');
    }

    if (longitude < -180 || longitude > 180) {
      return err('Longitude must be between -180 and 180');
    }

    return ok({ latitude, longitude });
  }

  /**
   * Parse a coordinate string.
   *
   * Supports formats: "lat,lon", "lat lon", "lat, lon"
   *
   * @param {string} str - Coordinate string
   * @returns {{ ok: true, value: Coordinate } | { ok: false, error: string }}
   */
  static parse(str) {
    if (typeof str !== 'string') {
      return err('Input must be a string');
    }

    const parts = str.split(/[,\s]+/).filter(Boolean);
    if (parts.length !== 2) {
      return err('Expected two numbers separated by comma or space');
    }

    const lat = parseFloat(parts[0]);
    const lon = parseFloat(parts[1]);

    return SafeGeo.coordinate(lat, lon);
  }

  /**
   * Calculate Haversine distance between two coordinates.
   *
   * @param {Coordinate} coordinateA - First coordinate
   * @param {Coordinate} coordinateB - Second coordinate
   * @returns {number} Distance in meters
   */
  static haversineDistance(coordinateA, coordinateB) {
    const lat1Rad = (coordinateA.latitude * Math.PI) / 180;
    const lat2Rad = (coordinateB.latitude * Math.PI) / 180;
    const deltaLat = ((coordinateB.latitude - coordinateA.latitude) * Math.PI) / 180;
    const deltaLon = ((coordinateB.longitude - coordinateA.longitude) * Math.PI) / 180;

    const haversineA =
      Math.sin(deltaLat / 2) * Math.sin(deltaLat / 2) +
      Math.cos(lat1Rad) * Math.cos(lat2Rad) * Math.sin(deltaLon / 2) * Math.sin(deltaLon / 2);

    const haversineC = 2 * Math.atan2(Math.sqrt(haversineA), Math.sqrt(1 - haversineA));

    return EARTH_RADIUS * haversineC;
  }

  /**
   * Calculate initial bearing from point A to point B.
   *
   * @param {Coordinate} coordinateA - Starting coordinate
   * @param {Coordinate} coordinateB - Destination coordinate
   * @returns {number} Bearing in degrees (0-360)
   */
  static bearing(coordinateA, coordinateB) {
    const lat1Rad = (coordinateA.latitude * Math.PI) / 180;
    const lat2Rad = (coordinateB.latitude * Math.PI) / 180;
    const deltaLon = ((coordinateB.longitude - coordinateA.longitude) * Math.PI) / 180;

    const yComponent = Math.sin(deltaLon) * Math.cos(lat2Rad);
    const xComponent =
      Math.cos(lat1Rad) * Math.sin(lat2Rad) - Math.sin(lat1Rad) * Math.cos(lat2Rad) * Math.cos(deltaLon);

    let bearingDegrees = (Math.atan2(yComponent, xComponent) * 180) / Math.PI;

    // Normalize to 0-360
    return ((bearingDegrees % 360) + 360) % 360;
  }

  /**
   * Calculate destination point given start, bearing, and distance.
   *
   * @param {Coordinate} start - Starting coordinate
   * @param {number} bearingDegrees - Bearing in degrees
   * @param {number} distanceMeters - Distance in meters
   * @returns {Coordinate}
   */
  static destination(start, bearingDegrees, distanceMeters) {
    const lat1Rad = (start.latitude * Math.PI) / 180;
    const lon1Rad = (start.longitude * Math.PI) / 180;
    const bearingRad = (bearingDegrees * Math.PI) / 180;
    const angularDistance = distanceMeters / EARTH_RADIUS;

    const lat2Rad = Math.asin(
      Math.sin(lat1Rad) * Math.cos(angularDistance) +
        Math.cos(lat1Rad) * Math.sin(angularDistance) * Math.cos(bearingRad),
    );

    const lon2Rad =
      lon1Rad +
      Math.atan2(
        Math.sin(bearingRad) * Math.sin(angularDistance) * Math.cos(lat1Rad),
        Math.cos(angularDistance) - Math.sin(lat1Rad) * Math.sin(lat2Rad),
      );

    return {
      latitude: (lat2Rad * 180) / Math.PI,
      longitude: ((((lon2Rad * 180) / Math.PI + 540) % 360) - 180), // Normalize to -180 to 180
    };
  }

  /**
   * Create a bounding box.
   *
   * @param {number} minLat - Minimum latitude
   * @param {number} maxLat - Maximum latitude
   * @param {number} minLon - Minimum longitude
   * @param {number} maxLon - Maximum longitude
   * @returns {{ ok: true, value: BoundingBox } | { ok: false, error: string }}
   */
  static boundingBox(minLat, maxLat, minLon, maxLon) {
    if (minLat > maxLat) {
      return err('minLat cannot be greater than maxLat');
    }
    if (minLon > maxLon) {
      return err('minLon cannot be greater than maxLon');
    }

    const minLatResult = SafeGeo.coordinate(minLat, minLon);
    if (!minLatResult.ok) return minLatResult;

    const maxLatResult = SafeGeo.coordinate(maxLat, maxLon);
    if (!maxLatResult.ok) return maxLatResult;

    return ok({ minLat, maxLat, minLon, maxLon });
  }

  /**
   * Create bounding box from center and radius.
   *
   * @param {Coordinate} center - Center coordinate
   * @param {number} radiusMeters - Radius in meters
   * @returns {BoundingBox}
   */
  static boundingBoxFromRadius(center, radiusMeters) {
    const latOffset = (radiusMeters / EARTH_RADIUS) * (180 / Math.PI);
    const lonOffset =
      ((radiusMeters / EARTH_RADIUS) * (180 / Math.PI)) / Math.cos((center.latitude * Math.PI) / 180);

    return {
      minLat: Math.max(-90, center.latitude - latOffset),
      maxLat: Math.min(90, center.latitude + latOffset),
      minLon: Math.max(-180, center.longitude - lonOffset),
      maxLon: Math.min(180, center.longitude + lonOffset),
    };
  }

  /**
   * Check if coordinate is within bounding box.
   *
   * @param {Coordinate} coord - Coordinate to check
   * @param {BoundingBox} box - Bounding box
   * @returns {boolean}
   */
  static isInBoundingBox(coord, box) {
    return (
      coord.latitude >= box.minLat &&
      coord.latitude <= box.maxLat &&
      coord.longitude >= box.minLon &&
      coord.longitude <= box.maxLon
    );
  }

  /**
   * Calculate midpoint between two coordinates.
   *
   * @param {Coordinate} coordinateA - First coordinate
   * @param {Coordinate} coordinateB - Second coordinate
   * @returns {Coordinate}
   */
  static midpoint(coordinateA, coordinateB) {
    const lat1Rad = (coordinateA.latitude * Math.PI) / 180;
    const lon1Rad = (coordinateA.longitude * Math.PI) / 180;
    const lat2Rad = (coordinateB.latitude * Math.PI) / 180;
    const deltaLon = ((coordinateB.longitude - coordinateA.longitude) * Math.PI) / 180;

    const bx = Math.cos(lat2Rad) * Math.cos(deltaLon);
    const by = Math.cos(lat2Rad) * Math.sin(deltaLon);

    const lat3Rad = Math.atan2(Math.sin(lat1Rad) + Math.sin(lat2Rad), Math.sqrt((Math.cos(lat1Rad) + bx) ** 2 + by ** 2));
    const lon3Rad = lon1Rad + Math.atan2(by, Math.cos(lat1Rad) + bx);

    return {
      latitude: (lat3Rad * 180) / Math.PI,
      longitude: (lon3Rad * 180) / Math.PI,
    };
  }

  /**
   * Format coordinate as string.
   *
   * @param {Coordinate} coord - Coordinate to format
   * @param {number} [precision=6] - Decimal places
   * @returns {string}
   */
  static format(coord, precision = 6) {
    return `${coord.latitude.toFixed(precision)}, ${coord.longitude.toFixed(precision)}`;
  }

  /**
   * Format coordinate as DMS (degrees, minutes, seconds).
   *
   * @param {Coordinate} coord - Coordinate to format
   * @returns {string}
   */
  static formatDMS(coord) {
    const toDMS = (decimal, posDir, negDir) => {
      const dir = decimal >= 0 ? posDir : negDir;
      const abs = Math.abs(decimal);
      const deg = Math.floor(abs);
      const minFloat = (abs - deg) * 60;
      const min = Math.floor(minFloat);
      const sec = (minFloat - min) * 60;
      return `${deg}°${min}'${sec.toFixed(2)}"${dir}`;
    };

    const lat = toDMS(coord.latitude, 'N', 'S');
    const lon = toDMS(coord.longitude, 'E', 'W');
    return `${lat} ${lon}`;
  }
}

// Export convenience functions
export const haversineDistance = SafeGeo.haversineDistance;
export const bearing = SafeGeo.bearing;
export const destination = SafeGeo.destination;
