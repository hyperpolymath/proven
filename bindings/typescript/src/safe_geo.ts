// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

const EARTH_RADIUS_KM = 6371;
const EARTH_RADIUS_MILES = 3959;

/**
 * Coordinate represents a geographic coordinate.
 */
export class Coordinate {
  readonly latitude: number;
  readonly longitude: number;

  private constructor(latitude: number, longitude: number) {
    this.latitude = latitude;
    this.longitude = longitude;
  }

  /**
   * Create a coordinate with validation.
   */
  static create(latitude: number, longitude: number): Result<Coordinate> {
    if (latitude < -90 || latitude > 90) {
      return { ok: false, error: 'Latitude must be between -90 and 90' };
    }
    if (longitude < -180 || longitude > 180) {
      return { ok: false, error: 'Longitude must be between -180 and 180' };
    }
    return { ok: true, value: new Coordinate(latitude, longitude) };
  }

  /**
   * Create a coordinate, normalizing out-of-range values.
   */
  static normalize(latitude: number, longitude: number): Coordinate {
    // Normalize latitude
    let lat = latitude;
    if (lat > 90) lat = 90;
    if (lat < -90) lat = -90;

    // Normalize longitude
    let lng = ((longitude + 180) % 360 + 360) % 360 - 180;

    return new Coordinate(lat, lng);
  }

  /**
   * Parse from "lat,lng" string.
   */
  static parse(str: string): Result<Coordinate> {
    const parts = str.split(',').map((p) => p.trim());
    if (parts.length !== 2) {
      return { ok: false, error: 'Expected "latitude,longitude" format' };
    }

    const lat = parseFloat(parts[0]);
    const lng = parseFloat(parts[1]);

    if (isNaN(lat) || isNaN(lng)) {
      return { ok: false, error: 'Invalid coordinate values' };
    }

    return Coordinate.create(lat, lng);
  }

  /**
   * Convert to string.
   */
  toString(): string {
    return `${this.latitude},${this.longitude}`;
  }

  /**
   * Convert to GeoJSON format.
   */
  toGeoJSON(): { type: 'Point'; coordinates: [number, number] } {
    return {
      type: 'Point',
      coordinates: [this.longitude, this.latitude],
    };
  }

  /**
   * Convert to degrees-minutes-seconds format.
   */
  toDMS(): { lat: string; lng: string } {
    const formatDMS = (value: number, isLat: boolean): string => {
      const direction = isLat
        ? value >= 0 ? 'N' : 'S'
        : value >= 0 ? 'E' : 'W';
      const abs = Math.abs(value);
      const degrees = Math.floor(abs);
      const minutes = Math.floor((abs - degrees) * 60);
      const seconds = ((abs - degrees - minutes / 60) * 3600).toFixed(2);
      return `${degrees}°${minutes}'${seconds}"${direction}`;
    };

    return {
      lat: formatDMS(this.latitude, true),
      lng: formatDMS(this.longitude, false),
    };
  }

  /**
   * Check if coordinate is in northern hemisphere.
   */
  isNorthern(): boolean {
    return this.latitude >= 0;
  }

  /**
   * Check if coordinate is in eastern hemisphere.
   */
  isEastern(): boolean {
    return this.longitude >= 0;
  }
}

/**
 * Calculate distance between two coordinates using Haversine formula.
 */
export function haversine(from: Coordinate, to: Coordinate, unit: 'km' | 'miles' = 'km'): number {
  const radius = unit === 'km' ? EARTH_RADIUS_KM : EARTH_RADIUS_MILES;

  const lat1 = (from.latitude * Math.PI) / 180;
  const lat2 = (to.latitude * Math.PI) / 180;
  const dLat = ((to.latitude - from.latitude) * Math.PI) / 180;
  const dLon = ((to.longitude - from.longitude) * Math.PI) / 180;

  const a =
    Math.sin(dLat / 2) * Math.sin(dLat / 2) +
    Math.cos(lat1) * Math.cos(lat2) * Math.sin(dLon / 2) * Math.sin(dLon / 2);
  const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

  return radius * c;
}

/**
 * Calculate initial bearing from one coordinate to another.
 */
export function bearing(from: Coordinate, to: Coordinate): number {
  const lat1 = (from.latitude * Math.PI) / 180;
  const lat2 = (to.latitude * Math.PI) / 180;
  const dLon = ((to.longitude - from.longitude) * Math.PI) / 180;

  const y = Math.sin(dLon) * Math.cos(lat2);
  const x =
    Math.cos(lat1) * Math.sin(lat2) - Math.sin(lat1) * Math.cos(lat2) * Math.cos(dLon);

  const bearing = (Math.atan2(y, x) * 180) / Math.PI;
  return (bearing + 360) % 360;
}

/**
 * Calculate destination point given start, bearing, and distance.
 */
export function destination(
  start: Coordinate,
  bearingDeg: number,
  distanceKm: number
): Coordinate {
  const lat1 = (start.latitude * Math.PI) / 180;
  const lon1 = (start.longitude * Math.PI) / 180;
  const brng = (bearingDeg * Math.PI) / 180;
  const d = distanceKm / EARTH_RADIUS_KM;

  const lat2 = Math.asin(
    Math.sin(lat1) * Math.cos(d) + Math.cos(lat1) * Math.sin(d) * Math.cos(brng)
  );
  const lon2 =
    lon1 +
    Math.atan2(
      Math.sin(brng) * Math.sin(d) * Math.cos(lat1),
      Math.cos(d) - Math.sin(lat1) * Math.sin(lat2)
    );

  return Coordinate.normalize((lat2 * 180) / Math.PI, (lon2 * 180) / Math.PI);
}

/**
 * Calculate midpoint between two coordinates.
 */
export function midpoint(from: Coordinate, to: Coordinate): Coordinate {
  const lat1 = (from.latitude * Math.PI) / 180;
  const lon1 = (from.longitude * Math.PI) / 180;
  const lat2 = (to.latitude * Math.PI) / 180;
  const dLon = ((to.longitude - from.longitude) * Math.PI) / 180;

  const Bx = Math.cos(lat2) * Math.cos(dLon);
  const By = Math.cos(lat2) * Math.sin(dLon);

  const lat3 = Math.atan2(
    Math.sin(lat1) + Math.sin(lat2),
    Math.sqrt((Math.cos(lat1) + Bx) ** 2 + By ** 2)
  );
  const lon3 = lon1 + Math.atan2(By, Math.cos(lat1) + Bx);

  return Coordinate.normalize((lat3 * 180) / Math.PI, (lon3 * 180) / Math.PI);
}

/**
 * Calculate bounding box around a coordinate with a given radius.
 */
export function boundingBox(
  center: Coordinate,
  radiusKm: number
): { minLat: number; maxLat: number; minLng: number; maxLng: number } {
  const latChange = (radiusKm / EARTH_RADIUS_KM) * (180 / Math.PI);
  const lonChange =
    ((radiusKm / EARTH_RADIUS_KM) * (180 / Math.PI)) /
    Math.cos((center.latitude * Math.PI) / 180);

  return {
    minLat: Math.max(-90, center.latitude - latChange),
    maxLat: Math.min(90, center.latitude + latChange),
    minLng: Math.max(-180, center.longitude - lonChange),
    maxLng: Math.min(180, center.longitude + lonChange),
  };
}

/**
 * Check if a coordinate is within a bounding box.
 */
export function isInBoundingBox(
  coord: Coordinate,
  box: { minLat: number; maxLat: number; minLng: number; maxLng: number }
): boolean {
  return (
    coord.latitude >= box.minLat &&
    coord.latitude <= box.maxLat &&
    coord.longitude >= box.minLng &&
    coord.longitude <= box.maxLng
  );
}

/**
 * Calculate centroid of multiple coordinates.
 */
export function centroid(coords: Coordinate[]): Coordinate | undefined {
  if (coords.length === 0) return undefined;

  let x = 0, y = 0, z = 0;

  for (const coord of coords) {
    const lat = (coord.latitude * Math.PI) / 180;
    const lon = (coord.longitude * Math.PI) / 180;

    x += Math.cos(lat) * Math.cos(lon);
    y += Math.cos(lat) * Math.sin(lon);
    z += Math.sin(lat);
  }

  const n = coords.length;
  x /= n;
  y /= n;
  z /= n;

  const lon = Math.atan2(y, x);
  const hyp = Math.sqrt(x * x + y * y);
  const lat = Math.atan2(z, hyp);

  return Coordinate.normalize((lat * 180) / Math.PI, (lon * 180) / Math.PI);
}

export const SafeGeo = {
  Coordinate,
  haversine,
  bearing,
  destination,
  midpoint,
  boundingBox,
  isInBoundingBox,
  centroid,
  EARTH_RADIUS_KM,
  EARTH_RADIUS_MILES,
};
