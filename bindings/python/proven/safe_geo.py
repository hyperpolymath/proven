# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeGeo - Geographic coordinates and distance calculations.

Provides safe geographic operations with coordinate validation.
All computation is delegated to the Idris core via FFI.
"""

import ctypes
from typing import Optional, Tuple
from dataclasses import dataclass

from .core import ProvenStatus, get_lib


@dataclass(frozen=True)
class Coordinate:
    """
    Geographic coordinate (latitude/longitude).

    Example:
        >>> nyc = Coordinate(40.7128, -74.0060)
        >>> la = Coordinate(34.0522, -118.2437)
        >>> nyc.distance_to(la)
        3935.746...
    """
    latitude: float
    longitude: float

    def __post_init__(self):
        if not -90 <= self.latitude <= 90:
            raise ValueError(f"Latitude must be -90 to 90, got {self.latitude}")
        if not -180 <= self.longitude <= 180:
            raise ValueError(f"Longitude must be -180 to 180, got {self.longitude}")

    def to_tuple(self) -> Tuple[float, float]:
        """Get as (lat, lon) tuple."""
        return (self.latitude, self.longitude)

    def to_radians(self) -> Tuple[float, float]:
        """Get as (lat_rad, lon_rad) tuple via FFI."""
        lib = get_lib()
        lat_r = lib.proven_angle_deg_to_rad(self.latitude)
        lon_r = lib.proven_angle_deg_to_rad(self.longitude)
        lat_val = lat_r.value if lat_r.status == ProvenStatus.OK else 0.0
        lon_val = lon_r.value if lon_r.status == ProvenStatus.OK else 0.0
        return (lat_val, lon_val)

    def distance_to(self, other: "Coordinate") -> float:
        """
        Calculate great-circle distance to another coordinate via FFI.

        Uses Haversine formula. Returns distance in kilometers.

        Args:
            other: Target coordinate

        Returns:
            Distance in kilometers
        """
        return haversine_distance(self, other)

    def bearing_to(self, other: "Coordinate") -> float:
        """
        Calculate initial bearing to another coordinate via FFI.

        Args:
            other: Target coordinate

        Returns:
            Bearing in degrees (0-360, 0 = North)
        """
        return bearing(self, other)

    def destination(self, bearing_deg: float, distance_km: float) -> "Coordinate":
        """
        Calculate destination point given bearing and distance via FFI.

        Args:
            bearing_deg: Bearing in degrees
            distance_km: Distance in kilometers

        Returns:
            Destination coordinate
        """
        return destination_point(self, bearing_deg, distance_km)


@dataclass
class BoundingBox:
    """
    Geographic bounding box.

    Example:
        >>> bbox = BoundingBox(south=40.0, west=-75.0, north=41.0, east=-73.0)
        >>> bbox.contains(Coordinate(40.5, -74.0))
        True
    """
    south: float  # Min latitude
    west: float   # Min longitude
    north: float  # Max latitude
    east: float   # Max longitude

    def __post_init__(self):
        if not -90 <= self.south <= 90:
            raise ValueError(f"South must be -90 to 90, got {self.south}")
        if not -90 <= self.north <= 90:
            raise ValueError(f"North must be -90 to 90, got {self.north}")
        if self.south > self.north:
            raise ValueError(f"South ({self.south}) must be <= North ({self.north})")

    def contains(self, coord: Coordinate) -> bool:
        """Check if coordinate is within bounding box."""
        if not self.south <= coord.latitude <= self.north:
            return False

        # Handle antimeridian crossing
        if self.west <= self.east:
            return self.west <= coord.longitude <= self.east
        else:
            return coord.longitude >= self.west or coord.longitude <= self.east

    def center(self) -> Coordinate:
        """Get center point of bounding box."""
        lat = (self.south + self.north) / 2

        if self.west <= self.east:
            lon = (self.west + self.east) / 2
        else:
            # Antimeridian crossing
            lon = (self.west + self.east + 360) / 2
            if lon > 180:
                lon -= 360

        return Coordinate(lat, lon)

    def expand(self, coord: Coordinate) -> "BoundingBox":
        """Return new bounding box expanded to include coordinate."""
        return BoundingBox(
            south=min(self.south, coord.latitude),
            west=min(self.west, coord.longitude),
            north=max(self.north, coord.latitude),
            east=max(self.east, coord.longitude),
        )


# Earth radius in kilometers
EARTH_RADIUS_KM = 6371.0


def haversine_distance(a: Coordinate, b: Coordinate) -> float:
    """
    Calculate great-circle distance using Haversine formula via FFI.

    Args:
        a: First coordinate
        b: Second coordinate

    Returns:
        Distance in kilometers

    Example:
        >>> nyc = Coordinate(40.7128, -74.0060)
        >>> london = Coordinate(51.5074, -0.1278)
        >>> haversine_distance(nyc, london)
        5570.222...
    """
    lib = get_lib()
    result = lib.proven_geo_haversine(a.latitude, a.longitude, b.latitude, b.longitude)
    if result.status != ProvenStatus.OK:
        return 0.0
    return result.value


def bearing(from_coord: Coordinate, to_coord: Coordinate) -> float:
    """
    Calculate initial bearing from one coordinate to another via FFI.

    Args:
        from_coord: Starting coordinate
        to_coord: Destination coordinate

    Returns:
        Bearing in degrees (0-360, 0 = North)
    """
    lib = get_lib()
    result = lib.proven_geo_bearing(
        from_coord.latitude, from_coord.longitude,
        to_coord.latitude, to_coord.longitude,
    )
    if result.status != ProvenStatus.OK:
        return 0.0
    return result.value


def destination_point(start: Coordinate, bearing_deg: float, distance_km: float) -> Coordinate:
    """
    Calculate destination point given bearing and distance via FFI.

    Args:
        start: Starting coordinate
        bearing_deg: Bearing in degrees
        distance_km: Distance in kilometers

    Returns:
        Destination coordinate
    """
    lib = get_lib()
    out_lat = ctypes.c_double(0.0)
    out_lon = ctypes.c_double(0.0)
    status = lib.proven_geo_destination(
        start.latitude, start.longitude,
        bearing_deg, distance_km,
        ctypes.byref(out_lat), ctypes.byref(out_lon),
    )
    if status != ProvenStatus.OK:
        return start
    return Coordinate(out_lat.value, out_lon.value)


# Keep backward compatible name
destination = destination_point


def midpoint(a: Coordinate, b: Coordinate) -> Coordinate:
    """
    Calculate midpoint between two coordinates via FFI.

    Uses destination at half the distance along the bearing.

    Args:
        a: First coordinate
        b: Second coordinate

    Returns:
        Midpoint coordinate
    """
    dist = haversine_distance(a, b)
    bear = bearing(a, b)
    return destination_point(a, bear, dist / 2.0)


class SafeGeo:
    """Safe geographic utilities via FFI."""

    @staticmethod
    def parse_coordinate(lat: float, lon: float) -> Optional[Coordinate]:
        """
        Safely parse a coordinate via FFI.

        Args:
            lat: Latitude
            lon: Longitude

        Returns:
            Coordinate, or None if invalid
        """
        lib = get_lib()
        result = lib.proven_geo_validate_coord(lat, lon)
        if result.status != ProvenStatus.OK or not result.value:
            return None
        try:
            return Coordinate(lat, lon)
        except ValueError:
            return None

    @staticmethod
    def distance(lat1: float, lon1: float, lat2: float, lon2: float) -> Optional[float]:
        """
        Safely calculate distance between two points via FFI.

        Args:
            lat1, lon1: First point
            lat2, lon2: Second point

        Returns:
            Distance in km, or None if coordinates invalid
        """
        lib = get_lib()
        result = lib.proven_geo_haversine(lat1, lon1, lat2, lon2)
        if result.status != ProvenStatus.OK:
            return None
        return result.value
