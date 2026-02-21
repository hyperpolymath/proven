# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeGeo - Geographic coordinates and distance calculations.

Provides safe geographic operations with coordinate validation.
"""

from typing import Optional, Tuple
from dataclasses import dataclass
import math


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
        """Get as (lat_rad, lon_rad) tuple."""
        return (math.radians(self.latitude), math.radians(self.longitude))

    def distance_to(self, other: "Coordinate") -> float:
        """
        Calculate great-circle distance to another coordinate.

        Uses Haversine formula. Returns distance in kilometers.

        Args:
            other: Target coordinate

        Returns:
            Distance in kilometers
        """
        return haversine_distance(self, other)

    def bearing_to(self, other: "Coordinate") -> float:
        """
        Calculate initial bearing to another coordinate.

        Args:
            other: Target coordinate

        Returns:
            Bearing in degrees (0-360, 0 = North)
        """
        return bearing(self, other)

    def destination(self, bearing_deg: float, distance_km: float) -> "Coordinate":
        """
        Calculate destination point given bearing and distance.

        Args:
            bearing_deg: Bearing in degrees
            distance_km: Distance in kilometers

        Returns:
            Destination coordinate
        """
        return destination(self, bearing_deg, distance_km)


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
    Calculate great-circle distance using Haversine formula.

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
    lat1, lon1 = a.to_radians()
    lat2, lon2 = b.to_radians()

    dlat = lat2 - lat1
    dlon = lon2 - lon1

    a_hav = math.sin(dlat / 2) ** 2 + math.cos(lat1) * math.cos(lat2) * math.sin(dlon / 2) ** 2
    c = 2 * math.asin(math.sqrt(a_hav))

    return EARTH_RADIUS_KM * c


def bearing(from_coord: Coordinate, to_coord: Coordinate) -> float:
    """
    Calculate initial bearing from one coordinate to another.

    Args:
        from_coord: Starting coordinate
        to_coord: Destination coordinate

    Returns:
        Bearing in degrees (0-360, 0 = North)
    """
    lat1, lon1 = from_coord.to_radians()
    lat2, lon2 = to_coord.to_radians()

    dlon = lon2 - lon1

    x = math.sin(dlon) * math.cos(lat2)
    y = math.cos(lat1) * math.sin(lat2) - math.sin(lat1) * math.cos(lat2) * math.cos(dlon)

    bearing_rad = math.atan2(x, y)
    bearing_deg = math.degrees(bearing_rad)

    return (bearing_deg + 360) % 360


def destination(start: Coordinate, bearing_deg: float, distance_km: float) -> Coordinate:
    """
    Calculate destination point given bearing and distance.

    Args:
        start: Starting coordinate
        bearing_deg: Bearing in degrees
        distance_km: Distance in kilometers

    Returns:
        Destination coordinate
    """
    lat1, lon1 = start.to_radians()
    bearing_rad = math.radians(bearing_deg)
    angular_distance = distance_km / EARTH_RADIUS_KM

    lat2 = math.asin(
        math.sin(lat1) * math.cos(angular_distance)
        + math.cos(lat1) * math.sin(angular_distance) * math.cos(bearing_rad)
    )

    lon2 = lon1 + math.atan2(
        math.sin(bearing_rad) * math.sin(angular_distance) * math.cos(lat1),
        math.cos(angular_distance) - math.sin(lat1) * math.sin(lat2),
    )

    # Normalize longitude to -180 to 180
    lon2_deg = math.degrees(lon2)
    lon2_deg = ((lon2_deg + 180) % 360) - 180

    return Coordinate(math.degrees(lat2), lon2_deg)


def midpoint(a: Coordinate, b: Coordinate) -> Coordinate:
    """
    Calculate midpoint between two coordinates.

    Args:
        a: First coordinate
        b: Second coordinate

    Returns:
        Midpoint coordinate
    """
    lat1, lon1 = a.to_radians()
    lat2, lon2 = b.to_radians()

    bx = math.cos(lat2) * math.cos(lon2 - lon1)
    by = math.cos(lat2) * math.sin(lon2 - lon1)

    lat3 = math.atan2(
        math.sin(lat1) + math.sin(lat2),
        math.sqrt((math.cos(lat1) + bx) ** 2 + by ** 2),
    )
    lon3 = lon1 + math.atan2(by, math.cos(lat1) + bx)

    return Coordinate(math.degrees(lat3), math.degrees(lon3))


class SafeGeo:
    """Safe geographic utilities."""

    @staticmethod
    def parse_coordinate(lat: float, lon: float) -> Optional[Coordinate]:
        """
        Safely parse a coordinate.

        Args:
            lat: Latitude
            lon: Longitude

        Returns:
            Coordinate, or None if invalid
        """
        try:
            return Coordinate(lat, lon)
        except ValueError:
            return None

    @staticmethod
    def distance(lat1: float, lon1: float, lat2: float, lon2: float) -> Optional[float]:
        """
        Safely calculate distance between two points.

        Args:
            lat1, lon1: First point
            lat2, lon2: Second point

        Returns:
            Distance in km, or None if coordinates invalid
        """
        c1 = SafeGeo.parse_coordinate(lat1, lon1)
        c2 = SafeGeo.parse_coordinate(lat2, lon2)
        if c1 is None or c2 is None:
            return None
        return haversine_distance(c1, c2)
