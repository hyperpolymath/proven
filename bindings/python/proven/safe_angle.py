# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeAngle - Degree/radian conversions with normalization.

Provides safe angle operations with proper handling of edge cases.
"""

from typing import Optional
from dataclasses import dataclass
from functools import total_ordering
import math


@total_ordering
@dataclass(frozen=True)
class Degrees:
    """
    Angle in degrees.

    Example:
        >>> d = Degrees(450)
        >>> d.normalize()
        Degrees(value=90.0)
        >>> d.to_radians()
        Radians(value=7.853981633974483)
    """
    value: float

    def normalize(self) -> "Degrees":
        """Normalize to [0, 360) range."""
        return Degrees(self.value % 360)

    def normalize_signed(self) -> "Degrees":
        """Normalize to [-180, 180) range."""
        v = self.value % 360
        if v >= 180:
            v -= 360
        return Degrees(v)

    def to_radians(self) -> "Radians":
        """Convert to radians."""
        return Radians(math.radians(self.value))

    def sin(self) -> float:
        """Calculate sine."""
        return math.sin(math.radians(self.value))

    def cos(self) -> float:
        """Calculate cosine."""
        return math.cos(math.radians(self.value))

    def tan(self) -> Optional[float]:
        """Calculate tangent (None at asymptotes)."""
        normalized = self.normalize().value
        if abs(normalized - 90) < 1e-10 or abs(normalized - 270) < 1e-10:
            return None
        return math.tan(math.radians(self.value))

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Degrees):
            return NotImplemented
        return abs(self.value - other.value) < 1e-10

    def __lt__(self, other: "Degrees") -> bool:
        if not isinstance(other, Degrees):
            return NotImplemented
        return self.value < other.value

    def __add__(self, other: "Degrees") -> "Degrees":
        return Degrees(self.value + other.value)

    def __sub__(self, other: "Degrees") -> "Degrees":
        return Degrees(self.value - other.value)

    def __mul__(self, scalar: float) -> "Degrees":
        return Degrees(self.value * scalar)

    def __neg__(self) -> "Degrees":
        return Degrees(-self.value)


@total_ordering
@dataclass(frozen=True)
class Radians:
    """
    Angle in radians.

    Example:
        >>> r = Radians(math.pi)
        >>> r.to_degrees()
        Degrees(value=180.0)
    """
    value: float

    def normalize(self) -> "Radians":
        """Normalize to [0, 2π) range."""
        return Radians(self.value % (2 * math.pi))

    def normalize_signed(self) -> "Radians":
        """Normalize to [-π, π) range."""
        v = self.value % (2 * math.pi)
        if v >= math.pi:
            v -= 2 * math.pi
        return Radians(v)

    def to_degrees(self) -> Degrees:
        """Convert to degrees."""
        return Degrees(math.degrees(self.value))

    def sin(self) -> float:
        """Calculate sine."""
        return math.sin(self.value)

    def cos(self) -> float:
        """Calculate cosine."""
        return math.cos(self.value)

    def tan(self) -> Optional[float]:
        """Calculate tangent (None at asymptotes)."""
        normalized = self.normalize().value
        if abs(normalized - math.pi / 2) < 1e-10 or abs(normalized - 3 * math.pi / 2) < 1e-10:
            return None
        return math.tan(self.value)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Radians):
            return NotImplemented
        return abs(self.value - other.value) < 1e-10

    def __lt__(self, other: "Radians") -> bool:
        if not isinstance(other, Radians):
            return NotImplemented
        return self.value < other.value

    def __add__(self, other: "Radians") -> "Radians":
        return Radians(self.value + other.value)

    def __sub__(self, other: "Radians") -> "Radians":
        return Radians(self.value - other.value)

    def __mul__(self, scalar: float) -> "Radians":
        return Radians(self.value * scalar)

    def __neg__(self) -> "Radians":
        return Radians(-self.value)


# Conversion functions
def deg_to_rad(degrees: float) -> float:
    """Convert degrees to radians."""
    return math.radians(degrees)


def rad_to_deg(radians: float) -> float:
    """Convert radians to degrees."""
    return math.degrees(radians)


def normalize_degrees(degrees: float) -> float:
    """Normalize degrees to [0, 360) range."""
    return degrees % 360


def normalize_radians(radians: float) -> float:
    """Normalize radians to [0, 2π) range."""
    return radians % (2 * math.pi)


def lerp_angle_degrees(a: float, b: float, t: float) -> float:
    """
    Linear interpolation between two angles (shortest path).

    Args:
        a: Start angle in degrees
        b: End angle in degrees
        t: Interpolation factor (0-1)

    Returns:
        Interpolated angle in degrees
    """
    diff = (b - a + 180) % 360 - 180
    return normalize_degrees(a + diff * t)


def lerp_angle_radians(a: float, b: float, t: float) -> float:
    """
    Linear interpolation between two angles (shortest path).

    Args:
        a: Start angle in radians
        b: End angle in radians
        t: Interpolation factor (0-1)

    Returns:
        Interpolated angle in radians
    """
    diff = (b - a + math.pi) % (2 * math.pi) - math.pi
    return normalize_radians(a + diff * t)


def angle_difference_degrees(a: float, b: float) -> float:
    """
    Get shortest angle difference (-180 to 180).

    Args:
        a: First angle in degrees
        b: Second angle in degrees

    Returns:
        Signed difference (b - a) via shortest path
    """
    diff = (b - a + 180) % 360 - 180
    return diff


def angle_difference_radians(a: float, b: float) -> float:
    """
    Get shortest angle difference (-π to π).

    Args:
        a: First angle in radians
        b: Second angle in radians

    Returns:
        Signed difference (b - a) via shortest path
    """
    diff = (b - a + math.pi) % (2 * math.pi) - math.pi
    return diff


class SafeAngle:
    """Safe angle utilities."""

    # Common angle constants
    DEGREES_0 = Degrees(0)
    DEGREES_90 = Degrees(90)
    DEGREES_180 = Degrees(180)
    DEGREES_270 = Degrees(270)
    DEGREES_360 = Degrees(360)

    RADIANS_0 = Radians(0)
    RADIANS_PI_2 = Radians(math.pi / 2)
    RADIANS_PI = Radians(math.pi)
    RADIANS_3PI_2 = Radians(3 * math.pi / 2)
    RADIANS_2PI = Radians(2 * math.pi)

    @staticmethod
    def from_degrees(value: float) -> Degrees:
        """Create angle from degrees."""
        return Degrees(value)

    @staticmethod
    def from_radians(value: float) -> Radians:
        """Create angle from radians."""
        return Radians(value)

    @staticmethod
    def atan2_degrees(y: float, x: float) -> Degrees:
        """Calculate atan2 returning degrees."""
        return Degrees(math.degrees(math.atan2(y, x)))

    @staticmethod
    def atan2_radians(y: float, x: float) -> Radians:
        """Calculate atan2 returning radians."""
        return Radians(math.atan2(y, x))
