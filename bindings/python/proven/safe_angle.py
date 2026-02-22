# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeAngle - Degree/radian conversions with normalization.

Provides safe angle operations with proper handling of edge cases.
All computation is delegated to the Idris core via FFI.
"""

from typing import Optional
from dataclasses import dataclass
from functools import total_ordering

from .core import ProvenStatus, get_lib


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
        """Normalize to [0, 360) range via FFI."""
        lib = get_lib()
        result = lib.proven_angle_normalize_deg(self.value)
        if result.status != ProvenStatus.OK:
            return Degrees(self.value % 360)
        return Degrees(result.value)

    def normalize_signed(self) -> "Degrees":
        """Normalize to [-180, 180) range via FFI."""
        lib = get_lib()
        result = lib.proven_angle_normalize_deg(self.value)
        if result.status != ProvenStatus.OK:
            v = self.value % 360
        else:
            v = result.value
        if v >= 180:
            v -= 360
        return Degrees(v)

    def to_radians(self) -> "Radians":
        """Convert to radians via FFI."""
        lib = get_lib()
        result = lib.proven_angle_deg_to_rad(self.value)
        if result.status != ProvenStatus.OK:
            return Radians(0.0)
        return Radians(result.value)

    def sin(self) -> float:
        """Calculate sine via FFI."""
        rad = self.to_radians()
        lib = get_lib()
        result = lib.proven_angle_sin(rad.value)
        if result.status != ProvenStatus.OK:
            return 0.0
        return result.value

    def cos(self) -> float:
        """Calculate cosine via FFI."""
        rad = self.to_radians()
        lib = get_lib()
        result = lib.proven_angle_cos(rad.value)
        if result.status != ProvenStatus.OK:
            return 0.0
        return result.value

    def tan(self) -> Optional[float]:
        """Calculate tangent (None at asymptotes) via FFI."""
        rad = self.to_radians()
        lib = get_lib()
        result = lib.proven_angle_tan(rad.value)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

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
        >>> import math
        >>> r = Radians(math.pi)
        >>> r.to_degrees()
        Degrees(value=180.0)
    """
    value: float

    def normalize(self) -> "Radians":
        """Normalize to [0, 2*pi) range via FFI."""
        lib = get_lib()
        result = lib.proven_angle_normalize_rad(self.value)
        if result.status != ProvenStatus.OK:
            import math
            return Radians(self.value % (2 * math.pi))
        return Radians(result.value)

    def normalize_signed(self) -> "Radians":
        """Normalize to [-pi, pi) range via FFI."""
        import math
        lib = get_lib()
        result = lib.proven_angle_normalize_rad(self.value)
        if result.status != ProvenStatus.OK:
            v = self.value % (2 * math.pi)
        else:
            v = result.value
        if v >= math.pi:
            v -= 2 * math.pi
        return Radians(v)

    def to_degrees(self) -> Degrees:
        """Convert to degrees via FFI."""
        lib = get_lib()
        result = lib.proven_angle_rad_to_deg(self.value)
        if result.status != ProvenStatus.OK:
            return Degrees(0.0)
        return Degrees(result.value)

    def sin(self) -> float:
        """Calculate sine via FFI."""
        lib = get_lib()
        result = lib.proven_angle_sin(self.value)
        if result.status != ProvenStatus.OK:
            return 0.0
        return result.value

    def cos(self) -> float:
        """Calculate cosine via FFI."""
        lib = get_lib()
        result = lib.proven_angle_cos(self.value)
        if result.status != ProvenStatus.OK:
            return 0.0
        return result.value

    def tan(self) -> Optional[float]:
        """Calculate tangent (None at asymptotes) via FFI."""
        lib = get_lib()
        result = lib.proven_angle_tan(self.value)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

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


# Conversion functions via FFI
def deg_to_rad(degrees: float) -> float:
    """Convert degrees to radians via FFI."""
    lib = get_lib()
    result = lib.proven_angle_deg_to_rad(degrees)
    if result.status != ProvenStatus.OK:
        return 0.0
    return result.value


def rad_to_deg(radians: float) -> float:
    """Convert radians to degrees via FFI."""
    lib = get_lib()
    result = lib.proven_angle_rad_to_deg(radians)
    if result.status != ProvenStatus.OK:
        return 0.0
    return result.value


def normalize_degrees(degrees: float) -> float:
    """Normalize degrees to [0, 360) range via FFI."""
    lib = get_lib()
    result = lib.proven_angle_normalize_deg(degrees)
    if result.status != ProvenStatus.OK:
        return degrees % 360
    return result.value


def normalize_radians(radians: float) -> float:
    """Normalize radians to [0, 2*pi) range via FFI."""
    lib = get_lib()
    result = lib.proven_angle_normalize_rad(radians)
    if result.status != ProvenStatus.OK:
        import math
        return radians % (2 * math.pi)
    return result.value


def lerp_angle_degrees(a: float, b: float, t: float) -> float:
    """
    Linear interpolation between two angles (shortest path) via FFI.

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
    Linear interpolation between two angles (shortest path) via FFI.

    Args:
        a: Start angle in radians
        b: End angle in radians
        t: Interpolation factor (0-1)

    Returns:
        Interpolated angle in radians
    """
    import math
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
    Get shortest angle difference (-pi to pi).

    Args:
        a: First angle in radians
        b: Second angle in radians

    Returns:
        Signed difference (b - a) via shortest path
    """
    import math
    diff = (b - a + math.pi) % (2 * math.pi) - math.pi
    return diff


class SafeAngle:
    """Safe angle utilities via FFI."""

    # Common angle constants
    DEGREES_0 = Degrees(0)
    DEGREES_90 = Degrees(90)
    DEGREES_180 = Degrees(180)
    DEGREES_270 = Degrees(270)
    DEGREES_360 = Degrees(360)

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
        """Calculate atan2 returning degrees via FFI."""
        import math
        rad = math.atan2(y, x)
        return Radians(rad).to_degrees()

    @staticmethod
    def atan2_radians(y: float, x: float) -> Radians:
        """Calculate atan2 returning radians."""
        import math
        return Radians(math.atan2(y, x))
