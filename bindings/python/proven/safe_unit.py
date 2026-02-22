# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeUnit - Physical unit conversions.

Provides type-safe unit conversions for length, mass, temperature, time, and data.
All conversions are delegated to the Idris core via FFI.
"""

from enum import Enum
from typing import Optional

from .core import ProvenStatus, get_lib


class LengthUnit(Enum):
    """Length units."""
    METERS = "m"
    KILOMETERS = "km"
    CENTIMETERS = "cm"
    MILLIMETERS = "mm"
    MILES = "mi"
    YARDS = "yd"
    FEET = "ft"
    INCHES = "in"


class MassUnit(Enum):
    """Mass units."""
    KILOGRAMS = "kg"
    GRAMS = "g"
    MILLIGRAMS = "mg"
    POUNDS = "lb"
    OUNCES = "oz"
    STONES = "st"


class TemperatureUnit(Enum):
    """Temperature units."""
    CELSIUS = "C"
    FAHRENHEIT = "F"
    KELVIN = "K"


class TimeUnit(Enum):
    """Time units."""
    SECONDS = "s"
    MILLISECONDS = "ms"
    MICROSECONDS = "us"
    NANOSECONDS = "ns"
    MINUTES = "min"
    HOURS = "h"
    DAYS = "d"
    WEEKS = "w"


class DataUnit(Enum):
    """Data size units."""
    BYTES = "B"
    KILOBYTES = "KB"
    MEGABYTES = "MB"
    GIGABYTES = "GB"
    TERABYTES = "TB"
    KIBIBYTES = "KiB"
    MEBIBYTES = "MiB"
    GIBIBYTES = "GiB"
    TEBIBYTES = "TiB"


# Maps from enum to FFI integer codes
_LENGTH_CODES = {
    LengthUnit.METERS: 0, LengthUnit.KILOMETERS: 1, LengthUnit.CENTIMETERS: 2,
    LengthUnit.MILLIMETERS: 3, LengthUnit.MILES: 4, LengthUnit.YARDS: 5,
    LengthUnit.FEET: 6, LengthUnit.INCHES: 7,
}

_MASS_CODES = {
    MassUnit.KILOGRAMS: 0, MassUnit.GRAMS: 1, MassUnit.MILLIGRAMS: 2,
    MassUnit.POUNDS: 3, MassUnit.OUNCES: 4, MassUnit.STONES: 5,
}

_TEMP_CODES = {
    TemperatureUnit.CELSIUS: 0, TemperatureUnit.FAHRENHEIT: 1, TemperatureUnit.KELVIN: 2,
}

_TIME_CODES = {
    TimeUnit.SECONDS: 0, TimeUnit.MILLISECONDS: 1, TimeUnit.MICROSECONDS: 2,
    TimeUnit.NANOSECONDS: 3, TimeUnit.MINUTES: 4, TimeUnit.HOURS: 5,
    TimeUnit.DAYS: 6, TimeUnit.WEEKS: 7,
}

_DATA_CODES = {
    DataUnit.BYTES: 0, DataUnit.KILOBYTES: 1, DataUnit.MEGABYTES: 2,
    DataUnit.GIGABYTES: 3, DataUnit.TERABYTES: 4, DataUnit.KIBIBYTES: 5,
    DataUnit.MEBIBYTES: 6, DataUnit.GIBIBYTES: 7, DataUnit.TEBIBYTES: 8,
}


def convert_length(value: float, from_unit: LengthUnit, to_unit: LengthUnit) -> float:
    """
    Convert length between units via FFI.

    Args:
        value: Value to convert
        from_unit: Source unit
        to_unit: Target unit

    Returns:
        Converted value

    Example:
        >>> convert_length(1.0, LengthUnit.KILOMETERS, LengthUnit.METERS)
        1000.0
    """
    lib = get_lib()
    result = lib.proven_unit_convert_length(value, _LENGTH_CODES[from_unit], _LENGTH_CODES[to_unit])
    if result.status != ProvenStatus.OK:
        return 0.0
    return result.value


def convert_mass(value: float, from_unit: MassUnit, to_unit: MassUnit) -> float:
    """
    Convert mass between units via FFI.

    Args:
        value: Value to convert
        from_unit: Source unit
        to_unit: Target unit

    Returns:
        Converted value
    """
    lib = get_lib()
    result = lib.proven_unit_convert_mass(value, _MASS_CODES[from_unit], _MASS_CODES[to_unit])
    if result.status != ProvenStatus.OK:
        return 0.0
    return result.value


def convert_temperature(value: float, from_unit: TemperatureUnit, to_unit: TemperatureUnit) -> float:
    """
    Convert temperature between units via FFI.

    Args:
        value: Value to convert
        from_unit: Source unit
        to_unit: Target unit

    Returns:
        Converted value

    Example:
        >>> convert_temperature(0, TemperatureUnit.CELSIUS, TemperatureUnit.FAHRENHEIT)
        32.0
    """
    lib = get_lib()
    result = lib.proven_unit_convert_temperature(value, _TEMP_CODES[from_unit], _TEMP_CODES[to_unit])
    if result.status != ProvenStatus.OK:
        return 0.0
    return result.value


def convert_time(value: float, from_unit: TimeUnit, to_unit: TimeUnit) -> float:
    """
    Convert time between units via FFI.

    Args:
        value: Value to convert
        from_unit: Source unit
        to_unit: Target unit

    Returns:
        Converted value
    """
    lib = get_lib()
    result = lib.proven_unit_convert_time(value, _TIME_CODES[from_unit], _TIME_CODES[to_unit])
    if result.status != ProvenStatus.OK:
        return 0.0
    return result.value


def convert_data(value: float, from_unit: DataUnit, to_unit: DataUnit) -> float:
    """
    Convert data size between units via FFI.

    Args:
        value: Value to convert
        from_unit: Source unit
        to_unit: Target unit

    Returns:
        Converted value

    Example:
        >>> convert_data(1.0, DataUnit.GIGABYTES, DataUnit.MEGABYTES)
        1000.0
    """
    lib = get_lib()
    result = lib.proven_unit_convert_data(value, _DATA_CODES[from_unit], _DATA_CODES[to_unit])
    if result.status != ProvenStatus.OK:
        return 0.0
    return result.value


class SafeUnit:
    """Safe unit conversion utilities via FFI."""

    convert_length = staticmethod(convert_length)
    convert_mass = staticmethod(convert_mass)
    convert_temperature = staticmethod(convert_temperature)
    convert_time = staticmethod(convert_time)
    convert_data = staticmethod(convert_data)

    # Re-export enums
    LengthUnit = LengthUnit
    MassUnit = MassUnit
    TemperatureUnit = TemperatureUnit
    TimeUnit = TimeUnit
    DataUnit = DataUnit
