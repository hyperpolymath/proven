# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeUnit - Physical unit conversions.

Provides type-safe unit conversions for length, mass, temperature, time, and data.
"""

from enum import Enum
from typing import Optional


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

    def to_meters(self, value: float) -> float:
        """Convert to meters (base unit)."""
        factors = {
            LengthUnit.METERS: 1.0,
            LengthUnit.KILOMETERS: 1000.0,
            LengthUnit.CENTIMETERS: 0.01,
            LengthUnit.MILLIMETERS: 0.001,
            LengthUnit.MILES: 1609.344,
            LengthUnit.YARDS: 0.9144,
            LengthUnit.FEET: 0.3048,
            LengthUnit.INCHES: 0.0254,
        }
        return value * factors[self]

    def from_meters(self, meters: float) -> float:
        """Convert from meters."""
        factors = {
            LengthUnit.METERS: 1.0,
            LengthUnit.KILOMETERS: 0.001,
            LengthUnit.CENTIMETERS: 100.0,
            LengthUnit.MILLIMETERS: 1000.0,
            LengthUnit.MILES: 1 / 1609.344,
            LengthUnit.YARDS: 1 / 0.9144,
            LengthUnit.FEET: 1 / 0.3048,
            LengthUnit.INCHES: 1 / 0.0254,
        }
        return meters * factors[self]


class MassUnit(Enum):
    """Mass units."""
    KILOGRAMS = "kg"
    GRAMS = "g"
    MILLIGRAMS = "mg"
    POUNDS = "lb"
    OUNCES = "oz"
    STONES = "st"

    def to_kilograms(self, value: float) -> float:
        """Convert to kilograms (base unit)."""
        factors = {
            MassUnit.KILOGRAMS: 1.0,
            MassUnit.GRAMS: 0.001,
            MassUnit.MILLIGRAMS: 0.000001,
            MassUnit.POUNDS: 0.453592,
            MassUnit.OUNCES: 0.0283495,
            MassUnit.STONES: 6.35029,
        }
        return value * factors[self]

    def from_kilograms(self, kg: float) -> float:
        """Convert from kilograms."""
        factors = {
            MassUnit.KILOGRAMS: 1.0,
            MassUnit.GRAMS: 1000.0,
            MassUnit.MILLIGRAMS: 1000000.0,
            MassUnit.POUNDS: 1 / 0.453592,
            MassUnit.OUNCES: 1 / 0.0283495,
            MassUnit.STONES: 1 / 6.35029,
        }
        return kg * factors[self]


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

    def to_seconds(self, value: float) -> float:
        """Convert to seconds (base unit)."""
        factors = {
            TimeUnit.SECONDS: 1.0,
            TimeUnit.MILLISECONDS: 0.001,
            TimeUnit.MICROSECONDS: 0.000001,
            TimeUnit.NANOSECONDS: 0.000000001,
            TimeUnit.MINUTES: 60.0,
            TimeUnit.HOURS: 3600.0,
            TimeUnit.DAYS: 86400.0,
            TimeUnit.WEEKS: 604800.0,
        }
        return value * factors[self]

    def from_seconds(self, secs: float) -> float:
        """Convert from seconds."""
        factors = {
            TimeUnit.SECONDS: 1.0,
            TimeUnit.MILLISECONDS: 1000.0,
            TimeUnit.MICROSECONDS: 1000000.0,
            TimeUnit.NANOSECONDS: 1000000000.0,
            TimeUnit.MINUTES: 1 / 60.0,
            TimeUnit.HOURS: 1 / 3600.0,
            TimeUnit.DAYS: 1 / 86400.0,
            TimeUnit.WEEKS: 1 / 604800.0,
        }
        return secs * factors[self]


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

    def to_bytes(self, value: float) -> float:
        """Convert to bytes (base unit)."""
        factors = {
            DataUnit.BYTES: 1.0,
            DataUnit.KILOBYTES: 1000.0,
            DataUnit.MEGABYTES: 1000000.0,
            DataUnit.GIGABYTES: 1000000000.0,
            DataUnit.TERABYTES: 1000000000000.0,
            DataUnit.KIBIBYTES: 1024.0,
            DataUnit.MEBIBYTES: 1048576.0,
            DataUnit.GIBIBYTES: 1073741824.0,
            DataUnit.TEBIBYTES: 1099511627776.0,
        }
        return value * factors[self]

    def from_bytes(self, bytes_val: float) -> float:
        """Convert from bytes."""
        factors = {
            DataUnit.BYTES: 1.0,
            DataUnit.KILOBYTES: 1 / 1000.0,
            DataUnit.MEGABYTES: 1 / 1000000.0,
            DataUnit.GIGABYTES: 1 / 1000000000.0,
            DataUnit.TERABYTES: 1 / 1000000000000.0,
            DataUnit.KIBIBYTES: 1 / 1024.0,
            DataUnit.MEBIBYTES: 1 / 1048576.0,
            DataUnit.GIBIBYTES: 1 / 1073741824.0,
            DataUnit.TEBIBYTES: 1 / 1099511627776.0,
        }
        return bytes_val * factors[self]


def convert_length(value: float, from_unit: LengthUnit, to_unit: LengthUnit) -> float:
    """
    Convert length between units.

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
    meters = from_unit.to_meters(value)
    return to_unit.from_meters(meters)


def convert_mass(value: float, from_unit: MassUnit, to_unit: MassUnit) -> float:
    """
    Convert mass between units.

    Args:
        value: Value to convert
        from_unit: Source unit
        to_unit: Target unit

    Returns:
        Converted value
    """
    kg = from_unit.to_kilograms(value)
    return to_unit.from_kilograms(kg)


def convert_temperature(value: float, from_unit: TemperatureUnit, to_unit: TemperatureUnit) -> float:
    """
    Convert temperature between units.

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
    # Convert to Kelvin first
    if from_unit == TemperatureUnit.CELSIUS:
        kelvin = value + 273.15
    elif from_unit == TemperatureUnit.FAHRENHEIT:
        kelvin = (value - 32) * 5 / 9 + 273.15
    else:  # Kelvin
        kelvin = value

    # Convert from Kelvin
    if to_unit == TemperatureUnit.CELSIUS:
        return kelvin - 273.15
    elif to_unit == TemperatureUnit.FAHRENHEIT:
        return (kelvin - 273.15) * 9 / 5 + 32
    else:  # Kelvin
        return kelvin


def convert_time(value: float, from_unit: TimeUnit, to_unit: TimeUnit) -> float:
    """
    Convert time between units.

    Args:
        value: Value to convert
        from_unit: Source unit
        to_unit: Target unit

    Returns:
        Converted value
    """
    secs = from_unit.to_seconds(value)
    return to_unit.from_seconds(secs)


def convert_data(value: float, from_unit: DataUnit, to_unit: DataUnit) -> float:
    """
    Convert data size between units.

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
    bytes_val = from_unit.to_bytes(value)
    return to_unit.from_bytes(bytes_val)


class SafeUnit:
    """Safe unit conversion utilities."""

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
