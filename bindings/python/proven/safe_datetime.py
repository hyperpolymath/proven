# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeDateTime - Date and time operations that cannot crash.

Provides exception-free ISO 8601 parsing, timezone handling, and date arithmetic.
Parsing and validation are delegated to the Idris core via FFI.
"""

from typing import Optional, Tuple
from datetime import datetime, timezone, timedelta

from .core import ProvenStatus, ProvenError, get_lib, check_status


class SafeDateTime:
    """Exception-free date and time operations via FFI."""

    @staticmethod
    def parse_iso8601(text: str) -> Optional[datetime]:
        """
        Parse ISO 8601 datetime string via FFI.

        Args:
            text: ISO 8601 formatted string

        Returns:
            datetime object, or None if parsing fails
        """
        lib = get_lib()
        encoded = text.encode("utf-8")
        result = lib.proven_datetime_parse_iso8601(encoded, len(encoded))
        if result.status != ProvenStatus.OK or result.value is None:
            return None

        # FFI returns a normalized ISO string
        normalized = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)

        try:
            return datetime.fromisoformat(normalized.replace("Z", "+00:00"))
        except (ValueError, AttributeError):
            return None

    @staticmethod
    def parse_date(text: str) -> Optional[Tuple[int, int, int]]:
        """
        Parse ISO 8601 date string via FFI.

        Args:
            text: Date string (YYYY-MM-DD)

        Returns:
            Tuple of (year, month, day), or None if parsing fails
        """
        lib = get_lib()
        encoded = text.encode("utf-8")
        result = lib.proven_datetime_parse_date(encoded, len(encoded))
        if result.status != ProvenStatus.OK or result.value is None:
            return None

        date_str = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)

        # Parse the validated date components
        try:
            parts = date_str.split("-")
            if len(parts) != 3:
                return None
            year, month, day = int(parts[0]), int(parts[1]), int(parts[2])
            # Validate via datetime constructor
            datetime(year, month, day)
            return (year, month, day)
        except (ValueError, OverflowError):
            return None

    @staticmethod
    def parse_time(text: str) -> Optional[Tuple[int, int, int, int]]:
        """
        Parse ISO 8601 time string via FFI.

        Args:
            text: Time string (HH:MM:SS or HH:MM:SS.mmm)

        Returns:
            Tuple of (hour, minute, second, microsecond), or None if parsing fails
        """
        lib = get_lib()
        encoded = text.encode("utf-8")
        result = lib.proven_datetime_parse_time(encoded, len(encoded))
        if result.status != ProvenStatus.OK or result.value is None:
            return None

        time_str = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)

        try:
            # Parse "HH:MM:SS" or "HH:MM:SS.ffffff"
            main_parts = time_str.split(".")
            hms = main_parts[0].split(":")
            if len(hms) != 3:
                return None
            hour, minute, second = int(hms[0]), int(hms[1]), int(hms[2])
            microsecond = int((main_parts[1] if len(main_parts) > 1 else "0").ljust(6, "0")[:6])
            if 0 <= hour < 24 and 0 <= minute < 60 and 0 <= second < 60:
                return (hour, minute, second, microsecond)
            return None
        except (ValueError, OverflowError):
            return None

    @staticmethod
    def format_iso8601(dt: datetime) -> str:
        """Format datetime as ISO 8601 string."""
        return dt.isoformat()

    @staticmethod
    def now_utc() -> datetime:
        """Get current UTC datetime."""
        return datetime.now(timezone.utc)

    @staticmethod
    def add_days(dt: datetime, days: int) -> Optional[datetime]:
        """Add days to datetime safely."""
        try:
            return dt + timedelta(days=days)
        except OverflowError:
            return None

    @staticmethod
    def add_hours(dt: datetime, hours: int) -> Optional[datetime]:
        """Add hours to datetime safely."""
        try:
            return dt + timedelta(hours=hours)
        except OverflowError:
            return None

    @staticmethod
    def diff_seconds(a: datetime, b: datetime) -> float:
        """Get difference between two datetimes in seconds."""
        return (a - b).total_seconds()

    @staticmethod
    def diff_days(a: datetime, b: datetime) -> int:
        """Get difference between two datetimes in whole days."""
        return (a - b).days

    @staticmethod
    def is_leap_year(year: int) -> bool:
        """Check if a year is a leap year via FFI."""
        lib = get_lib()
        result = lib.proven_datetime_is_leap_year(year)
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    @staticmethod
    def days_in_month(year: int, month: int) -> Optional[int]:
        """
        Get number of days in a month via FFI.

        Args:
            year: The year
            month: The month (1-12)

        Returns:
            Number of days, or None if invalid month
        """
        lib = get_lib()
        result = lib.proven_datetime_days_in_month(year, month)
        if result.status != ProvenStatus.OK:
            return None
        return result.value
