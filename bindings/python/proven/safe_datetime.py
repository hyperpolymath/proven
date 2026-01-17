# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeDateTime - Date and time operations that cannot crash.

Provides exception-free ISO 8601 parsing, timezone handling, and date arithmetic.
"""

from typing import Optional, Tuple
from datetime import datetime, timezone, timedelta
import re


class SafeDateTime:
    """Exception-free date and time operations."""

    # ISO 8601 patterns
    _ISO_DATE = re.compile(r"^(\d{4})-(\d{2})-(\d{2})$")
    _ISO_DATETIME = re.compile(
        r"^(\d{4})-(\d{2})-(\d{2})[T ](\d{2}):(\d{2}):(\d{2})(?:\.(\d+))?"
        r"(?:Z|([+-]\d{2}):?(\d{2}))?$"
    )
    _ISO_TIME = re.compile(r"^(\d{2}):(\d{2}):(\d{2})(?:\.(\d+))?$")

    @staticmethod
    def parse_iso8601(text: str) -> Optional[datetime]:
        """
        Parse ISO 8601 datetime string.

        Args:
            text: ISO 8601 formatted string

        Returns:
            datetime object, or None if parsing fails

        Example:
            >>> SafeDateTime.parse_iso8601("2025-01-17T10:30:00Z")
            datetime.datetime(2025, 1, 17, 10, 30, tzinfo=timezone.utc)
        """
        try:
            # Try Python's fromisoformat (3.7+)
            return datetime.fromisoformat(text.replace("Z", "+00:00"))
        except (ValueError, AttributeError):
            pass

        # Manual parsing fallback
        match = SafeDateTime._ISO_DATETIME.match(text)
        if match:
            try:
                year, month, day = int(match.group(1)), int(match.group(2)), int(match.group(3))
                hour, minute, second = int(match.group(4)), int(match.group(5)), int(match.group(6))
                microsecond = int((match.group(7) or "0").ljust(6, "0")[:6])

                tz = timezone.utc
                if match.group(8):
                    tz_hours = int(match.group(8))
                    tz_mins = int(match.group(9) or "0")
                    tz = timezone(timedelta(hours=tz_hours, minutes=tz_mins))

                return datetime(year, month, day, hour, minute, second, microsecond, tzinfo=tz)
            except (ValueError, OverflowError):
                return None

        return None

    @staticmethod
    def parse_date(text: str) -> Optional[Tuple[int, int, int]]:
        """
        Parse ISO 8601 date string.

        Args:
            text: Date string (YYYY-MM-DD)

        Returns:
            Tuple of (year, month, day), or None if parsing fails
        """
        match = SafeDateTime._ISO_DATE.match(text)
        if match:
            try:
                year, month, day = int(match.group(1)), int(match.group(2)), int(match.group(3))
                # Validate
                datetime(year, month, day)
                return (year, month, day)
            except (ValueError, OverflowError):
                return None
        return None

    @staticmethod
    def parse_time(text: str) -> Optional[Tuple[int, int, int, int]]:
        """
        Parse ISO 8601 time string.

        Args:
            text: Time string (HH:MM:SS or HH:MM:SS.mmm)

        Returns:
            Tuple of (hour, minute, second, microsecond), or None if parsing fails
        """
        match = SafeDateTime._ISO_TIME.match(text)
        if match:
            try:
                hour, minute, second = int(match.group(1)), int(match.group(2)), int(match.group(3))
                microsecond = int((match.group(4) or "0").ljust(6, "0")[:6])
                # Validate
                if 0 <= hour < 24 and 0 <= minute < 60 and 0 <= second < 60:
                    return (hour, minute, second, microsecond)
            except (ValueError, OverflowError):
                return None
        return None

    @staticmethod
    def format_iso8601(dt: datetime) -> str:
        """
        Format datetime as ISO 8601 string.

        Args:
            dt: datetime object

        Returns:
            ISO 8601 formatted string
        """
        return dt.isoformat()

    @staticmethod
    def now_utc() -> datetime:
        """Get current UTC datetime."""
        return datetime.now(timezone.utc)

    @staticmethod
    def add_days(dt: datetime, days: int) -> Optional[datetime]:
        """
        Add days to datetime safely.

        Args:
            dt: datetime object
            days: Number of days to add (can be negative)

        Returns:
            New datetime, or None on overflow
        """
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
        """
        Get difference between two datetimes in seconds.

        Args:
            a: First datetime
            b: Second datetime

        Returns:
            Difference in seconds (a - b)
        """
        return (a - b).total_seconds()

    @staticmethod
    def diff_days(a: datetime, b: datetime) -> int:
        """Get difference between two datetimes in whole days."""
        return (a - b).days

    @staticmethod
    def is_leap_year(year: int) -> bool:
        """Check if a year is a leap year."""
        return year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)

    @staticmethod
    def days_in_month(year: int, month: int) -> Optional[int]:
        """
        Get number of days in a month.

        Args:
            year: The year
            month: The month (1-12)

        Returns:
            Number of days, or None if invalid month
        """
        if not 1 <= month <= 12:
            return None
        days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        result = days[month - 1]
        if month == 2 and SafeDateTime.is_leap_year(year):
            result = 29
        return result
