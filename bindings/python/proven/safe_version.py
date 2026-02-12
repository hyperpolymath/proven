# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeVersion - Semantic versioning parsing and comparison.

Provides SemVer 2.0.0 compliant version handling.
"""

from typing import Optional, Tuple, List
from dataclasses import dataclass
from functools import total_ordering
import re


@total_ordering
@dataclass
class Version:
    """
    Semantic version per SemVer 2.0.0.

    Example:
        >>> v = Version.parse("1.2.3-alpha.1+build.456")
        >>> v.major
        1
        >>> v.minor
        2
        >>> v.patch
        3
        >>> v.prerelease
        'alpha.1'
    """
    major: int
    minor: int
    patch: int
    prerelease: Optional[str] = None
    build: Optional[str] = None

    _SEMVER_PATTERN = re.compile(
        r"^v?"  # Optional v prefix
        r"(\d+)\.(\d+)\.(\d+)"  # Major.Minor.Patch
        r"(?:-([0-9A-Za-z\-.]+))?"  # Optional prerelease
        r"(?:\+([0-9A-Za-z\-.]+))?$"  # Optional build metadata
    )

    def __str__(self) -> str:
        """Format as version string."""
        s = f"{self.major}.{self.minor}.{self.patch}"
        if self.prerelease:
            s += f"-{self.prerelease}"
        if self.build:
            s += f"+{self.build}"
        return s

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Version):
            return NotImplemented
        # Build metadata is ignored in equality
        return (
            self.major == other.major
            and self.minor == other.minor
            and self.patch == other.patch
            and self.prerelease == other.prerelease
        )

    def __lt__(self, other: "Version") -> bool:
        if not isinstance(other, Version):
            return NotImplemented

        # Compare major.minor.patch
        if (self.major, self.minor, self.patch) != (other.major, other.minor, other.patch):
            return (self.major, self.minor, self.patch) < (other.major, other.minor, other.patch)

        # Prerelease has lower precedence than normal
        if self.prerelease is None and other.prerelease is not None:
            return False
        if self.prerelease is not None and other.prerelease is None:
            return True
        if self.prerelease is None and other.prerelease is None:
            return False

        # Compare prerelease identifiers
        return self._compare_prerelease(self.prerelease, other.prerelease) < 0

    @staticmethod
    def _compare_prerelease(a: str, b: str) -> int:
        """Compare prerelease strings per SemVer spec."""
        a_parts = a.split(".")
        b_parts = b.split(".")

        for i in range(max(len(a_parts), len(b_parts))):
            if i >= len(a_parts):
                return -1  # a is shorter, so less
            if i >= len(b_parts):
                return 1  # b is shorter, so less

            a_part = a_parts[i]
            b_part = b_parts[i]

            # Numeric comparison if both are numbers
            a_is_num = a_part.isdigit()
            b_is_num = b_part.isdigit()

            if a_is_num and b_is_num:
                a_num, b_num = int(a_part), int(b_part)
                if a_num != b_num:
                    return -1 if a_num < b_num else 1
            elif a_is_num:
                return -1  # Numeric has lower precedence
            elif b_is_num:
                return 1
            else:
                # Lexicographic comparison
                if a_part != b_part:
                    return -1 if a_part < b_part else 1

        return 0

    def __hash__(self) -> int:
        return hash((self.major, self.minor, self.patch, self.prerelease))

    @classmethod
    def parse(cls, version_str: str) -> Optional["Version"]:
        """
        Parse a version string.

        Args:
            version_str: Version string (e.g., "1.2.3", "v1.2.3-alpha+build")

        Returns:
            Version object, or None if invalid

        Example:
            >>> Version.parse("1.2.3")
            Version(major=1, minor=2, patch=3, prerelease=None, build=None)
        """
        match = cls._SEMVER_PATTERN.match(version_str.strip())
        if not match:
            return None

        try:
            return cls(
                major=int(match.group(1)),
                minor=int(match.group(2)),
                patch=int(match.group(3)),
                prerelease=match.group(4),
                build=match.group(5),
            )
        except (ValueError, OverflowError):
            return None

    def bump_major(self) -> "Version":
        """Return new version with major incremented."""
        return Version(self.major + 1, 0, 0)

    def bump_minor(self) -> "Version":
        """Return new version with minor incremented."""
        return Version(self.major, self.minor + 1, 0)

    def bump_patch(self) -> "Version":
        """Return new version with patch incremented."""
        return Version(self.major, self.minor, self.patch + 1)

    def with_prerelease(self, prerelease: str) -> "Version":
        """Return new version with prerelease."""
        return Version(self.major, self.minor, self.patch, prerelease, self.build)

    def with_build(self, build: str) -> "Version":
        """Return new version with build metadata."""
        return Version(self.major, self.minor, self.patch, self.prerelease, build)

    def is_prerelease(self) -> bool:
        """Check if this is a prerelease version."""
        return self.prerelease is not None

    def is_stable(self) -> bool:
        """Check if this is a stable release (no prerelease, major > 0)."""
        return self.prerelease is None and self.major > 0

    def satisfies(self, constraint: str) -> bool:
        """
        Check if version satisfies a constraint.

        Supports: =, !=, >, <, >=, <=, ^, ~

        Args:
            constraint: Version constraint string

        Returns:
            True if version satisfies constraint

        Example:
            >>> Version.parse("1.2.3").satisfies(">=1.0.0")
            True
            >>> Version.parse("1.2.3").satisfies("^1.2.0")
            True
        """
        constraint = constraint.strip()

        # Parse operator and version
        if constraint.startswith(">="):
            op, ver_str = ">=", constraint[2:]
        elif constraint.startswith("<="):
            op, ver_str = "<=", constraint[2:]
        elif constraint.startswith("!="):
            op, ver_str = "!=", constraint[2:]
        elif constraint.startswith(">"):
            op, ver_str = ">", constraint[1:]
        elif constraint.startswith("<"):
            op, ver_str = "<", constraint[1:]
        elif constraint.startswith("="):
            op, ver_str = "=", constraint[1:]
        elif constraint.startswith("^"):
            op, ver_str = "^", constraint[1:]
        elif constraint.startswith("~"):
            op, ver_str = "~", constraint[1:]
        else:
            op, ver_str = "=", constraint

        other = Version.parse(ver_str.strip())
        if other is None:
            return False

        if op == "=":
            return self == other
        elif op == "!=":
            return self != other
        elif op == ">":
            return self > other
        elif op == "<":
            return self < other
        elif op == ">=":
            return self >= other
        elif op == "<=":
            return self <= other
        elif op == "^":
            # Compatible with (same major, minor >= other.minor)
            if self.major != other.major:
                return False
            return self >= other
        elif op == "~":
            # Approximately equivalent (same major.minor)
            return self.major == other.major and self.minor == other.minor and self >= other

        return False


class SafeVersion:
    """Utility functions for version handling."""

    @staticmethod
    def parse(version_str: str) -> Optional[Version]:
        """Parse a version string."""
        return Version.parse(version_str)

    @staticmethod
    def compare(a: str, b: str) -> int:
        """
        Compare two version strings.

        Args:
            a: First version
            b: Second version

        Returns:
            -1 if a < b, 0 if a == b, 1 if a > b, or None if invalid
        """
        va = Version.parse(a)
        vb = Version.parse(b)
        if va is None or vb is None:
            return 0
        if va < vb:
            return -1
        elif va > vb:
            return 1
        return 0

    @staticmethod
    def sort(versions: List[str]) -> List[str]:
        """
        Sort version strings.

        Args:
            versions: List of version strings

        Returns:
            Sorted list (invalid versions at end)
        """
        parsed = [(v, Version.parse(v)) for v in versions]
        valid = [(v, p) for v, p in parsed if p is not None]
        invalid = [v for v, p in parsed if p is None]

        valid.sort(key=lambda x: x[1])
        return [v for v, _ in valid] + invalid

    @staticmethod
    def latest(versions: List[str]) -> Optional[str]:
        """
        Get the latest (highest) version.

        Args:
            versions: List of version strings

        Returns:
            Latest version string, or None if empty/invalid
        """
        sorted_versions = SafeVersion.sort(versions)
        return sorted_versions[-1] if sorted_versions else None
