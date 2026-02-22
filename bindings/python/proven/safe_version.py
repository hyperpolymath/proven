# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeVersion - Semantic versioning parsing and comparison.

Provides SemVer 2.0.0 compliant version handling.
All parsing and comparison is delegated to the Idris core via FFI.
"""

import json
from typing import Optional, List
from dataclasses import dataclass
from functools import total_ordering

from .core import ProvenStatus, get_lib


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

        lib = get_lib()
        a_str = str(self).encode("utf-8")
        b_str = str(other).encode("utf-8")
        result = lib.proven_version_compare(a_str, len(a_str), b_str, len(b_str))
        if result.status != ProvenStatus.OK:
            return False
        return result.value < 0

    def __hash__(self) -> int:
        return hash((self.major, self.minor, self.patch, self.prerelease))

    @classmethod
    def parse(cls, version_str: str) -> Optional["Version"]:
        """
        Parse a version string via FFI.

        Args:
            version_str: Version string (e.g., "1.2.3", "v1.2.3-alpha+build")

        Returns:
            Version object, or None if invalid

        Example:
            >>> Version.parse("1.2.3")
            Version(major=1, minor=2, patch=3, prerelease=None, build=None)
        """
        if not version_str:
            return None

        lib = get_lib()
        encoded = version_str.strip().encode("utf-8")
        result = lib.proven_version_parse_semver(encoded, len(encoded))
        if result.status != ProvenStatus.OK or result.value is None:
            return None

        json_str = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)

        try:
            parsed = json.loads(json_str)
            return cls(
                major=parsed["major"],
                minor=parsed["minor"],
                patch=parsed["patch"],
                prerelease=parsed.get("prerelease"),
                build=parsed.get("build"),
            )
        except (json.JSONDecodeError, KeyError, TypeError, ValueError):
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
        Check if version satisfies a constraint via FFI.

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
        lib = get_lib()
        v_str = str(self).encode("utf-8")
        c_str = constraint.strip().encode("utf-8")
        result = lib.proven_version_satisfies(v_str, len(v_str), c_str, len(c_str))
        if result.status != ProvenStatus.OK:
            return False
        return result.value


class SafeVersion:
    """Utility functions for version handling via FFI."""

    @staticmethod
    def parse(version_str: str) -> Optional[Version]:
        """Parse a version string."""
        return Version.parse(version_str)

    @staticmethod
    def compare(a: str, b: str) -> int:
        """
        Compare two version strings via FFI.

        Args:
            a: First version
            b: Second version

        Returns:
            -1 if a < b, 0 if a == b, 1 if a > b
        """
        lib = get_lib()
        a_enc = a.encode("utf-8")
        b_enc = b.encode("utf-8")
        result = lib.proven_version_compare(a_enc, len(a_enc), b_enc, len(b_enc))
        if result.status != ProvenStatus.OK:
            return 0
        if result.value < 0:
            return -1
        elif result.value > 0:
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
