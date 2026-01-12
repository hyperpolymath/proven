# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafePath - Filesystem path operations that cannot crash.

Provides path sanitization and traversal attack prevention.
"""

from typing import Optional

from .core import ProvenStatus, ProvenError, get_lib, check_status


class SafePath:
    """Safe path operations with proven correctness guarantees."""

    @staticmethod
    def has_traversal(path: str) -> bool:
        """
        Check if a path contains directory traversal sequences.

        Detects ../ and similar attacks that could escape a base directory.

        Args:
            path: The path to check

        Returns:
            True if traversal detected, False if safe

        Example:
            >>> SafePath.has_traversal("foo/bar.txt")
            False
            >>> SafePath.has_traversal("../etc/passwd")
            True
            >>> SafePath.has_traversal("foo/../../../etc/passwd")
            True
        """
        lib = get_lib()
        encoded = path.encode("utf-8")
        result = lib.proven_path_has_traversal(encoded, len(encoded))
        if result.status != ProvenStatus.OK:
            # On error, assume unsafe
            return True
        return result.value

    @staticmethod
    def is_safe(path: str) -> bool:
        """
        Check if a path is safe (no traversal attacks).

        Convenience method, inverse of has_traversal.

        Args:
            path: The path to check

        Returns:
            True if safe, False if potentially malicious
        """
        return not SafePath.has_traversal(path)

    @staticmethod
    def sanitize_filename(filename: str) -> Optional[str]:
        """
        Sanitize a filename by removing dangerous characters.

        Removes path separators, null bytes, and other dangerous chars.

        Args:
            filename: The filename to sanitize

        Returns:
            Sanitized filename, or None on error

        Example:
            >>> SafePath.sanitize_filename("report.pdf")
            "report.pdf"
            >>> SafePath.sanitize_filename("../../../etc/passwd")
            "etc_passwd"
            >>> SafePath.sanitize_filename("file\\x00.txt")
            "file.txt"
        """
        lib = get_lib()
        encoded = filename.encode("utf-8")
        result = lib.proven_path_sanitize_filename(encoded, len(encoded))
        if result.status != ProvenStatus.OK:
            return None
        if result.value is None:
            return None
        sanitized = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)
        return sanitized

    @staticmethod
    def safe_join(base: str, *parts: str) -> Optional[str]:
        """
        Safely join path components, rejecting traversal attempts.

        Args:
            base: Base directory path
            *parts: Path components to join

        Returns:
            Joined path if safe, None if any component has traversal

        Example:
            >>> SafePath.safe_join("/var/data", "user", "file.txt")
            "/var/data/user/file.txt"
            >>> SafePath.safe_join("/var/data", "../etc/passwd")
            None
        """
        import os

        for part in parts:
            if SafePath.has_traversal(part):
                return None

        result = base
        for part in parts:
            # Sanitize each component
            sanitized = SafePath.sanitize_filename(part)
            if sanitized is None:
                return None
            result = os.path.join(result, sanitized)

        return result
