# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeString - String operations that cannot crash.

Provides safe UTF-8 validation and escaping for SQL, HTML, and JavaScript
without exceptions or security vulnerabilities.
"""

from typing import Optional

from .core import ProvenStatus, ProvenError, get_lib, check_status


class SafeString:
    """Safe string operations with proven correctness guarantees."""

    @staticmethod
    def is_valid_utf8(data: bytes) -> bool:
        """
        Check if bytes are valid UTF-8.

        Args:
            data: The bytes to validate

        Returns:
            True if valid UTF-8, False otherwise

        Example:
            >>> SafeString.is_valid_utf8(b"Hello")
            True
            >>> SafeString.is_valid_utf8(b"\\xff\\xfe")
            False
        """
        lib = get_lib()
        result = lib.proven_string_is_valid_utf8(data, len(data))
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    @staticmethod
    def escape_sql(value: str) -> Optional[str]:
        """
        Escape a string for safe SQL interpolation.

        Prevents SQL injection by escaping dangerous characters.

        Args:
            value: The string to escape

        Returns:
            Escaped string safe for SQL, or None on error

        Example:
            >>> SafeString.escape_sql("O'Brien")
            "O''Brien"
        """
        lib = get_lib()
        encoded = value.encode("utf-8")
        result = lib.proven_string_escape_sql(encoded, len(encoded))
        if result.status != ProvenStatus.OK:
            return None
        if result.value is None:
            return None
        escaped = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)
        return escaped

    @staticmethod
    def escape_html(value: str) -> Optional[str]:
        """
        Escape a string for safe HTML insertion.

        Prevents XSS by escaping < > & " ' characters.

        Args:
            value: The string to escape

        Returns:
            Escaped string safe for HTML, or None on error

        Example:
            >>> SafeString.escape_html("<script>alert(1)</script>")
            "&lt;script&gt;alert(1)&lt;/script&gt;"
        """
        lib = get_lib()
        encoded = value.encode("utf-8")
        result = lib.proven_string_escape_html(encoded, len(encoded))
        if result.status != ProvenStatus.OK:
            return None
        if result.value is None:
            return None
        escaped = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)
        return escaped

    @staticmethod
    def escape_js(value: str) -> Optional[str]:
        """
        Escape a string for safe JavaScript string literal insertion.

        Prevents XSS in JavaScript contexts.

        Args:
            value: The string to escape

        Returns:
            Escaped string safe for JS strings, or None on error

        Example:
            >>> SafeString.escape_js('alert("hi")')
            'alert(\\"hi\\")'
        """
        lib = get_lib()
        encoded = value.encode("utf-8")
        result = lib.proven_string_escape_js(encoded, len(encoded))
        if result.status != ProvenStatus.OK:
            return None
        if result.value is None:
            return None
        escaped = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)
        return escaped

    @staticmethod
    def safe_decode(data: bytes, fallback: str = "�") -> str:
        """
        Decode bytes to string, replacing invalid UTF-8 sequences.

        Args:
            data: The bytes to decode
            fallback: Character to use for invalid sequences (default: replacement char)

        Returns:
            Decoded string with invalid sequences replaced
        """
        if SafeString.is_valid_utf8(data):
            return data.decode("utf-8")
        return data.decode("utf-8", errors="replace")
