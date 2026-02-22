# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeHeader - HTTP header validation without CRLF injection.

Provides safe HTTP header handling with injection prevention.
All validation is delegated to the Idris core via FFI.
"""

from typing import Optional, List
from dataclasses import dataclass

from .core import ProvenStatus, get_lib


@dataclass
class Header:
    """Validated HTTP header."""
    name: str
    value: str

    def to_string(self) -> str:
        """Format as HTTP header line."""
        return f"{self.name}: {self.value}"


class SafeHeader:
    """Safe HTTP header operations with CRLF injection prevention via FFI."""

    # Standard header names (case-insensitive canonical forms)
    CONTENT_TYPE = "Content-Type"
    CONTENT_LENGTH = "Content-Length"
    AUTHORIZATION = "Authorization"
    CACHE_CONTROL = "Cache-Control"
    COOKIE = "Cookie"
    SET_COOKIE = "Set-Cookie"
    HOST = "Host"
    USER_AGENT = "User-Agent"
    ACCEPT = "Accept"
    ACCEPT_ENCODING = "Accept-Encoding"
    CONTENT_ENCODING = "Content-Encoding"
    LOCATION = "Location"
    X_FORWARDED_FOR = "X-Forwarded-For"
    X_REAL_IP = "X-Real-IP"

    @staticmethod
    def validate_name(name: str) -> bool:
        """
        Check if header name is valid per RFC 7230 via FFI.

        Args:
            name: Header name to validate

        Returns:
            True if valid, False otherwise
        """
        if not name:
            return False
        lib = get_lib()
        encoded = name.encode("utf-8")
        result = lib.proven_header_validate_name(encoded, len(encoded))
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    @staticmethod
    def validate_value(value: str) -> bool:
        """
        Check if header value is safe (no CRLF injection) via FFI.

        Args:
            value: Header value to validate

        Returns:
            True if safe, False if contains injection characters
        """
        lib = get_lib()
        encoded = value.encode("utf-8")
        result = lib.proven_header_validate_value(encoded, len(encoded))
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    @staticmethod
    def create(name: str, value: str) -> Optional[Header]:
        """
        Create a validated header via FFI.

        Args:
            name: Header name
            value: Header value

        Returns:
            Header object, or None if validation fails

        Example:
            >>> SafeHeader.create("Content-Type", "application/json")
            Header(name='Content-Type', value='application/json')
            >>> SafeHeader.create("Bad\\r\\nHeader", "value")
            None
        """
        if not SafeHeader.validate_name(name):
            return None
        if not SafeHeader.validate_value(value):
            return None
        return Header(name=name, value=value)

    @staticmethod
    def sanitize_value(value: str) -> str:
        """
        Remove dangerous characters from header value via FFI.

        Args:
            value: Header value to sanitize

        Returns:
            Sanitized value with CR/LF/NUL removed
        """
        lib = get_lib()
        encoded = value.encode("utf-8")
        result = lib.proven_header_sanitize_value(encoded, len(encoded))
        if result.status != ProvenStatus.OK or result.value is None:
            return ""
        output = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)
        return output

    @staticmethod
    def parse(line: str) -> Optional[Header]:
        """
        Parse a header line (name: value) via FFI.

        Args:
            line: Raw header line

        Returns:
            Header object, or None if parsing fails
        """
        if not line:
            return None

        lib = get_lib()
        encoded = line.encode("utf-8")
        result = lib.proven_header_parse(encoded, len(encoded))
        if result.status != ProvenStatus.OK or result.value is None:
            return None

        # FFI returns "name\0value" format
        raw = result.value[:result.length]
        lib.proven_free_string(result.value)

        parts = raw.split(b"\x00", 1)
        if len(parts) != 2:
            return None

        name = parts[0].decode("utf-8")
        value = parts[1].decode("utf-8")
        return Header(name=name, value=value)

    @staticmethod
    def parse_headers(text: str) -> List[Header]:
        """
        Parse multiple header lines.

        Args:
            text: Multi-line header text

        Returns:
            List of valid headers (invalid lines are skipped)
        """
        headers = []
        for line in text.split("\n"):
            line = line.strip()
            if not line:
                continue
            header = SafeHeader.parse(line)
            if header:
                headers.append(header)
        return headers

    @staticmethod
    def format_headers(headers: List[Header]) -> str:
        """
        Format headers as HTTP header block.

        Args:
            headers: List of headers

        Returns:
            Formatted header string with CRLF line endings
        """
        return "\r\n".join(h.to_string() for h in headers) + "\r\n"

    @staticmethod
    def get(headers: List[Header], name: str) -> Optional[str]:
        """
        Get header value by name (case-insensitive).

        Args:
            headers: List of headers
            name: Header name to find

        Returns:
            Header value, or None if not found
        """
        name_lower = name.lower()
        for header in headers:
            if header.name.lower() == name_lower:
                return header.value
        return None

    @staticmethod
    def get_all(headers: List[Header], name: str) -> List[str]:
        """
        Get all values for a header name (case-insensitive).

        Args:
            headers: List of headers
            name: Header name to find

        Returns:
            List of all values for that header
        """
        name_lower = name.lower()
        return [h.value for h in headers if h.name.lower() == name_lower]
