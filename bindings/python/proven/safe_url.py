# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeUrl - URL parsing that cannot crash.

Provides safe URL parsing and validation without regex catastrophic backtracking.
"""

from dataclasses import dataclass
from typing import Optional
import ctypes

from .core import ProvenStatus, ProvenError, get_lib, check_status, UrlComponents


@dataclass
class ParsedUrl:
    """A parsed URL with its components."""
    scheme: str
    host: str
    port: Optional[int]
    path: str
    query: Optional[str]
    fragment: Optional[str]

    def __str__(self) -> str:
        """Reconstruct the URL string."""
        result = f"{self.scheme}://{self.host}"
        if self.port is not None:
            result += f":{self.port}"
        result += self.path
        if self.query:
            result += f"?{self.query}"
        if self.fragment:
            result += f"#{self.fragment}"
        return result


class SafeUrl:
    """Safe URL operations with proven correctness guarantees."""

    @staticmethod
    def parse(url: str) -> Optional[ParsedUrl]:
        """
        Parse a URL into its components.

        Args:
            url: The URL string to parse

        Returns:
            ParsedUrl with components, or None if invalid

        Example:
            >>> url = SafeUrl.parse("https://example.com:8080/path?q=1#frag")
            >>> url.scheme
            "https"
            >>> url.host
            "example.com"
            >>> url.port
            8080
        """
        lib = get_lib()
        encoded = url.encode("utf-8")
        result = lib.proven_url_parse(encoded, len(encoded))

        if result.status != ProvenStatus.OK:
            return None

        components = result.components

        def decode_field(data: Optional[bytes], length: int) -> str:
            if data is None or length == 0:
                return ""
            return data[:length].decode("utf-8")

        parsed = ParsedUrl(
            scheme=decode_field(components.scheme, components.scheme_len),
            host=decode_field(components.host, components.host_len),
            port=components.port if components.has_port else None,
            path=decode_field(components.path, components.path_len) or "/",
            query=decode_field(components.query, components.query_len) or None,
            fragment=decode_field(components.fragment, components.fragment_len) or None,
        )

        # Free the URL components
        lib.proven_url_free(ctypes.byref(components))

        return parsed

    @staticmethod
    def is_valid(url: str) -> bool:
        """
        Check if a URL is valid.

        Args:
            url: The URL to validate

        Returns:
            True if valid, False otherwise
        """
        return SafeUrl.parse(url) is not None

    @staticmethod
    def get_host(url: str) -> Optional[str]:
        """
        Extract the host from a URL.

        Args:
            url: The URL

        Returns:
            The host, or None if invalid
        """
        parsed = SafeUrl.parse(url)
        if parsed is None:
            return None
        return parsed.host

    @staticmethod
    def get_scheme(url: str) -> Optional[str]:
        """
        Extract the scheme from a URL.

        Args:
            url: The URL

        Returns:
            The scheme, or None if invalid
        """
        parsed = SafeUrl.parse(url)
        if parsed is None:
            return None
        return parsed.scheme

    @staticmethod
    def is_https(url: str) -> bool:
        """
        Check if a URL uses HTTPS.

        Args:
            url: The URL to check

        Returns:
            True if HTTPS, False otherwise
        """
        scheme = SafeUrl.get_scheme(url)
        return scheme is not None and scheme.lower() == "https"
