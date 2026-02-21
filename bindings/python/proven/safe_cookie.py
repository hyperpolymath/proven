# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeCookie - HTTP cookie validation with injection prevention.

Provides safe cookie handling per RFC 6265.
"""

from typing import Optional, List
from dataclasses import dataclass, field
from enum import Enum
from datetime import datetime
import re


class SameSite(Enum):
    """SameSite cookie attribute values."""
    STRICT = "Strict"
    LAX = "Lax"
    NONE = "None"


class CookiePrefix(Enum):
    """Cookie security prefixes."""
    NONE = ""
    SECURE = "__Secure-"
    HOST = "__Host-"


@dataclass
class CookieAttributes:
    """Cookie attributes for Set-Cookie header."""
    expires: Optional[datetime] = None
    max_age: Optional[int] = None
    domain: Optional[str] = None
    path: Optional[str] = None
    secure: bool = False
    http_only: bool = False
    same_site: Optional[SameSite] = None


@dataclass
class Cookie:
    """Validated HTTP cookie."""
    name: str
    value: str
    attributes: CookieAttributes = field(default_factory=CookieAttributes)

    def to_set_cookie(self) -> str:
        """Format as Set-Cookie header value."""
        parts = [f"{self.name}={self.value}"]

        if self.attributes.expires:
            parts.append(f"Expires={self.attributes.expires.strftime('%a, %d %b %Y %H:%M:%S GMT')}")
        if self.attributes.max_age is not None:
            parts.append(f"Max-Age={self.attributes.max_age}")
        if self.attributes.domain:
            parts.append(f"Domain={self.attributes.domain}")
        if self.attributes.path:
            parts.append(f"Path={self.attributes.path}")
        if self.attributes.secure:
            parts.append("Secure")
        if self.attributes.http_only:
            parts.append("HttpOnly")
        if self.attributes.same_site:
            parts.append(f"SameSite={self.attributes.same_site.value}")

        return "; ".join(parts)

    def to_cookie_header(self) -> str:
        """Format as Cookie header value (name=value only)."""
        return f"{self.name}={self.value}"


class SafeCookie:
    """Safe cookie validation and parsing."""

    # Valid cookie name per RFC 6265 (token)
    _NAME_PATTERN = re.compile(r"^[!#$%&'*+\-.^_`|~0-9A-Za-z]+$")
    # Invalid characters in cookie value
    _INVALID_VALUE_CHARS = re.compile(r'[\x00-\x1f\x7f\s";\\,]')

    @staticmethod
    def validate_name(name: str) -> bool:
        """
        Validate cookie name per RFC 6265.

        Args:
            name: Cookie name to validate

        Returns:
            True if valid
        """
        if not name:
            return False
        return bool(SafeCookie._NAME_PATTERN.match(name))

    @staticmethod
    def validate_value(value: str) -> bool:
        """
        Validate cookie value per RFC 6265.

        Args:
            value: Cookie value to validate

        Returns:
            True if valid (no CTLs, spaces, quotes, etc.)
        """
        return not bool(SafeCookie._INVALID_VALUE_CHARS.search(value))

    @staticmethod
    def create(name: str, value: str, attributes: Optional[CookieAttributes] = None) -> Optional[Cookie]:
        """
        Create a validated cookie.

        Args:
            name: Cookie name
            value: Cookie value
            attributes: Optional attributes

        Returns:
            Cookie object, or None if validation fails

        Example:
            >>> SafeCookie.create("session", "abc123")
            Cookie(name='session', value='abc123', ...)
        """
        if not SafeCookie.validate_name(name):
            return None
        if not SafeCookie.validate_value(value):
            return None

        attrs = attributes or CookieAttributes()

        # Validate prefix requirements
        if name.startswith("__Secure-") and not attrs.secure:
            return None  # __Secure- requires Secure attribute
        if name.startswith("__Host-"):
            if not attrs.secure or attrs.domain or attrs.path != "/":
                return None  # __Host- has strict requirements

        return Cookie(name=name, value=value, attributes=attrs)

    @staticmethod
    def create_secure(name: str, value: str, **kwargs) -> Optional[Cookie]:
        """
        Create a secure cookie with recommended settings.

        Args:
            name: Cookie name
            value: Cookie value
            **kwargs: Additional attributes

        Returns:
            Cookie with Secure, HttpOnly, and SameSite=Strict
        """
        attrs = CookieAttributes(
            secure=True,
            http_only=True,
            same_site=SameSite.STRICT,
            path=kwargs.get("path", "/"),
        )
        return SafeCookie.create(name, value, attrs)

    @staticmethod
    def parse_cookie_header(header: str) -> List[Cookie]:
        """
        Parse Cookie header value.

        Args:
            header: Cookie header value (name=value; name=value; ...)

        Returns:
            List of parsed cookies
        """
        cookies = []
        for pair in header.split(";"):
            pair = pair.strip()
            if not pair:
                continue
            eq = pair.find("=")
            if eq <= 0:
                continue
            name = pair[:eq].strip()
            value = pair[eq + 1:].strip()
            cookie = SafeCookie.create(name, value)
            if cookie:
                cookies.append(cookie)
        return cookies

    @staticmethod
    def sanitize_value(value: str) -> str:
        """
        Remove invalid characters from cookie value.

        Args:
            value: Value to sanitize

        Returns:
            Sanitized value
        """
        return SafeCookie._INVALID_VALUE_CHARS.sub("", value)

    @staticmethod
    def format_cookie_header(cookies: List[Cookie]) -> str:
        """
        Format cookies as Cookie header value.

        Args:
            cookies: List of cookies

        Returns:
            Cookie header value
        """
        return "; ".join(c.to_cookie_header() for c in cookies)
