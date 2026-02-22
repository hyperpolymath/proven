# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeContentType - MIME type validation with sniffing prevention.

Provides safe Content-Type handling to prevent MIME confusion attacks.
All parsing and validation is delegated to the Idris core via FFI.
"""

import json
from typing import Optional, Dict
from dataclasses import dataclass
from enum import Enum

from .core import ProvenStatus, get_lib


class MediaCategory(Enum):
    """High-level media categories."""
    TEXT = "text"
    IMAGE = "image"
    AUDIO = "audio"
    VIDEO = "video"
    APPLICATION = "application"
    MULTIPART = "multipart"
    MESSAGE = "message"
    FONT = "font"
    MODEL = "model"
    UNKNOWN = "unknown"


@dataclass
class ContentType:
    """Parsed Content-Type header."""
    type: str
    subtype: str
    parameters: Dict[str, str]

    @property
    def media_type(self) -> str:
        """Get full media type (type/subtype)."""
        return f"{self.type}/{self.subtype}"

    @property
    def category(self) -> MediaCategory:
        """Get media category."""
        try:
            return MediaCategory(self.type)
        except ValueError:
            return MediaCategory.UNKNOWN

    @property
    def charset(self) -> Optional[str]:
        """Get charset parameter if present."""
        return self.parameters.get("charset")

    @property
    def boundary(self) -> Optional[str]:
        """Get boundary parameter (for multipart)."""
        return self.parameters.get("boundary")

    def to_string(self) -> str:
        """Format as Content-Type header value."""
        result = self.media_type
        for key, value in self.parameters.items():
            if " " in value or ";" in value:
                value = f'"{value}"'
            result += f"; {key}={value}"
        return result


class SafeContentType:
    """Safe Content-Type parsing and validation via FFI."""

    # Common safe MIME types
    TEXT_PLAIN = "text/plain"
    TEXT_HTML = "text/html"
    TEXT_CSS = "text/css"
    TEXT_JAVASCRIPT = "text/javascript"
    APPLICATION_JSON = "application/json"
    APPLICATION_XML = "application/xml"
    APPLICATION_OCTET_STREAM = "application/octet-stream"
    APPLICATION_PDF = "application/pdf"
    IMAGE_PNG = "image/png"
    IMAGE_JPEG = "image/jpeg"
    IMAGE_GIF = "image/gif"
    IMAGE_WEBP = "image/webp"
    IMAGE_SVG = "image/svg+xml"
    MULTIPART_FORM_DATA = "multipart/form-data"
    APPLICATION_FORM_URLENCODED = "application/x-www-form-urlencoded"

    @staticmethod
    def parse(value: str) -> Optional[ContentType]:
        """
        Parse Content-Type header value via FFI.

        Args:
            value: Content-Type header value

        Returns:
            ContentType object, or None if parsing fails

        Example:
            >>> SafeContentType.parse("text/html; charset=utf-8")
            ContentType(type='text', subtype='html', parameters={'charset': 'utf-8'})
        """
        if not value:
            return None

        lib = get_lib()
        encoded = value.encode("utf-8")
        result = lib.proven_content_type_parse(encoded, len(encoded))
        if result.status != ProvenStatus.OK or result.value is None:
            return None

        json_str = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)

        try:
            parsed = json.loads(json_str)
            return ContentType(
                type=parsed.get("type", ""),
                subtype=parsed.get("subtype", ""),
                parameters=parsed.get("parameters", {}),
            )
        except (json.JSONDecodeError, KeyError, TypeError):
            return None

    @staticmethod
    def create(type_: str, subtype: str, **parameters) -> Optional[ContentType]:
        """
        Create a Content-Type with validation via FFI.

        Args:
            type_: Media type (e.g., "text")
            subtype: Media subtype (e.g., "html")
            **parameters: Additional parameters

        Returns:
            ContentType object, or None if invalid
        """
        # Validate by parsing the assembled content type
        parts = f"{type_}/{subtype}"
        if parameters:
            param_str = "; ".join(f"{k}={v}" for k, v in parameters.items())
            parts = f"{parts}; {param_str}"

        result = SafeContentType.parse(parts)
        if result is None:
            return None
        return result

    @staticmethod
    def is_dangerous(content_type: str) -> bool:
        """
        Check if content type can execute code via FFI.

        Args:
            content_type: Media type string

        Returns:
            True if type can execute code in browsers
        """
        if not content_type:
            return False
        lib = get_lib()
        encoded = content_type.encode("utf-8")
        result = lib.proven_content_type_is_dangerous(encoded, len(encoded))
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    @staticmethod
    def is_text(content_type: str) -> bool:
        """Check if content type is text-based."""
        parsed = SafeContentType.parse(content_type)
        if parsed:
            return parsed.type == "text" or parsed.media_type in {
                "application/json",
                "application/xml",
                "application/javascript",
            }
        return False

    @staticmethod
    def is_image(content_type: str) -> bool:
        """Check if content type is an image."""
        parsed = SafeContentType.parse(content_type)
        return parsed is not None and parsed.type == "image"

    @staticmethod
    def get_safe_for_inline(content_type: str) -> str:
        """
        Get a safe content type for inline display.

        Converts dangerous types to safe alternatives.

        Args:
            content_type: Original content type

        Returns:
            Safe content type for inline display
        """
        if SafeContentType.is_dangerous(content_type):
            return SafeContentType.TEXT_PLAIN
        return content_type

    @staticmethod
    def guess_from_extension(filename: str) -> str:
        """
        Guess content type from file extension via FFI.

        Args:
            filename: Filename or path

        Returns:
            Guessed content type (defaults to application/octet-stream)
        """
        if not filename:
            return SafeContentType.APPLICATION_OCTET_STREAM

        lib = get_lib()
        encoded = filename.encode("utf-8")
        result = lib.proven_content_type_guess_ext(encoded, len(encoded))
        if result.status != ProvenStatus.OK or result.value is None:
            return SafeContentType.APPLICATION_OCTET_STREAM
        output = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)
        return output

    @staticmethod
    def with_charset(media_type: str, charset: str = "utf-8") -> str:
        """
        Add charset parameter to content type.

        Args:
            media_type: Base media type
            charset: Character set

        Returns:
            Content type with charset
        """
        return f"{media_type}; charset={charset}"
