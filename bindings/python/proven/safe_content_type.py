# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeContentType - MIME type validation with sniffing prevention.

Provides safe Content-Type handling to prevent MIME confusion attacks.
"""

from typing import Optional, Dict, List, Tuple
from dataclasses import dataclass
from enum import Enum
import re


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
            # Quote value if needed
            if " " in value or ";" in value:
                value = f'"{value}"'
            result += f"; {key}={value}"
        return result


class SafeContentType:
    """Safe Content-Type parsing and validation."""

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

    # Dangerous types that can execute code
    DANGEROUS_TYPES = frozenset([
        "text/html",
        "application/javascript",
        "text/javascript",
        "application/x-javascript",
        "image/svg+xml",
        "application/xhtml+xml",
        "text/xml",
        "application/xml",
    ])

    # Pattern for media type
    _MEDIA_TYPE_PATTERN = re.compile(
        r"^([a-zA-Z0-9!#$&\-^_.+]+)/([a-zA-Z0-9!#$&\-^_.+]+)$"
    )

    @staticmethod
    def parse(value: str) -> Optional[ContentType]:
        """
        Parse Content-Type header value.

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

        parts = value.split(";")
        media_type = parts[0].strip().lower()

        match = SafeContentType._MEDIA_TYPE_PATTERN.match(media_type)
        if not match:
            return None

        type_part = match.group(1)
        subtype = match.group(2)

        # Parse parameters
        parameters: Dict[str, str] = {}
        for param in parts[1:]:
            param = param.strip()
            if not param:
                continue
            eq = param.find("=")
            if eq <= 0:
                continue
            key = param[:eq].strip().lower()
            val = param[eq + 1:].strip()
            # Remove quotes
            if val.startswith('"') and val.endswith('"'):
                val = val[1:-1]
            parameters[key] = val

        return ContentType(type=type_part, subtype=subtype, parameters=parameters)

    @staticmethod
    def create(type: str, subtype: str, **parameters) -> Optional[ContentType]:
        """
        Create a Content-Type with validation.

        Args:
            type: Media type (e.g., "text")
            subtype: Media subtype (e.g., "html")
            **parameters: Additional parameters

        Returns:
            ContentType object, or None if invalid
        """
        media_type = f"{type}/{subtype}"
        if not SafeContentType._MEDIA_TYPE_PATTERN.match(media_type):
            return None
        return ContentType(type=type.lower(), subtype=subtype.lower(), parameters=parameters)

    @staticmethod
    def is_dangerous(content_type: str) -> bool:
        """
        Check if content type can execute code.

        Args:
            content_type: Media type string

        Returns:
            True if type can execute code in browsers
        """
        parsed = SafeContentType.parse(content_type)
        if parsed:
            return parsed.media_type in SafeContentType.DANGEROUS_TYPES
        return content_type.lower().split(";")[0].strip() in SafeContentType.DANGEROUS_TYPES

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
        Guess content type from file extension.

        Args:
            filename: Filename or path

        Returns:
            Guessed content type (defaults to application/octet-stream)
        """
        ext_map = {
            ".txt": "text/plain",
            ".html": "text/html",
            ".htm": "text/html",
            ".css": "text/css",
            ".js": "text/javascript",
            ".json": "application/json",
            ".xml": "application/xml",
            ".pdf": "application/pdf",
            ".png": "image/png",
            ".jpg": "image/jpeg",
            ".jpeg": "image/jpeg",
            ".gif": "image/gif",
            ".webp": "image/webp",
            ".svg": "image/svg+xml",
            ".mp3": "audio/mpeg",
            ".mp4": "video/mp4",
            ".webm": "video/webm",
            ".woff": "font/woff",
            ".woff2": "font/woff2",
            ".zip": "application/zip",
            ".gz": "application/gzip",
        }

        dot = filename.rfind(".")
        if dot < 0:
            return SafeContentType.APPLICATION_OCTET_STREAM

        ext = filename[dot:].lower()
        return ext_map.get(ext, SafeContentType.APPLICATION_OCTET_STREAM)

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
