# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeColor - RGB/RGBA colors with WCAG contrast calculations.

Provides safe color operations including accessibility contrast checks.
"""

from typing import Optional, Tuple
from dataclasses import dataclass
import re


@dataclass(frozen=True)
class Rgb:
    """RGB color with values 0-255."""
    r: int
    g: int
    b: int

    def __post_init__(self):
        for c, v in [("r", self.r), ("g", self.g), ("b", self.b)]:
            if not 0 <= v <= 255:
                raise ValueError(f"{c} must be 0-255, got {v}")

    def to_hex(self) -> str:
        """Convert to hex string (#RRGGBB)."""
        return f"#{self.r:02x}{self.g:02x}{self.b:02x}"

    def to_tuple(self) -> Tuple[int, int, int]:
        """Convert to tuple."""
        return (self.r, self.g, self.b)

    def luminance(self) -> float:
        """
        Calculate relative luminance per WCAG 2.0.

        Returns:
            Luminance value (0.0 to 1.0)
        """
        def channel(c: int) -> float:
            s = c / 255.0
            return s / 12.92 if s <= 0.03928 else ((s + 0.055) / 1.055) ** 2.4

        return 0.2126 * channel(self.r) + 0.7152 * channel(self.g) + 0.0722 * channel(self.b)

    def contrast_ratio(self, other: "Rgb") -> float:
        """
        Calculate contrast ratio with another color per WCAG 2.0.

        Args:
            other: Color to compare against

        Returns:
            Contrast ratio (1.0 to 21.0)
        """
        l1 = self.luminance()
        l2 = other.luminance()
        lighter = max(l1, l2)
        darker = min(l1, l2)
        return (lighter + 0.05) / (darker + 0.05)

    def meets_wcag_aa(self, other: "Rgb", large_text: bool = False) -> bool:
        """
        Check if contrast meets WCAG 2.0 AA.

        Args:
            other: Background/foreground color
            large_text: True for large text (lower requirement)

        Returns:
            True if meets AA requirements
        """
        ratio = self.contrast_ratio(other)
        threshold = 3.0 if large_text else 4.5
        return ratio >= threshold

    def meets_wcag_aaa(self, other: "Rgb", large_text: bool = False) -> bool:
        """
        Check if contrast meets WCAG 2.0 AAA.

        Args:
            other: Background/foreground color
            large_text: True for large text (lower requirement)

        Returns:
            True if meets AAA requirements
        """
        ratio = self.contrast_ratio(other)
        threshold = 4.5 if large_text else 7.0
        return ratio >= threshold

    def with_alpha(self, alpha: int) -> "Rgba":
        """Create RGBA with given alpha."""
        return Rgba(self.r, self.g, self.b, alpha)


@dataclass(frozen=True)
class Rgba:
    """RGBA color with values 0-255."""
    r: int
    g: int
    b: int
    a: int

    def __post_init__(self):
        for c, v in [("r", self.r), ("g", self.g), ("b", self.b), ("a", self.a)]:
            if not 0 <= v <= 255:
                raise ValueError(f"{c} must be 0-255, got {v}")

    def to_hex(self) -> str:
        """Convert to hex string (#RRGGBBAA)."""
        return f"#{self.r:02x}{self.g:02x}{self.b:02x}{self.a:02x}"

    def to_rgb(self) -> Rgb:
        """Convert to RGB (discards alpha)."""
        return Rgb(self.r, self.g, self.b)

    def to_tuple(self) -> Tuple[int, int, int, int]:
        """Convert to tuple."""
        return (self.r, self.g, self.b, self.a)

    @property
    def alpha_normalized(self) -> float:
        """Get alpha as 0.0-1.0."""
        return self.a / 255.0


class SafeColor:
    """Safe color operations."""

    # Common colors
    BLACK = Rgb(0, 0, 0)
    WHITE = Rgb(255, 255, 255)
    RED = Rgb(255, 0, 0)
    GREEN = Rgb(0, 255, 0)
    BLUE = Rgb(0, 0, 255)
    YELLOW = Rgb(255, 255, 0)
    CYAN = Rgb(0, 255, 255)
    MAGENTA = Rgb(255, 0, 255)

    _HEX_PATTERN = re.compile(r"^#?([0-9a-fA-F]{6}|[0-9a-fA-F]{8}|[0-9a-fA-F]{3}|[0-9a-fA-F]{4})$")

    @staticmethod
    def from_hex(hex_str: str) -> Optional[Rgb]:
        """
        Parse hex color string.

        Supports: #RGB, #RRGGBB, RGB, RRGGBB

        Args:
            hex_str: Hex color string

        Returns:
            Rgb color, or None if invalid

        Example:
            >>> SafeColor.from_hex("#ff0000")
            Rgb(r=255, g=0, b=0)
            >>> SafeColor.from_hex("f00")
            Rgb(r=255, g=0, b=0)
        """
        match = SafeColor._HEX_PATTERN.match(hex_str)
        if not match:
            return None

        hex_part = match.group(1)

        try:
            if len(hex_part) == 3:
                # #RGB -> #RRGGBB
                r = int(hex_part[0] * 2, 16)
                g = int(hex_part[1] * 2, 16)
                b = int(hex_part[2] * 2, 16)
            elif len(hex_part) == 4:
                # #RGBA -> use RGB only
                r = int(hex_part[0] * 2, 16)
                g = int(hex_part[1] * 2, 16)
                b = int(hex_part[2] * 2, 16)
            elif len(hex_part) == 6:
                r = int(hex_part[0:2], 16)
                g = int(hex_part[2:4], 16)
                b = int(hex_part[4:6], 16)
            else:  # 8
                r = int(hex_part[0:2], 16)
                g = int(hex_part[2:4], 16)
                b = int(hex_part[4:6], 16)
            return Rgb(r, g, b)
        except ValueError:
            return None

    @staticmethod
    def from_hex_rgba(hex_str: str) -> Optional[Rgba]:
        """
        Parse hex color string with alpha.

        Supports: #RGBA, #RRGGBBAA

        Args:
            hex_str: Hex color string

        Returns:
            Rgba color, or None if invalid
        """
        match = SafeColor._HEX_PATTERN.match(hex_str)
        if not match:
            return None

        hex_part = match.group(1)

        try:
            if len(hex_part) == 4:
                r = int(hex_part[0] * 2, 16)
                g = int(hex_part[1] * 2, 16)
                b = int(hex_part[2] * 2, 16)
                a = int(hex_part[3] * 2, 16)
            elif len(hex_part) == 8:
                r = int(hex_part[0:2], 16)
                g = int(hex_part[2:4], 16)
                b = int(hex_part[4:6], 16)
                a = int(hex_part[6:8], 16)
            else:
                # No alpha provided, assume 255
                rgb = SafeColor.from_hex(hex_str)
                if rgb:
                    return Rgba(rgb.r, rgb.g, rgb.b, 255)
                return None
            return Rgba(r, g, b, a)
        except ValueError:
            return None

    @staticmethod
    def blend(fg: Rgba, bg: Rgb) -> Rgb:
        """
        Blend foreground RGBA over background RGB.

        Args:
            fg: Foreground color with alpha
            bg: Background color

        Returns:
            Blended RGB color
        """
        alpha = fg.a / 255.0
        inv_alpha = 1.0 - alpha

        r = int(fg.r * alpha + bg.r * inv_alpha)
        g = int(fg.g * alpha + bg.g * inv_alpha)
        b = int(fg.b * alpha + bg.b * inv_alpha)

        return Rgb(
            max(0, min(255, r)),
            max(0, min(255, g)),
            max(0, min(255, b)),
        )

    @staticmethod
    def interpolate(c1: Rgb, c2: Rgb, t: float) -> Rgb:
        """
        Linear interpolation between two colors.

        Args:
            c1: Start color
            c2: End color
            t: Interpolation factor (0.0-1.0)

        Returns:
            Interpolated color
        """
        t = max(0.0, min(1.0, t))
        r = int(c1.r + (c2.r - c1.r) * t)
        g = int(c1.g + (c2.g - c1.g) * t)
        b = int(c1.b + (c2.b - c1.b) * t)
        return Rgb(r, g, b)

    @staticmethod
    def contrast_ratio(c1: Rgb, c2: Rgb) -> float:
        """Calculate WCAG contrast ratio between two colors."""
        return c1.contrast_ratio(c2)

    @staticmethod
    def suggest_text_color(background: Rgb) -> Rgb:
        """
        Suggest black or white text for a background.

        Args:
            background: Background color

        Returns:
            Black or white, whichever has better contrast
        """
        black_contrast = background.contrast_ratio(SafeColor.BLACK)
        white_contrast = background.contrast_ratio(SafeColor.WHITE)
        return SafeColor.BLACK if black_contrast >= white_contrast else SafeColor.WHITE
