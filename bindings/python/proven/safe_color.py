# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeColor - RGB/RGBA colors with WCAG contrast calculations.

Provides safe color operations including accessibility contrast checks.
All computation is delegated to the Idris core via FFI.
"""

import ctypes
from typing import Optional, Tuple
from dataclasses import dataclass

from .core import ProvenStatus, get_lib


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
        Calculate relative luminance per WCAG 2.0 via FFI.

        Returns:
            Luminance value (0.0 to 1.0)
        """
        lib = get_lib()
        result = lib.proven_color_luminance(self.r, self.g, self.b)
        if result.status != ProvenStatus.OK:
            return 0.0
        return result.value

    def contrast_ratio(self, other: "Rgb") -> float:
        """
        Calculate contrast ratio with another color per WCAG 2.0 via FFI.

        Args:
            other: Color to compare against

        Returns:
            Contrast ratio (1.0 to 21.0)
        """
        lib = get_lib()
        result = lib.proven_color_contrast_ratio(
            self.r, self.g, self.b,
            other.r, other.g, other.b,
        )
        if result.status != ProvenStatus.OK:
            return 1.0
        return result.value

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
    """Safe color operations via FFI."""

    # Common colors
    BLACK = Rgb(0, 0, 0)
    WHITE = Rgb(255, 255, 255)
    RED = Rgb(255, 0, 0)
    GREEN = Rgb(0, 255, 0)
    BLUE = Rgb(0, 0, 255)
    YELLOW = Rgb(255, 255, 0)
    CYAN = Rgb(0, 255, 255)
    MAGENTA = Rgb(255, 0, 255)

    @staticmethod
    def from_hex(hex_str: str) -> Optional[Rgb]:
        """
        Parse hex color string via FFI.

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
        if not hex_str:
            return None

        lib = get_lib()
        encoded = hex_str.encode("utf-8")
        rgba_out = (ctypes.c_uint8 * 4)()
        status = lib.proven_color_parse_hex(encoded, len(encoded), rgba_out)
        if status != ProvenStatus.OK:
            return None
        try:
            return Rgb(rgba_out[0], rgba_out[1], rgba_out[2])
        except ValueError:
            return None

    @staticmethod
    def from_hex_rgba(hex_str: str) -> Optional[Rgba]:
        """
        Parse hex color string with alpha via FFI.

        Supports: #RGBA, #RRGGBBAA

        Args:
            hex_str: Hex color string

        Returns:
            Rgba color, or None if invalid
        """
        if not hex_str:
            return None

        lib = get_lib()
        encoded = hex_str.encode("utf-8")
        rgba_out = (ctypes.c_uint8 * 4)()
        status = lib.proven_color_parse_hex(encoded, len(encoded), rgba_out)
        if status != ProvenStatus.OK:
            return None
        try:
            return Rgba(rgba_out[0], rgba_out[1], rgba_out[2], rgba_out[3])
        except ValueError:
            return None

    @staticmethod
    def blend(fg: Rgba, bg: Rgb) -> Rgb:
        """
        Blend foreground RGBA over background RGB via FFI.

        Args:
            fg: Foreground color with alpha
            bg: Background color

        Returns:
            Blended RGB color
        """
        lib = get_lib()
        rgb_out = (ctypes.c_uint8 * 3)()
        status = lib.proven_color_blend(
            fg.r, fg.g, fg.b, fg.a,
            bg.r, bg.g, bg.b,
            rgb_out,
        )
        if status != ProvenStatus.OK:
            return bg
        try:
            return Rgb(rgb_out[0], rgb_out[1], rgb_out[2])
        except ValueError:
            return bg

    @staticmethod
    def interpolate(c1: Rgb, c2: Rgb, t: float) -> Rgb:
        """
        Linear interpolation between two colors via FFI.

        Args:
            c1: Start color
            c2: End color
            t: Interpolation factor (0.0-1.0)

        Returns:
            Interpolated color
        """
        lib = get_lib()
        t = max(0.0, min(1.0, t))

        # Use FFI float lerp for each channel
        r_result = lib.proven_float_lerp(float(c1.r), float(c2.r), t)
        g_result = lib.proven_float_lerp(float(c1.g), float(c2.g), t)
        b_result = lib.proven_float_lerp(float(c1.b), float(c2.b), t)

        r = int(r_result.value) if r_result.status == ProvenStatus.OK else c1.r
        g = int(g_result.value) if g_result.status == ProvenStatus.OK else c1.g
        b = int(b_result.value) if b_result.status == ProvenStatus.OK else c1.b

        return Rgb(
            max(0, min(255, r)),
            max(0, min(255, g)),
            max(0, min(255, b)),
        )

    @staticmethod
    def contrast_ratio(c1: Rgb, c2: Rgb) -> float:
        """Calculate WCAG contrast ratio between two colors via FFI."""
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
