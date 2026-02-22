// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe color parsing and manipulation via libproven FFI.
///
/// All computation is performed in the formally verified Idris 2 core.
library;

import 'dart:ffi';

import 'package:ffi/ffi.dart';

import 'ffi.dart';

/// Parsed RGB color.
class RgbColorValue {
  final int red;
  final int green;
  final int blue;

  const RgbColorValue(this.red, this.green, this.blue);

  @override
  String toString() => 'rgb($red, $green, $blue)';

  /// Convert to hex string via FFI.
  String? toHex() {
    final rgb = calloc<RGBColor>();
    try {
      rgb.ref.r = red;
      rgb.ref.g = green;
      rgb.ref.b = blue;
      final result = provenColorToHex(rgb.ref);
      return callStringFfi(result);
    } finally {
      calloc.free(rgb);
    }
  }
}

/// Parsed HSL color.
class HslColorValue {
  final double hue;
  final double saturation;
  final double lightness;

  const HslColorValue(this.hue, this.saturation, this.lightness);

  @override
  String toString() =>
      'hsl(${hue.toStringAsFixed(1)}, ${(saturation * 100).toStringAsFixed(1)}%, ${(lightness * 100).toStringAsFixed(1)}%)';
}

/// Safe color operations.
///
/// Delegates to libproven for color parsing and conversion.
class SafeColor {
  /// Parse a hex color string (e.g., "#ff0000" or "ff0000").
  /// Returns null on invalid input.
  static RgbColorValue? parseHex(String hex) {
    final (ptr, len) = toNativeUtf8Bytes(hex);
    try {
      final result = provenColorParseHex(ptr, len);
      if (result.status != ProvenStatus.ok) return null;
      return RgbColorValue(result.color.r, result.color.g, result.color.b);
    } finally {
      calloc.free(ptr);
    }
  }

  /// Convert RGB to HSL.
  /// Returns null on FFI error.
  static HslColorValue? rgbToHsl(int r, int g, int b) {
    final rgb = calloc<RGBColor>();
    try {
      rgb.ref.r = r;
      rgb.ref.g = g;
      rgb.ref.b = b;
      final hsl = provenColorRgbToHsl(rgb.ref);
      return HslColorValue(hsl.h, hsl.s, hsl.l);
    } finally {
      calloc.free(rgb);
    }
  }

  /// Convert RGB to hex string.
  /// Returns null on FFI error.
  static String? rgbToHex(int r, int g, int b) {
    final rgb = calloc<RGBColor>();
    try {
      rgb.ref.r = r;
      rgb.ref.g = g;
      rgb.ref.b = b;
      final result = provenColorToHex(rgb.ref);
      return callStringFfi(result);
    } finally {
      calloc.free(rgb);
    }
  }
}
