// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe color parsing and manipulation for Dart.
library;

/// Result of a color operation.
class ColorResult {
  final Color? color;
  final String? error;

  const ColorResult.ok(Color c)
      : color = c,
        error = null;

  const ColorResult.error(String e)
      : color = null,
        error = e;

  bool get isOk => error == null;

  Color unwrap() {
    if (error != null) {
      throw ColorException(error!);
    }
    return color!;
  }
}

/// Exception thrown on color errors.
class ColorException implements Exception {
  final String message;
  const ColorException(this.message);

  @override
  String toString() => 'ColorException: $message';
}

/// An RGBA color.
class Color {
  final int red;
  final int green;
  final int blue;
  final double alpha;

  const Color({
    required this.red,
    required this.green,
    required this.blue,
    this.alpha = 1.0,
  });

  /// Create from HSL values.
  factory Color.fromHsl(double hue, double saturation, double lightness, [double alpha = 1.0]) {
    final h = hue % 360;
    final s = saturation.clamp(0.0, 1.0);
    final l = lightness.clamp(0.0, 1.0);

    if (s == 0) {
      final v = (l * 255).round();
      return Color(red: v, green: v, blue: v, alpha: alpha);
    }

    final c = (1 - (2 * l - 1).abs()) * s;
    final x = c * (1 - ((h / 60) % 2 - 1).abs());
    final m = l - c / 2;

    double r1, g1, b1;
    if (h < 60) {
      r1 = c;
      g1 = x;
      b1 = 0;
    } else if (h < 120) {
      r1 = x;
      g1 = c;
      b1 = 0;
    } else if (h < 180) {
      r1 = 0;
      g1 = c;
      b1 = x;
    } else if (h < 240) {
      r1 = 0;
      g1 = x;
      b1 = c;
    } else if (h < 300) {
      r1 = x;
      g1 = 0;
      b1 = c;
    } else {
      r1 = c;
      g1 = 0;
      b1 = x;
    }

    return Color(
      red: ((r1 + m) * 255).round().clamp(0, 255),
      green: ((g1 + m) * 255).round().clamp(0, 255),
      blue: ((b1 + m) * 255).round().clamp(0, 255),
      alpha: alpha.clamp(0.0, 1.0),
    );
  }

  /// Convert to HSL.
  ({double hue, double saturation, double lightness}) toHsl() {
    final r = red / 255;
    final g = green / 255;
    final b = blue / 255;

    final max = [r, g, b].reduce((a, b) => a > b ? a : b);
    final min = [r, g, b].reduce((a, b) => a < b ? a : b);
    final l = (max + min) / 2;

    if (max == min) {
      return (hue: 0.0, saturation: 0.0, lightness: l);
    }

    final d = max - min;
    final s = l > 0.5 ? d / (2 - max - min) : d / (max + min);

    double h;
    if (max == r) {
      h = ((g - b) / d + (g < b ? 6 : 0)) * 60;
    } else if (max == g) {
      h = ((b - r) / d + 2) * 60;
    } else {
      h = ((r - g) / d + 4) * 60;
    }

    return (hue: h, saturation: s, lightness: l);
  }

  /// Convert to hex string.
  String toHex({bool includeAlpha = false}) {
    final r = red.toRadixString(16).padLeft(2, '0');
    final g = green.toRadixString(16).padLeft(2, '0');
    final b = blue.toRadixString(16).padLeft(2, '0');

    if (includeAlpha && alpha < 1.0) {
      final a = (alpha * 255).round().toRadixString(16).padLeft(2, '0');
      return '#$r$g$b$a';
    }
    return '#$r$g$b';
  }

  /// Convert to CSS rgb() or rgba() string.
  String toCss() {
    if (alpha < 1.0) {
      return 'rgba($red, $green, $blue, ${alpha.toStringAsFixed(2)})';
    }
    return 'rgb($red, $green, $blue)';
  }

  /// Get luminance (perceived brightness).
  double get luminance {
    // sRGB luminance
    final r = _linearize(red / 255);
    final g = _linearize(green / 255);
    final b = _linearize(blue / 255);
    return 0.2126 * r + 0.7152 * g + 0.0722 * b;
  }

  static double _linearize(double value) {
    return value <= 0.03928 ? value / 12.92 : ((value + 0.055) / 1.055).clamp(0, 1);
  }

  /// Check if color is light.
  bool get isLight => luminance > 0.5;

  /// Check if color is dark.
  bool get isDark => luminance <= 0.5;

  /// Get contrast ratio with another color.
  double contrastWith(Color other) {
    final l1 = luminance;
    final l2 = other.luminance;
    final lighter = l1 > l2 ? l1 : l2;
    final darker = l1 > l2 ? l2 : l1;
    return (lighter + 0.05) / (darker + 0.05);
  }

  /// Lighten the color.
  Color lighten(double amount) {
    final hsl = toHsl();
    return Color.fromHsl(
      hsl.hue,
      hsl.saturation,
      (hsl.lightness + amount).clamp(0.0, 1.0),
      alpha,
    );
  }

  /// Darken the color.
  Color darken(double amount) {
    final hsl = toHsl();
    return Color.fromHsl(
      hsl.hue,
      hsl.saturation,
      (hsl.lightness - amount).clamp(0.0, 1.0),
      alpha,
    );
  }

  /// Adjust saturation.
  Color saturate(double amount) {
    final hsl = toHsl();
    return Color.fromHsl(
      hsl.hue,
      (hsl.saturation + amount).clamp(0.0, 1.0),
      hsl.lightness,
      alpha,
    );
  }

  /// Desaturate.
  Color desaturate(double amount) {
    return saturate(-amount);
  }

  /// Get grayscale version.
  Color get grayscale {
    final gray = (0.299 * red + 0.587 * green + 0.114 * blue).round().clamp(0, 255);
    return Color(red: gray, green: gray, blue: gray, alpha: alpha);
  }

  /// Get inverted color.
  Color get inverted {
    return Color(
      red: 255 - red,
      green: 255 - green,
      blue: 255 - blue,
      alpha: alpha,
    );
  }

  /// Mix with another color.
  Color mix(Color other, double amount) {
    final t = amount.clamp(0.0, 1.0);
    return Color(
      red: (red + (other.red - red) * t).round().clamp(0, 255),
      green: (green + (other.green - green) * t).round().clamp(0, 255),
      blue: (blue + (other.blue - blue) * t).round().clamp(0, 255),
      alpha: alpha + (other.alpha - alpha) * t,
    );
  }

  /// With different alpha.
  Color withAlpha(double newAlpha) {
    return Color(red: red, green: green, blue: blue, alpha: newAlpha.clamp(0.0, 1.0));
  }

  @override
  String toString() => toCss();

  @override
  bool operator ==(Object other) {
    return other is Color && red == other.red && green == other.green && blue == other.blue && alpha == other.alpha;
  }

  @override
  int get hashCode => red.hashCode ^ green.hashCode ^ blue.hashCode ^ alpha.hashCode;
}

/// Safe color operations.
class SafeColor {
  /// Parse a hex color string.
  static ColorResult parseHex(String hex) {
    var input = hex.trim();

    if (input.startsWith('#')) {
      input = input.substring(1);
    }

    int r, g, b;
    double a = 1.0;

    if (input.length == 3) {
      // #RGB
      r = int.tryParse(input[0] * 2, radix: 16) ?? -1;
      g = int.tryParse(input[1] * 2, radix: 16) ?? -1;
      b = int.tryParse(input[2] * 2, radix: 16) ?? -1;
    } else if (input.length == 4) {
      // #RGBA
      r = int.tryParse(input[0] * 2, radix: 16) ?? -1;
      g = int.tryParse(input[1] * 2, radix: 16) ?? -1;
      b = int.tryParse(input[2] * 2, radix: 16) ?? -1;
      final aa = int.tryParse(input[3] * 2, radix: 16);
      if (aa != null) a = aa / 255;
    } else if (input.length == 6) {
      // #RRGGBB
      r = int.tryParse(input.substring(0, 2), radix: 16) ?? -1;
      g = int.tryParse(input.substring(2, 4), radix: 16) ?? -1;
      b = int.tryParse(input.substring(4, 6), radix: 16) ?? -1;
    } else if (input.length == 8) {
      // #RRGGBBAA
      r = int.tryParse(input.substring(0, 2), radix: 16) ?? -1;
      g = int.tryParse(input.substring(2, 4), radix: 16) ?? -1;
      b = int.tryParse(input.substring(4, 6), radix: 16) ?? -1;
      final aa = int.tryParse(input.substring(6, 8), radix: 16);
      if (aa != null) a = aa / 255;
    } else {
      return const ColorResult.error('Invalid hex color length');
    }

    if (r < 0 || g < 0 || b < 0) {
      return const ColorResult.error('Invalid hex color');
    }

    return ColorResult.ok(Color(red: r, green: g, blue: b, alpha: a));
  }

  /// Parse an RGB string.
  static ColorResult parseRgb(String rgb) {
    final match = RegExp(r'rgba?\s*\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)(?:\s*,\s*([\d.]+))?\s*\)').firstMatch(rgb);

    if (match == null) {
      return const ColorResult.error('Invalid RGB format');
    }

    final r = int.tryParse(match.group(1)!) ?? -1;
    final g = int.tryParse(match.group(2)!) ?? -1;
    final b = int.tryParse(match.group(3)!) ?? -1;
    final a = match.group(4) != null ? double.tryParse(match.group(4)!) ?? 1.0 : 1.0;

    if (r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255) {
      return const ColorResult.error('RGB values must be 0-255');
    }

    return ColorResult.ok(Color(red: r, green: g, blue: b, alpha: a.clamp(0.0, 1.0)));
  }

  /// Parse an HSL string.
  static ColorResult parseHsl(String hsl) {
    final match =
        RegExp(r'hsla?\s*\(\s*([\d.]+)\s*,\s*([\d.]+)%\s*,\s*([\d.]+)%(?:\s*,\s*([\d.]+))?\s*\)').firstMatch(hsl);

    if (match == null) {
      return const ColorResult.error('Invalid HSL format');
    }

    final h = double.tryParse(match.group(1)!) ?? -1;
    final s = (double.tryParse(match.group(2)!) ?? -1) / 100;
    final l = (double.tryParse(match.group(3)!) ?? -1) / 100;
    final a = match.group(4) != null ? double.tryParse(match.group(4)!) ?? 1.0 : 1.0;

    if (s < 0 || s > 1 || l < 0 || l > 1) {
      return const ColorResult.error('Invalid HSL values');
    }

    return ColorResult.ok(Color.fromHsl(h, s, l, a.clamp(0.0, 1.0)));
  }

  /// Parse any color string.
  static ColorResult parse(String color) {
    final input = color.trim().toLowerCase();

    // Named colors
    final named = _namedColors[input];
    if (named != null) {
      return parseHex(named);
    }

    if (input.startsWith('#')) {
      return parseHex(input);
    }
    if (input.startsWith('rgb')) {
      return parseRgb(input);
    }
    if (input.startsWith('hsl')) {
      return parseHsl(input);
    }

    return const ColorResult.error('Unknown color format');
  }

  /// Check if a color string is valid.
  static bool isValid(String color) => parse(color).isOk;

  static const _namedColors = {
    'black': '#000000',
    'white': '#ffffff',
    'red': '#ff0000',
    'green': '#008000',
    'blue': '#0000ff',
    'yellow': '#ffff00',
    'cyan': '#00ffff',
    'magenta': '#ff00ff',
    'gray': '#808080',
    'grey': '#808080',
    'silver': '#c0c0c0',
    'maroon': '#800000',
    'olive': '#808000',
    'lime': '#00ff00',
    'aqua': '#00ffff',
    'teal': '#008080',
    'navy': '#000080',
    'fuchsia': '#ff00ff',
    'purple': '#800080',
    'orange': '#ffa500',
    'pink': '#ffc0cb',
    'brown': '#a52a2a',
    'transparent': '#00000000',
  };
}
