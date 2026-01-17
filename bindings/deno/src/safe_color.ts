// SPDX-License-Identifier: PMPL-1.0
/**
 * Safe color parsing, conversion, and manipulation.
 */

import { err, ok, type Result } from './result.ts';

/** RGB color with values 0-255. */
export interface Rgb {
  readonly r: number;
  readonly g: number;
  readonly b: number;
}

/** RGBA color with alpha 0-1. */
export interface Rgba extends Rgb {
  readonly a: number;
}

/** HSL color with h 0-360, s/l 0-100. */
export interface Hsl {
  readonly h: number;
  readonly s: number;
  readonly l: number;
}

/** HSLA color with alpha 0-1. */
export interface Hsla extends Hsl {
  readonly a: number;
}

/** HSV/HSB color with h 0-360, s/v 0-100. */
export interface Hsv {
  readonly h: number;
  readonly s: number;
  readonly v: number;
}

/** Color format type. */
export type ColorFormat = 'hex' | 'rgb' | 'rgba' | 'hsl' | 'hsla' | 'named';

/** Parsed color result. */
export interface ParsedColor {
  readonly format: ColorFormat;
  readonly rgba: Rgba;
}

/** Named CSS colors (subset). */
const NAMED_COLORS: Record<string, string> = {
  black: '#000000',
  white: '#ffffff',
  red: '#ff0000',
  green: '#008000',
  blue: '#0000ff',
  yellow: '#ffff00',
  cyan: '#00ffff',
  magenta: '#ff00ff',
  silver: '#c0c0c0',
  gray: '#808080',
  grey: '#808080',
  maroon: '#800000',
  olive: '#808000',
  lime: '#00ff00',
  aqua: '#00ffff',
  teal: '#008080',
  navy: '#000080',
  fuchsia: '#ff00ff',
  purple: '#800080',
  orange: '#ffa500',
  pink: '#ffc0cb',
  brown: '#a52a2a',
  coral: '#ff7f50',
  gold: '#ffd700',
  indigo: '#4b0082',
  ivory: '#fffff0',
  khaki: '#f0e68c',
  lavender: '#e6e6fa',
  salmon: '#fa8072',
  transparent: '#00000000',
};

/**
 * Safe color operations.
 */
export class SafeColor {
  /**
   * Parse any color string format.
   *
   * @example
   * ```ts
   * const result = SafeColor.parse("#ff5500");
   * const result2 = SafeColor.parse("rgb(255, 85, 0)");
   * const result3 = SafeColor.parse("hsl(20, 100%, 50%)");
   * ```
   */
  static parse(color: string): Result<ParsedColor> {
    const trimmed = color.trim().toLowerCase();

    // Try named colors
    if (NAMED_COLORS[trimmed]) {
      const hexResult = this.parseHex(NAMED_COLORS[trimmed]);
      if (hexResult.ok) {
        return ok({ format: 'named', rgba: hexResult.value });
      }
    }

    // Try hex
    if (trimmed.startsWith('#')) {
      const result = this.parseHex(trimmed);
      if (result.ok) {
        return ok({ format: 'hex', rgba: result.value });
      }
      return result;
    }

    // Try rgb/rgba
    if (trimmed.startsWith('rgb')) {
      const result = this.parseRgbString(trimmed);
      if (result.ok) {
        return ok({
          format: trimmed.startsWith('rgba') ? 'rgba' : 'rgb',
          rgba: result.value,
        });
      }
      return result;
    }

    // Try hsl/hsla
    if (trimmed.startsWith('hsl')) {
      const result = this.parseHslString(trimmed);
      if (result.ok) {
        return ok({
          format: trimmed.startsWith('hsla') ? 'hsla' : 'hsl',
          rgba: result.value,
        });
      }
      return result;
    }

    return err(`Unknown color format: ${color}`);
  }

  /**
   * Parse hex color string.
   */
  static parseHex(hex: string): Result<Rgba> {
    let h = hex.replace('#', '');

    if (h.length === 3) {
      h = h[0] + h[0] + h[1] + h[1] + h[2] + h[2] + 'ff';
    } else if (h.length === 4) {
      h = h[0] + h[0] + h[1] + h[1] + h[2] + h[2] + h[3] + h[3];
    } else if (h.length === 6) {
      h = h + 'ff';
    } else if (h.length !== 8) {
      return err('Invalid hex color length');
    }

    if (!/^[0-9a-f]+$/i.test(h)) {
      return err('Invalid hex characters');
    }

    return ok({
      r: parseInt(h.slice(0, 2), 16),
      g: parseInt(h.slice(2, 4), 16),
      b: parseInt(h.slice(4, 6), 16),
      a: parseInt(h.slice(6, 8), 16) / 255,
    });
  }

  /**
   * Parse rgb/rgba string.
   */
  static parseRgbString(rgb: string): Result<Rgba> {
    const match = rgb.match(
      /rgba?\s*\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*(?:,\s*([\d.]+))?\s*\)/,
    );
    if (!match) {
      return err('Invalid rgb format');
    }

    const r = parseInt(match[1], 10);
    const g = parseInt(match[2], 10);
    const b = parseInt(match[3], 10);
    const a = match[4] !== undefined ? parseFloat(match[4]) : 1;

    if (r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255) {
      return err('RGB values must be 0-255');
    }
    if (a < 0 || a > 1) {
      return err('Alpha must be 0-1');
    }

    return ok({ r, g, b, a });
  }

  /**
   * Parse hsl/hsla string.
   */
  static parseHslString(hsl: string): Result<Rgba> {
    const match = hsl.match(
      /hsla?\s*\(\s*([\d.]+)\s*,\s*([\d.]+)%\s*,\s*([\d.]+)%\s*(?:,\s*([\d.]+))?\s*\)/,
    );
    if (!match) {
      return err('Invalid hsl format');
    }

    const h = parseFloat(match[1]);
    const s = parseFloat(match[2]);
    const l = parseFloat(match[3]);
    const a = match[4] !== undefined ? parseFloat(match[4]) : 1;

    if (h < 0 || h > 360) {
      return err('Hue must be 0-360');
    }
    if (s < 0 || s > 100 || l < 0 || l > 100) {
      return err('Saturation and lightness must be 0-100');
    }
    if (a < 0 || a > 1) {
      return err('Alpha must be 0-1');
    }

    return ok({ ...this.hslToRgb({ h, s, l }), a });
  }

  /**
   * Convert RGB to HSL.
   */
  static rgbToHsl(rgb: Rgb): Hsl {
    const r = rgb.r / 255;
    const g = rgb.g / 255;
    const b = rgb.b / 255;

    const max = Math.max(r, g, b);
    const min = Math.min(r, g, b);
    const l = (max + min) / 2;

    if (max === min) {
      return { h: 0, s: 0, l: l * 100 };
    }

    const d = max - min;
    const s = l > 0.5 ? d / (2 - max - min) : d / (max + min);

    let h: number;
    switch (max) {
      case r:
        h = ((g - b) / d + (g < b ? 6 : 0)) / 6;
        break;
      case g:
        h = ((b - r) / d + 2) / 6;
        break;
      default:
        h = ((r - g) / d + 4) / 6;
    }

    return { h: h * 360, s: s * 100, l: l * 100 };
  }

  /**
   * Convert HSL to RGB.
   */
  static hslToRgb(hsl: Hsl): Rgb {
    const h = hsl.h / 360;
    const s = hsl.s / 100;
    const l = hsl.l / 100;

    if (s === 0) {
      const v = Math.round(l * 255);
      return { r: v, g: v, b: v };
    }

    const hue2rgb = (p: number, q: number, t: number): number => {
      if (t < 0) t += 1;
      if (t > 1) t -= 1;
      if (t < 1 / 6) return p + (q - p) * 6 * t;
      if (t < 1 / 2) return q;
      if (t < 2 / 3) return p + (q - p) * (2 / 3 - t) * 6;
      return p;
    };

    const q = l < 0.5 ? l * (1 + s) : l + s - l * s;
    const p = 2 * l - q;

    return {
      r: Math.round(hue2rgb(p, q, h + 1 / 3) * 255),
      g: Math.round(hue2rgb(p, q, h) * 255),
      b: Math.round(hue2rgb(p, q, h - 1 / 3) * 255),
    };
  }

  /**
   * Convert RGB to HSV.
   */
  static rgbToHsv(rgb: Rgb): Hsv {
    const r = rgb.r / 255;
    const g = rgb.g / 255;
    const b = rgb.b / 255;

    const max = Math.max(r, g, b);
    const min = Math.min(r, g, b);
    const d = max - min;

    let h: number;
    if (max === min) {
      h = 0;
    } else if (max === r) {
      h = ((g - b) / d + (g < b ? 6 : 0)) / 6;
    } else if (max === g) {
      h = ((b - r) / d + 2) / 6;
    } else {
      h = ((r - g) / d + 4) / 6;
    }

    const s = max === 0 ? 0 : d / max;

    return { h: h * 360, s: s * 100, v: max * 100 };
  }

  /**
   * Convert HSV to RGB.
   */
  static hsvToRgb(hsv: Hsv): Rgb {
    const h = hsv.h / 360;
    const s = hsv.s / 100;
    const v = hsv.v / 100;

    const i = Math.floor(h * 6);
    const f = h * 6 - i;
    const p = v * (1 - s);
    const q = v * (1 - f * s);
    const t = v * (1 - (1 - f) * s);

    let r: number, g: number, b: number;
    switch (i % 6) {
      case 0:
        r = v; g = t; b = p;
        break;
      case 1:
        r = q; g = v; b = p;
        break;
      case 2:
        r = p; g = v; b = t;
        break;
      case 3:
        r = p; g = q; b = v;
        break;
      case 4:
        r = t; g = p; b = v;
        break;
      default:
        r = v; g = p; b = q;
    }

    return {
      r: Math.round(r * 255),
      g: Math.round(g * 255),
      b: Math.round(b * 255),
    };
  }

  /**
   * Format RGBA to hex string.
   */
  static toHex(rgba: Rgba, includeAlpha: boolean = false): string {
    const r = Math.round(rgba.r).toString(16).padStart(2, '0');
    const g = Math.round(rgba.g).toString(16).padStart(2, '0');
    const b = Math.round(rgba.b).toString(16).padStart(2, '0');

    if (includeAlpha) {
      const a = Math.round(rgba.a * 255)
        .toString(16)
        .padStart(2, '0');
      return `#${r}${g}${b}${a}`;
    }

    return `#${r}${g}${b}`;
  }

  /**
   * Format RGBA to rgb/rgba string.
   */
  static toRgbString(rgba: Rgba): string {
    if (rgba.a < 1) {
      return `rgba(${Math.round(rgba.r)}, ${Math.round(rgba.g)}, ${Math.round(rgba.b)}, ${rgba.a})`;
    }
    return `rgb(${Math.round(rgba.r)}, ${Math.round(rgba.g)}, ${Math.round(rgba.b)})`;
  }

  /**
   * Format HSL to hsl/hsla string.
   */
  static toHslString(hsl: Hsl | Hsla): string {
    const a = 'a' in hsl ? hsl.a : 1;
    if (a < 1) {
      return `hsla(${Math.round(hsl.h)}, ${Math.round(hsl.s)}%, ${Math.round(hsl.l)}%, ${a})`;
    }
    return `hsl(${Math.round(hsl.h)}, ${Math.round(hsl.s)}%, ${Math.round(hsl.l)}%)`;
  }

  /**
   * Lighten a color by percentage.
   */
  static lighten(color: Rgba, percent: number): Rgba {
    const hsl = this.rgbToHsl(color);
    const newL = Math.min(100, hsl.l + percent);
    const rgb = this.hslToRgb({ ...hsl, l: newL });
    return { ...rgb, a: color.a };
  }

  /**
   * Darken a color by percentage.
   */
  static darken(color: Rgba, percent: number): Rgba {
    const hsl = this.rgbToHsl(color);
    const newL = Math.max(0, hsl.l - percent);
    const rgb = this.hslToRgb({ ...hsl, l: newL });
    return { ...rgb, a: color.a };
  }

  /**
   * Saturate a color by percentage.
   */
  static saturate(color: Rgba, percent: number): Rgba {
    const hsl = this.rgbToHsl(color);
    const newS = Math.min(100, hsl.s + percent);
    const rgb = this.hslToRgb({ ...hsl, s: newS });
    return { ...rgb, a: color.a };
  }

  /**
   * Desaturate a color by percentage.
   */
  static desaturate(color: Rgba, percent: number): Rgba {
    const hsl = this.rgbToHsl(color);
    const newS = Math.max(0, hsl.s - percent);
    const rgb = this.hslToRgb({ ...hsl, s: newS });
    return { ...rgb, a: color.a };
  }

  /**
   * Get the complementary color.
   */
  static complement(color: Rgba): Rgba {
    const hsl = this.rgbToHsl(color);
    const newH = (hsl.h + 180) % 360;
    const rgb = this.hslToRgb({ ...hsl, h: newH });
    return { ...rgb, a: color.a };
  }

  /**
   * Mix two colors.
   */
  static mix(color1: Rgba, color2: Rgba, weight: number = 0.5): Rgba {
    const w = Math.max(0, Math.min(1, weight));
    return {
      r: Math.round(color1.r * (1 - w) + color2.r * w),
      g: Math.round(color1.g * (1 - w) + color2.g * w),
      b: Math.round(color1.b * (1 - w) + color2.b * w),
      a: color1.a * (1 - w) + color2.a * w,
    };
  }

  /**
   * Calculate contrast ratio between two colors (WCAG).
   */
  static contrastRatio(color1: Rgba, color2: Rgba): number {
    const luminance1 = this.relativeLuminance(color1);
    const luminance2 = this.relativeLuminance(color2);
    const lighter = Math.max(luminance1, luminance2);
    const darker = Math.min(luminance1, luminance2);
    return (lighter + 0.05) / (darker + 0.05);
  }

  /**
   * Calculate relative luminance (WCAG).
   */
  static relativeLuminance(color: Rgb): number {
    const toLinear = (c: number): number => {
      const s = c / 255;
      return s <= 0.03928 ? s / 12.92 : Math.pow((s + 0.055) / 1.055, 2.4);
    };

    return 0.2126 * toLinear(color.r) + 0.7152 * toLinear(color.g) + 0.0722 * toLinear(color.b);
  }

  /**
   * Check if color is dark (for choosing text color).
   */
  static isDark(color: Rgb): boolean {
    return this.relativeLuminance(color) < 0.179;
  }

  /**
   * Check WCAG AA compliance for normal text.
   */
  static meetsWcagAA(foreground: Rgba, background: Rgba): boolean {
    return this.contrastRatio(foreground, background) >= 4.5;
  }

  /**
   * Check WCAG AAA compliance for normal text.
   */
  static meetsWcagAAA(foreground: Rgba, background: Rgba): boolean {
    return this.contrastRatio(foreground, background) >= 7;
  }
}
