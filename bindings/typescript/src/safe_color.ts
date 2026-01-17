// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * RGB represents an RGB color.
 */
export class RGB {
  readonly r: number;
  readonly g: number;
  readonly b: number;

  constructor(r: number, g: number, b: number) {
    this.r = Math.round(Math.max(0, Math.min(255, r)));
    this.g = Math.round(Math.max(0, Math.min(255, g)));
    this.b = Math.round(Math.max(0, Math.min(255, b)));
  }

  /**
   * Parse a hex color string.
   */
  static fromHex(hex: string): Result<RGB> {
    let s = hex.trim().replace(/^#/, '');

    if (s.length === 3) {
      s = s[0] + s[0] + s[1] + s[1] + s[2] + s[2];
    }

    if (s.length !== 6 || !/^[0-9a-fA-F]+$/.test(s)) {
      return { ok: false, error: 'Invalid hex color' };
    }

    const r = parseInt(s.slice(0, 2), 16);
    const g = parseInt(s.slice(2, 4), 16);
    const b = parseInt(s.slice(4, 6), 16);

    return { ok: true, value: new RGB(r, g, b) };
  }

  /**
   * Convert to hex string.
   */
  toHex(): string {
    const r = this.r.toString(16).padStart(2, '0');
    const g = this.g.toString(16).padStart(2, '0');
    const b = this.b.toString(16).padStart(2, '0');
    return `#${r}${g}${b}`;
  }

  /**
   * Convert to CSS rgb() string.
   */
  toCss(): string {
    return `rgb(${this.r}, ${this.g}, ${this.b})`;
  }

  /**
   * Convert to array.
   */
  toArray(): [number, number, number] {
    return [this.r, this.g, this.b];
  }

  /**
   * Calculate relative luminance (WCAG).
   */
  luminance(): number {
    const linearize = (c: number): number => {
      const s = c / 255;
      return s <= 0.03928 ? s / 12.92 : Math.pow((s + 0.055) / 1.055, 2.4);
    };

    const r = linearize(this.r);
    const g = linearize(this.g);
    const b = linearize(this.b);

    return 0.2126 * r + 0.7152 * g + 0.0722 * b;
  }

  /**
   * Calculate contrast ratio with another color.
   */
  contrast(other: RGB): number {
    const l1 = this.luminance();
    const l2 = other.luminance();
    const lighter = Math.max(l1, l2);
    const darker = Math.min(l1, l2);
    return (lighter + 0.05) / (darker + 0.05);
  }

  /**
   * Check WCAG AA compliance for normal text (4.5:1).
   */
  meetsWCAG_AA(background: RGB): boolean {
    return this.contrast(background) >= 4.5;
  }

  /**
   * Check WCAG AAA compliance for normal text (7:1).
   */
  meetsWCAG_AAA(background: RGB): boolean {
    return this.contrast(background) >= 7;
  }

  /**
   * Check WCAG AA compliance for large text (3:1).
   */
  meetsWCAG_AALarge(background: RGB): boolean {
    return this.contrast(background) >= 3;
  }

  /**
   * Mix with another color.
   */
  mix(other: RGB, amount: number = 0.5): RGB {
    const t = Math.max(0, Math.min(1, amount));
    return new RGB(
      this.r + (other.r - this.r) * t,
      this.g + (other.g - this.g) * t,
      this.b + (other.b - this.b) * t
    );
  }

  /**
   * Lighten the color.
   */
  lighten(amount: number): RGB {
    return this.mix(WHITE, amount);
  }

  /**
   * Darken the color.
   */
  darken(amount: number): RGB {
    return this.mix(BLACK, amount);
  }

  /**
   * Invert the color.
   */
  invert(): RGB {
    return new RGB(255 - this.r, 255 - this.g, 255 - this.b);
  }

  /**
   * Convert to grayscale.
   */
  grayscale(): RGB {
    const gray = Math.round(0.299 * this.r + 0.587 * this.g + 0.114 * this.b);
    return new RGB(gray, gray, gray);
  }

  /**
   * Adjust saturation.
   */
  saturate(amount: number): RGB {
    const gray = this.grayscale();
    return gray.mix(this, 1 + amount);
  }

  /**
   * Check if colors are equal.
   */
  equals(other: RGB): boolean {
    return this.r === other.r && this.g === other.g && this.b === other.b;
  }
}

/**
 * RGBA represents an RGBA color with alpha.
 */
export class RGBA {
  readonly r: number;
  readonly g: number;
  readonly b: number;
  readonly a: number;

  constructor(r: number, g: number, b: number, a: number = 1) {
    this.r = Math.round(Math.max(0, Math.min(255, r)));
    this.g = Math.round(Math.max(0, Math.min(255, g)));
    this.b = Math.round(Math.max(0, Math.min(255, b)));
    this.a = Math.max(0, Math.min(1, a));
  }

  /**
   * Create from RGB with alpha.
   */
  static fromRGB(rgb: RGB, alpha: number = 1): RGBA {
    return new RGBA(rgb.r, rgb.g, rgb.b, alpha);
  }

  /**
   * Convert to RGB (drops alpha).
   */
  toRGB(): RGB {
    return new RGB(this.r, this.g, this.b);
  }

  /**
   * Convert to CSS rgba() string.
   */
  toCss(): string {
    return `rgba(${this.r}, ${this.g}, ${this.b}, ${this.a.toFixed(2)})`;
  }

  /**
   * Blend over a background color.
   */
  blendOver(background: RGB): RGB {
    return new RGB(
      this.r * this.a + background.r * (1 - this.a),
      this.g * this.a + background.g * (1 - this.a),
      this.b * this.a + background.b * (1 - this.a)
    );
  }

  /**
   * Blend over another RGBA color.
   */
  blendOverRGBA(background: RGBA): RGBA {
    const outA = this.a + background.a * (1 - this.a);
    if (outA === 0) {
      return new RGBA(0, 0, 0, 0);
    }
    return new RGBA(
      (this.r * this.a + background.r * background.a * (1 - this.a)) / outA,
      (this.g * this.a + background.g * background.a * (1 - this.a)) / outA,
      (this.b * this.a + background.b * background.a * (1 - this.a)) / outA,
      outA
    );
  }
}

/**
 * HSL color representation.
 */
export class HSL {
  readonly h: number; // 0-360
  readonly s: number; // 0-100
  readonly l: number; // 0-100

  constructor(h: number, s: number, l: number) {
    this.h = ((h % 360) + 360) % 360;
    this.s = Math.max(0, Math.min(100, s));
    this.l = Math.max(0, Math.min(100, l));
  }

  /**
   * Create from RGB.
   */
  static fromRGB(rgb: RGB): HSL {
    const r = rgb.r / 255;
    const g = rgb.g / 255;
    const b = rgb.b / 255;

    const max = Math.max(r, g, b);
    const min = Math.min(r, g, b);
    const l = (max + min) / 2;

    if (max === min) {
      return new HSL(0, 0, l * 100);
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

    return new HSL(h * 360, s * 100, l * 100);
  }

  /**
   * Convert to RGB.
   */
  toRGB(): RGB {
    const h = this.h / 360;
    const s = this.s / 100;
    const l = this.l / 100;

    if (s === 0) {
      const gray = Math.round(l * 255);
      return new RGB(gray, gray, gray);
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

    return new RGB(
      hue2rgb(p, q, h + 1 / 3) * 255,
      hue2rgb(p, q, h) * 255,
      hue2rgb(p, q, h - 1 / 3) * 255
    );
  }

  /**
   * Adjust hue.
   */
  rotateHue(degrees: number): HSL {
    return new HSL(this.h + degrees, this.s, this.l);
  }

  /**
   * Get complementary color.
   */
  complement(): HSL {
    return this.rotateHue(180);
  }

  /**
   * Convert to CSS hsl() string.
   */
  toCss(): string {
    return `hsl(${this.h.toFixed(0)}, ${this.s.toFixed(0)}%, ${this.l.toFixed(0)}%)`;
  }
}

// Common colors
export const BLACK = new RGB(0, 0, 0);
export const WHITE = new RGB(255, 255, 255);
export const RED = new RGB(255, 0, 0);
export const GREEN = new RGB(0, 255, 0);
export const BLUE = new RGB(0, 0, 255);
export const YELLOW = new RGB(255, 255, 0);
export const CYAN = new RGB(0, 255, 255);
export const MAGENTA = new RGB(255, 0, 255);

export const SafeColor = {
  RGB,
  RGBA,
  HSL,
  BLACK,
  WHITE,
  RED,
  GREEN,
  BLUE,
  YELLOW,
  CYAN,
  MAGENTA,
};
