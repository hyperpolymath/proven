// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeColor - RGB/RGBA with WCAG contrast calculations.
 *
 * Provides safe color operations with accessibility checks.
 * @module
 */

import { ok, err } from './result.js';

/**
 * RGB color (values 0-255).
 */
export class Rgb {
  /** @type {number} */
  #r;
  /** @type {number} */
  #g;
  /** @type {number} */
  #b;

  /**
   * Create an RGB color.
   *
   * @param {number} r - Red (0-255)
   * @param {number} g - Green (0-255)
   * @param {number} b - Blue (0-255)
   */
  constructor(r, g, b) {
    this.#r = Math.max(0, Math.min(255, Math.round(r)));
    this.#g = Math.max(0, Math.min(255, Math.round(g)));
    this.#b = Math.max(0, Math.min(255, Math.round(b)));
  }

  get r() {
    return this.#r;
  }
  get g() {
    return this.#g;
  }
  get b() {
    return this.#b;
  }

  /**
   * Create from validated values.
   *
   * @param {number} r - Red (0-255)
   * @param {number} g - Green (0-255)
   * @param {number} b - Blue (0-255)
   * @returns {{ ok: true, value: Rgb } | { ok: false, error: string }}
   */
  static create(r, g, b) {
    if (!Number.isFinite(r) || !Number.isFinite(g) || !Number.isFinite(b)) {
      return err('Color values must be finite numbers');
    }
    return ok(new Rgb(r, g, b));
  }

  /**
   * Parse a hex color string.
   *
   * @param {string} hex - Hex string (e.g., "#ff0000" or "ff0000")
   * @returns {{ ok: true, value: Rgb } | { ok: false, error: string }}
   */
  static fromHex(hex) {
    let str = hex.trim();
    if (str.startsWith('#')) {
      str = str.slice(1);
    }

    if (str.length === 3) {
      str = str[0] + str[0] + str[1] + str[1] + str[2] + str[2];
    }

    if (str.length !== 6 || !/^[0-9a-fA-F]+$/.test(str)) {
      return err('Invalid hex color format');
    }

    const r = parseInt(str.slice(0, 2), 16);
    const g = parseInt(str.slice(2, 4), 16);
    const b = parseInt(str.slice(4, 6), 16);

    return ok(new Rgb(r, g, b));
  }

  /**
   * Convert to hex string.
   *
   * @returns {string}
   */
  toHex() {
    const toHexStr = (value) => value.toString(16).padStart(2, '0');
    return `#${toHexStr(this.#r)}${toHexStr(this.#g)}${toHexStr(this.#b)}`;
  }

  /**
   * Convert to CSS rgb() string.
   *
   * @returns {string}
   */
  toCss() {
    return `rgb(${this.#r}, ${this.#g}, ${this.#b})`;
  }

  /**
   * Calculate relative luminance (WCAG).
   *
   * @returns {number} Luminance (0-1)
   */
  luminance() {
    const rsRgb = this.#r / 255;
    const gsRgb = this.#g / 255;
    const bsRgb = this.#b / 255;

    const r = rsRgb <= 0.03928 ? rsRgb / 12.92 : Math.pow((rsRgb + 0.055) / 1.055, 2.4);
    const g = gsRgb <= 0.03928 ? gsRgb / 12.92 : Math.pow((gsRgb + 0.055) / 1.055, 2.4);
    const b = bsRgb <= 0.03928 ? bsRgb / 12.92 : Math.pow((bsRgb + 0.055) / 1.055, 2.4);

    return 0.2126 * r + 0.7152 * g + 0.0722 * b;
  }

  /**
   * Calculate contrast ratio with another color.
   *
   * @param {Rgb} other - Other color
   * @returns {number} Contrast ratio (1-21)
   */
  contrast(other) {
    const l1 = this.luminance();
    const l2 = other.luminance();
    const lighter = Math.max(l1, l2);
    const darker = Math.min(l1, l2);
    return (lighter + 0.05) / (darker + 0.05);
  }

  /**
   * Check WCAG AA compliance for normal text.
   *
   * @param {Rgb} background - Background color
   * @returns {boolean}
   */
  meetsWcagAA(background) {
    return this.contrast(background) >= 4.5;
  }

  /**
   * Check WCAG AAA compliance for normal text.
   *
   * @param {Rgb} background - Background color
   * @returns {boolean}
   */
  meetsWcagAAA(background) {
    return this.contrast(background) >= 7;
  }

  /**
   * Check WCAG AA compliance for large text.
   *
   * @param {Rgb} background - Background color
   * @returns {boolean}
   */
  meetsWcagAALarge(background) {
    return this.contrast(background) >= 3;
  }

  /**
   * Mix with another color.
   *
   * @param {Rgb} other - Other color
   * @param {number} [amount=0.5] - Mix amount (0 = this, 1 = other)
   * @returns {Rgb}
   */
  mix(other, amount = 0.5) {
    const t = Math.max(0, Math.min(1, amount));
    return new Rgb(
      this.#r + (other.r - this.#r) * t,
      this.#g + (other.g - this.#g) * t,
      this.#b + (other.b - this.#b) * t,
    );
  }

  /**
   * Lighten color.
   *
   * @param {number} amount - Amount (0-1)
   * @returns {Rgb}
   */
  lighten(amount) {
    return this.mix(new Rgb(255, 255, 255), amount);
  }

  /**
   * Darken color.
   *
   * @param {number} amount - Amount (0-1)
   * @returns {Rgb}
   */
  darken(amount) {
    return this.mix(new Rgb(0, 0, 0), amount);
  }

  /**
   * Invert color.
   *
   * @returns {Rgb}
   */
  invert() {
    return new Rgb(255 - this.#r, 255 - this.#g, 255 - this.#b);
  }

  /**
   * Convert to grayscale.
   *
   * @returns {Rgb}
   */
  grayscale() {
    const gray = Math.round(0.299 * this.#r + 0.587 * this.#g + 0.114 * this.#b);
    return new Rgb(gray, gray, gray);
  }

  /**
   * Check equality.
   *
   * @param {Rgb} other - Other color
   * @returns {boolean}
   */
  equals(other) {
    return this.#r === other.r && this.#g === other.g && this.#b === other.b;
  }
}

/**
 * RGBA color (with alpha channel).
 */
export class Rgba extends Rgb {
  /** @type {number} */
  #a;

  /**
   * Create an RGBA color.
   *
   * @param {number} r - Red (0-255)
   * @param {number} g - Green (0-255)
   * @param {number} b - Blue (0-255)
   * @param {number} a - Alpha (0-1)
   */
  constructor(r, g, b, a) {
    super(r, g, b);
    this.#a = Math.max(0, Math.min(1, a));
  }

  get a() {
    return this.#a;
  }

  /**
   * Create from validated values.
   *
   * @param {number} r - Red (0-255)
   * @param {number} g - Green (0-255)
   * @param {number} b - Blue (0-255)
   * @param {number} a - Alpha (0-1)
   * @returns {{ ok: true, value: Rgba } | { ok: false, error: string }}
   */
  static create(r, g, b, a) {
    if (!Number.isFinite(r) || !Number.isFinite(g) || !Number.isFinite(b) || !Number.isFinite(a)) {
      return err('Color values must be finite numbers');
    }
    return ok(new Rgba(r, g, b, a));
  }

  /**
   * Parse a hex color string with optional alpha.
   *
   * @param {string} hex - Hex string (e.g., "#ff0000" or "#ff000080")
   * @returns {{ ok: true, value: Rgba } | { ok: false, error: string }}
   */
  static fromHex(hex) {
    let str = hex.trim();
    if (str.startsWith('#')) {
      str = str.slice(1);
    }

    if (str.length === 4) {
      str = str[0] + str[0] + str[1] + str[1] + str[2] + str[2] + str[3] + str[3];
    } else if (str.length === 3) {
      str = str[0] + str[0] + str[1] + str[1] + str[2] + str[2] + 'ff';
    } else if (str.length === 6) {
      str = str + 'ff';
    }

    if (str.length !== 8 || !/^[0-9a-fA-F]+$/.test(str)) {
      return err('Invalid hex color format');
    }

    const r = parseInt(str.slice(0, 2), 16);
    const g = parseInt(str.slice(2, 4), 16);
    const b = parseInt(str.slice(4, 6), 16);
    const a = parseInt(str.slice(6, 8), 16) / 255;

    return ok(new Rgba(r, g, b, a));
  }

  /**
   * Convert to CSS rgba() string.
   *
   * @returns {string}
   */
  toCss() {
    return `rgba(${this.r}, ${this.g}, ${this.b}, ${this.#a})`;
  }

  /**
   * Convert to hex string (8 characters).
   *
   * @returns {string}
   */
  toHex() {
    const toHexStr = (value) => value.toString(16).padStart(2, '0');
    const alpha = Math.round(this.#a * 255);
    return `#${toHexStr(this.r)}${toHexStr(this.g)}${toHexStr(this.b)}${toHexStr(alpha)}`;
  }

  /**
   * Convert to Rgb (drops alpha).
   *
   * @returns {Rgb}
   */
  toRgb() {
    return new Rgb(this.r, this.g, this.b);
  }

  /**
   * Blend over a background color.
   *
   * @param {Rgb} background - Background color
   * @returns {Rgb}
   */
  blendOver(background) {
    const alpha = this.#a;
    return new Rgb(
      this.r * alpha + background.r * (1 - alpha),
      this.g * alpha + background.g * (1 - alpha),
      this.b * alpha + background.b * (1 - alpha),
    );
  }
}

/**
 * Safe color utilities.
 */
export class SafeColor {
  /**
   * Create an RGB color.
   *
   * @param {number} r - Red (0-255)
   * @param {number} g - Green (0-255)
   * @param {number} b - Blue (0-255)
   * @returns {Rgb}
   */
  static rgb(r, g, b) {
    return new Rgb(r, g, b);
  }

  /**
   * Create an RGBA color.
   *
   * @param {number} r - Red (0-255)
   * @param {number} g - Green (0-255)
   * @param {number} b - Blue (0-255)
   * @param {number} a - Alpha (0-1)
   * @returns {Rgba}
   */
  static rgba(r, g, b, a) {
    return new Rgba(r, g, b, a);
  }

  /**
   * Parse any color format.
   *
   * @param {string} color - Color string
   * @returns {{ ok: true, value: Rgb | Rgba } | { ok: false, error: string }}
   */
  static parse(color) {
    const trimmed = color.trim().toLowerCase();

    // Hex format
    if (trimmed.startsWith('#')) {
      if (trimmed.length === 9 || trimmed.length === 5) {
        return Rgba.fromHex(trimmed);
      }
      return Rgb.fromHex(trimmed);
    }

    // rgb() format
    const rgbMatch = trimmed.match(/^rgb\s*\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)$/);
    if (rgbMatch) {
      return ok(new Rgb(parseInt(rgbMatch[1]), parseInt(rgbMatch[2]), parseInt(rgbMatch[3])));
    }

    // rgba() format
    const rgbaMatch = trimmed.match(/^rgba\s*\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,\s*([\d.]+)\s*\)$/);
    if (rgbaMatch) {
      return ok(
        new Rgba(parseInt(rgbaMatch[1]), parseInt(rgbaMatch[2]), parseInt(rgbaMatch[3]), parseFloat(rgbaMatch[4])),
      );
    }

    return err('Unknown color format');
  }

  // Common colors
  static BLACK = new Rgb(0, 0, 0);
  static WHITE = new Rgb(255, 255, 255);
  static RED = new Rgb(255, 0, 0);
  static GREEN = new Rgb(0, 255, 0);
  static BLUE = new Rgb(0, 0, 255);
  static YELLOW = new Rgb(255, 255, 0);
  static CYAN = new Rgb(0, 255, 255);
  static MAGENTA = new Rgb(255, 0, 255);
}
