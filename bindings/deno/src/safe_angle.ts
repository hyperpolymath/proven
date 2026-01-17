// SPDX-License-Identifier: PMPL-1.0
/**
 * Safe angle operations with unit conversions and normalization.
 */

import { err, ok, type Result } from './result.ts';

/** Angle unit type. */
export type AngleUnit = 'degrees' | 'radians' | 'gradians' | 'turns';

/** Angle representation with unit. */
export interface Angle {
  readonly value: number;
  readonly unit: AngleUnit;
}

/** Mathematical constants. */
const PI = Math.PI;
const TWO_PI = 2 * Math.PI;
const DEG_TO_RAD = PI / 180;
const RAD_TO_DEG = 180 / PI;
const GRAD_TO_RAD = PI / 200;
const RAD_TO_GRAD = 200 / PI;

/**
 * Safe angle operations.
 */
export class SafeAngle {
  /**
   * Create an angle in degrees.
   *
   * @example
   * ```ts
   * const angle = SafeAngle.degrees(90);
   * ```
   */
  static degrees(value: number): Angle {
    return { value, unit: 'degrees' };
  }

  /**
   * Create an angle in radians.
   */
  static radians(value: number): Angle {
    return { value, unit: 'radians' };
  }

  /**
   * Create an angle in gradians.
   */
  static gradians(value: number): Angle {
    return { value, unit: 'gradians' };
  }

  /**
   * Create an angle in turns.
   */
  static turns(value: number): Angle {
    return { value, unit: 'turns' };
  }

  /**
   * Convert angle to radians.
   */
  static toRadians(angle: Angle): number {
    switch (angle.unit) {
      case 'radians':
        return angle.value;
      case 'degrees':
        return angle.value * DEG_TO_RAD;
      case 'gradians':
        return angle.value * GRAD_TO_RAD;
      case 'turns':
        return angle.value * TWO_PI;
    }
  }

  /**
   * Convert angle to degrees.
   */
  static toDegrees(angle: Angle): number {
    switch (angle.unit) {
      case 'degrees':
        return angle.value;
      case 'radians':
        return angle.value * RAD_TO_DEG;
      case 'gradians':
        return angle.value * 0.9; // 360/400
      case 'turns':
        return angle.value * 360;
    }
  }

  /**
   * Convert angle to gradians.
   */
  static toGradians(angle: Angle): number {
    switch (angle.unit) {
      case 'gradians':
        return angle.value;
      case 'degrees':
        return angle.value / 0.9;
      case 'radians':
        return angle.value * RAD_TO_GRAD;
      case 'turns':
        return angle.value * 400;
    }
  }

  /**
   * Convert angle to turns.
   */
  static toTurns(angle: Angle): number {
    switch (angle.unit) {
      case 'turns':
        return angle.value;
      case 'degrees':
        return angle.value / 360;
      case 'radians':
        return angle.value / TWO_PI;
      case 'gradians':
        return angle.value / 400;
    }
  }

  /**
   * Convert angle to specified unit.
   */
  static convert(angle: Angle, toUnit: AngleUnit): Angle {
    let value: number;
    switch (toUnit) {
      case 'radians':
        value = this.toRadians(angle);
        break;
      case 'degrees':
        value = this.toDegrees(angle);
        break;
      case 'gradians':
        value = this.toGradians(angle);
        break;
      case 'turns':
        value = this.toTurns(angle);
        break;
    }
    return { value, unit: toUnit };
  }

  /**
   * Normalize angle to [0, 360) degrees or [0, 2pi) radians.
   */
  static normalize(angle: Angle): Angle {
    const radians = this.toRadians(angle);
    const normalized = ((radians % TWO_PI) + TWO_PI) % TWO_PI;
    return this.convert({ value: normalized, unit: 'radians' }, angle.unit);
  }

  /**
   * Normalize angle to [-180, 180) degrees or [-pi, pi) radians.
   */
  static normalizeSigned(angle: Angle): Angle {
    let radians = this.toRadians(angle);
    radians = ((radians % TWO_PI) + TWO_PI) % TWO_PI;
    if (radians > PI) {
      radians -= TWO_PI;
    }
    return this.convert({ value: radians, unit: 'radians' }, angle.unit);
  }

  /**
   * Add two angles.
   */
  static add(a: Angle, b: Angle): Angle {
    const sumRadians = this.toRadians(a) + this.toRadians(b);
    return this.convert({ value: sumRadians, unit: 'radians' }, a.unit);
  }

  /**
   * Subtract two angles.
   */
  static subtract(a: Angle, b: Angle): Angle {
    const diffRadians = this.toRadians(a) - this.toRadians(b);
    return this.convert({ value: diffRadians, unit: 'radians' }, a.unit);
  }

  /**
   * Multiply angle by scalar.
   */
  static multiply(angle: Angle, scalar: number): Angle {
    return { value: angle.value * scalar, unit: angle.unit };
  }

  /**
   * Divide angle by scalar.
   */
  static divide(angle: Angle, scalar: number): Result<Angle> {
    if (scalar === 0) {
      return err('Division by zero');
    }
    return ok({ value: angle.value / scalar, unit: angle.unit });
  }

  /**
   * Calculate sine of angle.
   */
  static sin(angle: Angle): number {
    return Math.sin(this.toRadians(angle));
  }

  /**
   * Calculate cosine of angle.
   */
  static cos(angle: Angle): number {
    return Math.cos(this.toRadians(angle));
  }

  /**
   * Calculate tangent of angle.
   */
  static tan(angle: Angle): Result<number> {
    const radians = this.toRadians(angle);
    const result = Math.tan(radians);
    if (!Number.isFinite(result)) {
      return err('Tangent undefined at this angle');
    }
    return ok(result);
  }

  /**
   * Calculate arc sine (inverse sine).
   */
  static asin(value: number): Result<Angle> {
    if (value < -1 || value > 1) {
      return err('Value must be between -1 and 1');
    }
    return ok({ value: Math.asin(value), unit: 'radians' });
  }

  /**
   * Calculate arc cosine (inverse cosine).
   */
  static acos(value: number): Result<Angle> {
    if (value < -1 || value > 1) {
      return err('Value must be between -1 and 1');
    }
    return ok({ value: Math.acos(value), unit: 'radians' });
  }

  /**
   * Calculate arc tangent (inverse tangent).
   */
  static atan(value: number): Angle {
    return { value: Math.atan(value), unit: 'radians' };
  }

  /**
   * Calculate two-argument arc tangent (atan2).
   */
  static atan2(y: number, x: number): Angle {
    return { value: Math.atan2(y, x), unit: 'radians' };
  }

  /**
   * Calculate the shortest angular difference between two angles.
   */
  static shortestDifference(from: Angle, to: Angle): Angle {
    let diff = this.toRadians(to) - this.toRadians(from);
    diff = ((diff % TWO_PI) + TWO_PI) % TWO_PI;
    if (diff > PI) {
      diff -= TWO_PI;
    }
    return this.convert({ value: diff, unit: 'radians' }, from.unit);
  }

  /**
   * Linearly interpolate between two angles (taking shortest path).
   */
  static lerp(from: Angle, to: Angle, t: number): Angle {
    const diff = this.shortestDifference(from, to);
    const fromRadians = this.toRadians(from);
    const result = fromRadians + this.toRadians(diff) * t;
    return this.convert({ value: result, unit: 'radians' }, from.unit);
  }

  /**
   * Check if angle is in quadrant (1, 2, 3, or 4).
   */
  static getQuadrant(angle: Angle): 1 | 2 | 3 | 4 {
    const normalized = this.normalize(angle);
    const degrees = this.toDegrees(normalized);

    if (degrees >= 0 && degrees < 90) return 1;
    if (degrees >= 90 && degrees < 180) return 2;
    if (degrees >= 180 && degrees < 270) return 3;
    return 4;
  }

  /**
   * Check if angle is between two angles (going clockwise from start to end).
   */
  static isBetween(angle: Angle, start: Angle, end: Angle): boolean {
    const a = this.toRadians(this.normalize(angle));
    const s = this.toRadians(this.normalize(start));
    const e = this.toRadians(this.normalize(end));

    if (s <= e) {
      return a >= s && a <= e;
    }
    return a >= s || a <= e;
  }

  /**
   * Parse angle from string.
   *
   * @example
   * ```ts
   * SafeAngle.parse("90deg");
   * SafeAngle.parse("1.5rad");
   * SafeAngle.parse("100grad");
   * SafeAngle.parse("0.25turn");
   * ```
   */
  static parse(s: string): Result<Angle> {
    const trimmed = s.trim().toLowerCase();

    const degMatch = trimmed.match(/^(-?[\d.]+)\s*(deg|degrees?)$/);
    if (degMatch) {
      const value = parseFloat(degMatch[1]);
      if (!Number.isFinite(value)) return err('Invalid number');
      return ok({ value, unit: 'degrees' });
    }

    const radMatch = trimmed.match(/^(-?[\d.]+)\s*(rad|radians?)$/);
    if (radMatch) {
      const value = parseFloat(radMatch[1]);
      if (!Number.isFinite(value)) return err('Invalid number');
      return ok({ value, unit: 'radians' });
    }

    const gradMatch = trimmed.match(/^(-?[\d.]+)\s*(grad|gradians?)$/);
    if (gradMatch) {
      const value = parseFloat(gradMatch[1]);
      if (!Number.isFinite(value)) return err('Invalid number');
      return ok({ value, unit: 'gradians' });
    }

    const turnMatch = trimmed.match(/^(-?[\d.]+)\s*(turn|turns?)$/);
    if (turnMatch) {
      const value = parseFloat(turnMatch[1]);
      if (!Number.isFinite(value)) return err('Invalid number');
      return ok({ value, unit: 'turns' });
    }

    // Try just a number (default to degrees)
    const numMatch = trimmed.match(/^(-?[\d.]+)$/);
    if (numMatch) {
      const value = parseFloat(numMatch[1]);
      if (!Number.isFinite(value)) return err('Invalid number');
      return ok({ value, unit: 'degrees' });
    }

    return err(`Invalid angle format: ${s}`);
  }

  /**
   * Format angle to string.
   */
  static format(angle: Angle, precision: number = 2): string {
    const value = angle.value.toFixed(precision);
    switch (angle.unit) {
      case 'degrees':
        return `${value}deg`;
      case 'radians':
        return `${value}rad`;
      case 'gradians':
        return `${value}grad`;
      case 'turns':
        return `${value}turn`;
    }
  }

  /**
   * Common angle constants.
   */
  static readonly ZERO = this.degrees(0);
  static readonly RIGHT = this.degrees(90);
  static readonly STRAIGHT = this.degrees(180);
  static readonly FULL = this.degrees(360);
  static readonly PI = this.radians(Math.PI);
  static readonly TWO_PI = this.radians(2 * Math.PI);
  static readonly HALF_PI = this.radians(Math.PI / 2);
}
