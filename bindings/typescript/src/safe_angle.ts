// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

const DEG_TO_RAD = Math.PI / 180;
const RAD_TO_DEG = 180 / Math.PI;

/**
 * Degrees represents an angle in degrees.
 */
export class Degrees {
  readonly value: number;

  constructor(value: number) {
    this.value = value;
  }

  /**
   * Create from degrees.
   */
  static of(degrees: number): Degrees {
    return new Degrees(degrees);
  }

  /**
   * Create from turns (1 turn = 360 degrees).
   */
  static fromTurns(turns: number): Degrees {
    return new Degrees(turns * 360);
  }

  /**
   * Create from gradians (1 gradian = 0.9 degrees).
   */
  static fromGradians(gradians: number): Degrees {
    return new Degrees(gradians * 0.9);
  }

  /**
   * Convert to radians.
   */
  toRadians(): Radians {
    return new Radians(this.value * DEG_TO_RAD);
  }

  /**
   * Convert to turns.
   */
  toTurns(): number {
    return this.value / 360;
  }

  /**
   * Convert to gradians.
   */
  toGradians(): number {
    return this.value / 0.9;
  }

  /**
   * Normalize to [0, 360).
   */
  normalize(): Degrees {
    return new Degrees(((this.value % 360) + 360) % 360);
  }

  /**
   * Normalize to [-180, 180).
   */
  normalizeSigned(): Degrees {
    let v = ((this.value % 360) + 360) % 360;
    if (v >= 180) v -= 360;
    return new Degrees(v);
  }

  /**
   * Add angles.
   */
  add(other: Degrees): Degrees {
    return new Degrees(this.value + other.value);
  }

  /**
   * Subtract angles.
   */
  sub(other: Degrees): Degrees {
    return new Degrees(this.value - other.value);
  }

  /**
   * Multiply by scalar.
   */
  mul(scalar: number): Degrees {
    return new Degrees(this.value * scalar);
  }

  /**
   * Divide by scalar.
   */
  div(scalar: number): Degrees {
    return new Degrees(this.value / scalar);
  }

  /**
   * Negate.
   */
  neg(): Degrees {
    return new Degrees(-this.value);
  }

  /**
   * Sine.
   */
  sin(): number {
    return Math.sin(this.value * DEG_TO_RAD);
  }

  /**
   * Cosine.
   */
  cos(): number {
    return Math.cos(this.value * DEG_TO_RAD);
  }

  /**
   * Tangent.
   */
  tan(): number {
    return Math.tan(this.value * DEG_TO_RAD);
  }

  /**
   * Check if this is a right angle (90°).
   */
  isRight(): boolean {
    return Math.abs(this.normalize().value - 90) < 1e-10;
  }

  /**
   * Check if this is a straight angle (180°).
   */
  isStraight(): boolean {
    return Math.abs(this.normalize().value - 180) < 1e-10;
  }

  /**
   * Check if this is a full angle (360°).
   */
  isFull(): boolean {
    return Math.abs(this.normalize().value) < 1e-10;
  }

  /**
   * Check if acute (0° < angle < 90°).
   */
  isAcute(): boolean {
    const n = this.normalize().value;
    return n > 0 && n < 90;
  }

  /**
   * Check if obtuse (90° < angle < 180°).
   */
  isObtuse(): boolean {
    const n = this.normalize().value;
    return n > 90 && n < 180;
  }

  /**
   * Check if reflex (180° < angle < 360°).
   */
  isReflex(): boolean {
    const n = this.normalize().value;
    return n > 180 && n < 360;
  }

  /**
   * Format as DMS (degrees-minutes-seconds).
   */
  toDMS(): string {
    const abs = Math.abs(this.value);
    const degrees = Math.floor(abs);
    const minutes = Math.floor((abs - degrees) * 60);
    const seconds = ((abs - degrees - minutes / 60) * 3600).toFixed(2);
    const sign = this.value < 0 ? '-' : '';
    return `${sign}${degrees}°${minutes}'${seconds}"`;
  }

  toString(): string {
    return `${this.value}°`;
  }
}

/**
 * Radians represents an angle in radians.
 */
export class Radians {
  readonly value: number;

  constructor(value: number) {
    this.value = value;
  }

  /**
   * Create from radians.
   */
  static of(radians: number): Radians {
    return new Radians(radians);
  }

  /**
   * Create from π multiples.
   */
  static fromPiMultiple(multiple: number): Radians {
    return new Radians(multiple * Math.PI);
  }

  /**
   * Convert to degrees.
   */
  toDegrees(): Degrees {
    return new Degrees(this.value * RAD_TO_DEG);
  }

  /**
   * Convert to turns.
   */
  toTurns(): number {
    return this.value / (2 * Math.PI);
  }

  /**
   * Normalize to [0, 2π).
   */
  normalize(): Radians {
    const twoPi = 2 * Math.PI;
    return new Radians(((this.value % twoPi) + twoPi) % twoPi);
  }

  /**
   * Normalize to [-π, π).
   */
  normalizeSigned(): Radians {
    const twoPi = 2 * Math.PI;
    let v = ((this.value % twoPi) + twoPi) % twoPi;
    if (v >= Math.PI) v -= twoPi;
    return new Radians(v);
  }

  /**
   * Add angles.
   */
  add(other: Radians): Radians {
    return new Radians(this.value + other.value);
  }

  /**
   * Subtract angles.
   */
  sub(other: Radians): Radians {
    return new Radians(this.value - other.value);
  }

  /**
   * Multiply by scalar.
   */
  mul(scalar: number): Radians {
    return new Radians(this.value * scalar);
  }

  /**
   * Divide by scalar.
   */
  div(scalar: number): Radians {
    return new Radians(this.value / scalar);
  }

  /**
   * Negate.
   */
  neg(): Radians {
    return new Radians(-this.value);
  }

  /**
   * Sine.
   */
  sin(): number {
    return Math.sin(this.value);
  }

  /**
   * Cosine.
   */
  cos(): number {
    return Math.cos(this.value);
  }

  /**
   * Tangent.
   */
  tan(): number {
    return Math.tan(this.value);
  }

  toString(): string {
    return `${this.value} rad`;
  }
}

// Common angles
export const ANGLES = {
  ZERO: Degrees.of(0),
  RIGHT: Degrees.of(90),
  STRAIGHT: Degrees.of(180),
  FULL: Degrees.of(360),
  PI: Radians.of(Math.PI),
  PI_2: Radians.of(Math.PI / 2),
  PI_4: Radians.of(Math.PI / 4),
  TWO_PI: Radians.of(2 * Math.PI),
};

/**
 * Arcsine in degrees.
 */
export function asinDeg(value: number): Degrees | undefined {
  if (value < -1 || value > 1) return undefined;
  return new Degrees(Math.asin(value) * RAD_TO_DEG);
}

/**
 * Arccosine in degrees.
 */
export function acosDeg(value: number): Degrees | undefined {
  if (value < -1 || value > 1) return undefined;
  return new Degrees(Math.acos(value) * RAD_TO_DEG);
}

/**
 * Arctangent in degrees.
 */
export function atanDeg(value: number): Degrees {
  return new Degrees(Math.atan(value) * RAD_TO_DEG);
}

/**
 * Atan2 in degrees.
 */
export function atan2Deg(y: number, x: number): Degrees {
  return new Degrees(Math.atan2(y, x) * RAD_TO_DEG);
}

export const SafeAngle = {
  Degrees,
  Radians,
  ANGLES,
  asinDeg,
  acosDeg,
  atanDeg,
  atan2Deg,
};
