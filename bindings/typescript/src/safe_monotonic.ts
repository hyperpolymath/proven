// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * MonotonicCounter is a counter that can only increase.
 */
export class MonotonicCounter {
  private value: number;
  private readonly max: number;

  constructor(initial: number = 0, max: number = Number.MAX_SAFE_INTEGER) {
    this.value = Math.max(0, Math.floor(initial));
    this.max = max;
  }

  /**
   * Increment by 1.
   */
  increment(): Result<number> {
    return this.incrementBy(1);
  }

  /**
   * Increment by a specific amount.
   */
  incrementBy(amount: number): Result<number> {
    if (amount < 0) {
      return { ok: false, error: 'Cannot decrement monotonic counter' };
    }

    const newValue = this.value + amount;
    if (newValue > this.max) {
      return { ok: false, error: `Would exceed maximum value ${this.max}` };
    }

    this.value = newValue;
    return { ok: true, value: this.value };
  }

  /**
   * Get current value.
   */
  get(): number {
    return this.value;
  }

  /**
   * Compare with another value.
   */
  compare(other: number): number {
    if (this.value < other) return -1;
    if (this.value > other) return 1;
    return 0;
  }

  /**
   * Check if greater than another value.
   */
  isGreaterThan(other: number): boolean {
    return this.value > other;
  }

  /**
   * Check if at maximum.
   */
  isAtMax(): boolean {
    return this.value >= this.max;
  }
}

/**
 * MonotonicTimestamp represents a timestamp that can only move forward.
 */
export class MonotonicTimestamp {
  private timestamp: number;
  private readonly getNow: () => number;

  constructor(initial?: number, getNow: () => number = Date.now) {
    this.getNow = getNow;
    this.timestamp = initial ?? this.getNow();
  }

  /**
   * Update to current time if later.
   */
  update(): number {
    const now = this.getNow();
    if (now > this.timestamp) {
      this.timestamp = now;
    }
    return this.timestamp;
  }

  /**
   * Try to set a specific timestamp (must be >= current).
   */
  trySet(newTimestamp: number): Result<number> {
    if (newTimestamp < this.timestamp) {
      return { ok: false, error: `Cannot go backwards from ${this.timestamp} to ${newTimestamp}` };
    }
    this.timestamp = newTimestamp;
    return { ok: true, value: this.timestamp };
  }

  /**
   * Get current timestamp.
   */
  get(): number {
    return this.timestamp;
  }

  /**
   * Get as Date object.
   */
  toDate(): Date {
    return new Date(this.timestamp);
  }

  /**
   * Get as ISO string.
   */
  toISOString(): string {
    return new Date(this.timestamp).toISOString();
  }

  /**
   * Get milliseconds since a reference time.
   */
  since(reference: number): number {
    return this.timestamp - reference;
  }

  /**
   * Check if this timestamp is after another.
   */
  isAfter(other: number): boolean {
    return this.timestamp > other;
  }
}

/**
 * MonotonicID generates IDs that are guaranteed to be monotonically increasing.
 */
export class MonotonicID {
  private lastTimestamp: number = 0;
  private sequence: number = 0;
  private readonly machineId: number;
  private readonly sequenceBits: number = 12;
  private readonly machineBits: number = 10;
  private readonly maxSequence: number;
  private readonly getNow: () => number;

  constructor(machineId: number = 0, getNow: () => number = Date.now) {
    this.machineId = machineId & ((1 << this.machineBits) - 1);
    this.maxSequence = (1 << this.sequenceBits) - 1;
    this.getNow = getNow;
  }

  /**
   * Generate the next ID.
   */
  next(): Result<bigint> {
    let timestamp = this.getNow();

    if (timestamp < this.lastTimestamp) {
      return { ok: false, error: 'Clock moved backwards' };
    }

    if (timestamp === this.lastTimestamp) {
      this.sequence = (this.sequence + 1) & this.maxSequence;
      if (this.sequence === 0) {
        // Wait for next millisecond
        while (timestamp <= this.lastTimestamp) {
          timestamp = this.getNow();
        }
      }
    } else {
      this.sequence = 0;
    }

    this.lastTimestamp = timestamp;

    // Format: timestamp (42 bits) | machine (10 bits) | sequence (12 bits)
    const id =
      (BigInt(timestamp) << BigInt(this.machineBits + this.sequenceBits)) |
      (BigInt(this.machineId) << BigInt(this.sequenceBits)) |
      BigInt(this.sequence);

    return { ok: true, value: id };
  }

  /**
   * Generate next ID as string.
   */
  nextString(): Result<string> {
    const result = this.next();
    if (!result.ok) {
      return { ok: false, error: result.error };
    }
    return { ok: true, value: result.value!.toString() };
  }

  /**
   * Extract timestamp from an ID.
   */
  extractTimestamp(id: bigint): number {
    return Number(id >> BigInt(this.machineBits + this.sequenceBits));
  }

  /**
   * Extract machine ID from an ID.
   */
  extractMachineId(id: bigint): number {
    return Number((id >> BigInt(this.sequenceBits)) & BigInt((1 << this.machineBits) - 1));
  }

  /**
   * Extract sequence from an ID.
   */
  extractSequence(id: bigint): number {
    return Number(id & BigInt(this.maxSequence));
  }
}

/**
 * MonotonicSequence generates sequential numbers.
 */
export class MonotonicSequence {
  private current: number;
  private readonly step: number;
  private readonly max: number;

  constructor(start: number = 0, step: number = 1, max: number = Number.MAX_SAFE_INTEGER) {
    this.current = start;
    this.step = Math.max(1, step);
    this.max = max;
  }

  /**
   * Get the next value.
   */
  next(): Result<number> {
    const value = this.current;
    const nextValue = this.current + this.step;

    if (nextValue > this.max) {
      return { ok: false, error: `Would exceed maximum value ${this.max}` };
    }

    this.current = nextValue;
    return { ok: true, value };
  }

  /**
   * Peek at the next value without consuming.
   */
  peek(): number {
    return this.current;
  }

  /**
   * Skip n values.
   */
  skip(n: number): Result<number> {
    const skipAmount = n * this.step;
    const newValue = this.current + skipAmount;

    if (newValue > this.max) {
      return { ok: false, error: `Would exceed maximum value ${this.max}` };
    }

    this.current = newValue;
    return { ok: true, value: this.current };
  }

  /**
   * Get the current value.
   */
  get(): number {
    return this.current;
  }
}

/**
 * MonotonicVersion represents a monotonically increasing version.
 */
export class MonotonicVersion {
  private major: number;
  private minor: number;
  private patch: number;

  constructor(major: number = 0, minor: number = 0, patch: number = 0) {
    this.major = Math.max(0, Math.floor(major));
    this.minor = Math.max(0, Math.floor(minor));
    this.patch = Math.max(0, Math.floor(patch));
  }

  /**
   * Parse a version string.
   */
  static parse(version: string): Result<MonotonicVersion> {
    const match = version.match(/^(\d+)\.(\d+)\.(\d+)$/);
    if (!match) {
      return { ok: false, error: 'Invalid version format' };
    }
    return {
      ok: true,
      value: new MonotonicVersion(parseInt(match[1]), parseInt(match[2]), parseInt(match[3])),
    };
  }

  /**
   * Bump major version.
   */
  bumpMajor(): MonotonicVersion {
    return new MonotonicVersion(this.major + 1, 0, 0);
  }

  /**
   * Bump minor version.
   */
  bumpMinor(): MonotonicVersion {
    return new MonotonicVersion(this.major, this.minor + 1, 0);
  }

  /**
   * Bump patch version.
   */
  bumpPatch(): MonotonicVersion {
    return new MonotonicVersion(this.major, this.minor, this.patch + 1);
  }

  /**
   * Compare with another version.
   */
  compare(other: MonotonicVersion): number {
    if (this.major !== other.major) return this.major - other.major;
    if (this.minor !== other.minor) return this.minor - other.minor;
    return this.patch - other.patch;
  }

  /**
   * Check if this version is greater than another.
   */
  isGreaterThan(other: MonotonicVersion): boolean {
    return this.compare(other) > 0;
  }

  /**
   * Convert to string.
   */
  toString(): string {
    return `${this.major}.${this.minor}.${this.patch}`;
  }

  /**
   * Convert to number array.
   */
  toArray(): [number, number, number] {
    return [this.major, this.minor, this.patch];
  }
}

export const SafeMonotonic = {
  MonotonicCounter,
  MonotonicTimestamp,
  MonotonicID,
  MonotonicSequence,
  MonotonicVersion,
};
