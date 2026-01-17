// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeMonotonic - Monotonically increasing values.
 *
 * Provides counters and sequences that only increase.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Monotonically increasing counter.
 */
export class MonotonicCounter {
  /** @type {bigint} */
  #value;
  /** @type {bigint} */
  #initial;
  /** @type {bigint | undefined} */
  #max;

  /**
   * Create a monotonic counter.
   *
   * @param {Object} options - Options
   * @param {bigint | number} [options.initial=0n] - Initial value
   * @param {bigint | number} [options.max] - Maximum value (optional)
   */
  constructor(options = {}) {
    const { initial = 0n, max } = options;

    this.#initial = BigInt(initial);
    this.#value = this.#initial;
    this.#max = max !== undefined ? BigInt(max) : undefined;

    if (this.#max !== undefined && this.#initial > this.#max) {
      throw new Error('Initial value cannot exceed maximum');
    }
  }

  /**
   * Get current value.
   *
   * @returns {bigint}
   */
  get value() {
    return this.#value;
  }

  /**
   * Get current value as number (may lose precision).
   *
   * @returns {number}
   */
  get valueAsNumber() {
    return Number(this.#value);
  }

  /**
   * Get initial value.
   *
   * @returns {bigint}
   */
  get initial() {
    return this.#initial;
  }

  /**
   * Get maximum value.
   *
   * @returns {bigint | undefined}
   */
  get max() {
    return this.#max;
  }

  /**
   * Increment and return new value.
   *
   * @param {bigint | number} [amount=1n] - Amount to increment
   * @returns {{ ok: true, value: bigint } | { ok: false, error: string }}
   */
  increment(amount = 1n) {
    const inc = BigInt(amount);

    if (inc < 0n) {
      return err('Cannot decrement a monotonic counter');
    }

    const newValue = this.#value + inc;

    if (this.#max !== undefined && newValue > this.#max) {
      return err('Counter would exceed maximum');
    }

    this.#value = newValue;
    return ok(this.#value);
  }

  /**
   * Increment and return previous value.
   *
   * @param {bigint | number} [amount=1n] - Amount to increment
   * @returns {{ ok: true, value: bigint } | { ok: false, error: string }}
   */
  fetchIncrement(amount = 1n) {
    const previous = this.#value;
    const result = this.increment(amount);

    if (!result.ok) {
      return result;
    }

    return ok(previous);
  }

  /**
   * Try to set a new value (must be >= current).
   *
   * @param {bigint | number} newValue - New value
   * @returns {{ ok: true, value: bigint } | { ok: false, error: string }}
   */
  trySet(newValue) {
    const val = BigInt(newValue);

    if (val < this.#value) {
      return err('Cannot decrease monotonic counter');
    }

    if (this.#max !== undefined && val > this.#max) {
      return err('Value would exceed maximum');
    }

    this.#value = val;
    return ok(this.#value);
  }

  /**
   * Check if counter has reached maximum.
   *
   * @returns {boolean}
   */
  isAtMax() {
    return this.#max !== undefined && this.#value >= this.#max;
  }

  /**
   * Get remaining capacity.
   *
   * @returns {bigint | undefined}
   */
  remaining() {
    if (this.#max === undefined) {
      return undefined;
    }
    return this.#max - this.#value;
  }

  /**
   * Reset to initial value.
   */
  reset() {
    this.#value = this.#initial;
  }
}

/**
 * Monotonically increasing timestamp.
 */
export class MonotonicTimestamp {
  /** @type {bigint} */
  #lastTimestamp;
  /** @type {bigint} */
  #counter;

  /**
   * Create a monotonic timestamp generator.
   */
  constructor() {
    this.#lastTimestamp = 0n;
    this.#counter = 0n;
  }

  /**
   * Get next timestamp (always increases).
   *
   * @returns {bigint}
   */
  next() {
    const now = BigInt(Date.now());

    if (now > this.#lastTimestamp) {
      this.#lastTimestamp = now;
      this.#counter = 0n;
    } else {
      this.#counter++;
    }

    // Combine timestamp and counter for uniqueness
    return this.#lastTimestamp * 1000000n + this.#counter;
  }

  /**
   * Get next timestamp as number (may lose precision).
   *
   * @returns {number}
   */
  nextAsNumber() {
    return Number(this.next());
  }

  /**
   * Get current last timestamp.
   *
   * @returns {bigint}
   */
  get lastTimestamp() {
    return this.#lastTimestamp;
  }

  /**
   * Get current counter value.
   *
   * @returns {bigint}
   */
  get counter() {
    return this.#counter;
  }
}

/**
 * Monotonic ID generator.
 */
export class MonotonicId {
  /** @type {string} */
  #prefix;
  /** @type {MonotonicCounter} */
  #counter;

  /**
   * Create a monotonic ID generator.
   *
   * @param {Object} options - Options
   * @param {string} [options.prefix=''] - ID prefix
   * @param {bigint | number} [options.start=1n] - Starting value
   */
  constructor(options = {}) {
    const { prefix = '', start = 1n } = options;

    this.#prefix = prefix;
    this.#counter = new MonotonicCounter({ initial: BigInt(start) });
  }

  /**
   * Generate next ID.
   *
   * @returns {string}
   */
  next() {
    const value = this.#counter.value;
    this.#counter.increment();
    return `${this.#prefix}${value}`;
  }

  /**
   * Peek at next ID without consuming.
   *
   * @returns {string}
   */
  peek() {
    return `${this.#prefix}${this.#counter.value}`;
  }

  /**
   * Get current counter value.
   *
   * @returns {bigint}
   */
  get current() {
    return this.#counter.value;
  }

  /**
   * Get prefix.
   *
   * @returns {string}
   */
  get prefix() {
    return this.#prefix;
  }
}

/**
 * Monotonic sequence (list that only grows).
 *
 * @template T
 */
export class MonotonicSequence {
  /** @type {T[]} */
  #items;
  /** @type {number | undefined} */
  #maxLength;

  /**
   * Create a monotonic sequence.
   *
   * @param {Object} options - Options
   * @param {number} [options.maxLength] - Maximum length (optional)
   */
  constructor(options = {}) {
    const { maxLength } = options;

    this.#items = [];
    this.#maxLength = maxLength;
  }

  /**
   * Get current length.
   *
   * @returns {number}
   */
  get length() {
    return this.#items.length;
  }

  /**
   * Get maximum length.
   *
   * @returns {number | undefined}
   */
  get maxLength() {
    return this.#maxLength;
  }

  /**
   * Append an item.
   *
   * @param {T} item - Item to append
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  append(item) {
    if (this.#maxLength !== undefined && this.#items.length >= this.#maxLength) {
      return err('Sequence is at maximum length');
    }

    this.#items.push(item);
    return ok(this.#items.length);
  }

  /**
   * Get item at index.
   *
   * @param {number} index - Index
   * @returns {T | undefined}
   */
  get(index) {
    if (index < 0 || index >= this.#items.length) {
      return undefined;
    }
    return this.#items[index];
  }

  /**
   * Get last item.
   *
   * @returns {T | undefined}
   */
  last() {
    return this.#items[this.#items.length - 1];
  }

  /**
   * Get all items.
   *
   * @returns {T[]}
   */
  toArray() {
    return [...this.#items];
  }

  /**
   * Iterate over items.
   *
   * @returns {IterableIterator<T>}
   */
  *[Symbol.iterator]() {
    yield* this.#items;
  }

  /**
   * Check if empty.
   *
   * @returns {boolean}
   */
  isEmpty() {
    return this.#items.length === 0;
  }

  /**
   * Check if at max length.
   *
   * @returns {boolean}
   */
  isFull() {
    return this.#maxLength !== undefined && this.#items.length >= this.#maxLength;
  }
}

/**
 * Safe monotonic utilities.
 */
export class SafeMonotonic {
  /**
   * Create a monotonic counter.
   *
   * @param {bigint | number} [initial=0n] - Initial value
   * @returns {MonotonicCounter}
   */
  static counter(initial = 0n) {
    return new MonotonicCounter({ initial: BigInt(initial) });
  }

  /**
   * Create a bounded monotonic counter.
   *
   * @param {bigint | number} max - Maximum value
   * @param {bigint | number} [initial=0n] - Initial value
   * @returns {MonotonicCounter}
   */
  static boundedCounter(max, initial = 0n) {
    return new MonotonicCounter({
      initial: BigInt(initial),
      max: BigInt(max),
    });
  }

  /**
   * Create a monotonic timestamp generator.
   *
   * @returns {MonotonicTimestamp}
   */
  static timestamp() {
    return new MonotonicTimestamp();
  }

  /**
   * Create a monotonic ID generator.
   *
   * @param {string} [prefix=''] - ID prefix
   * @returns {MonotonicId}
   */
  static id(prefix = '') {
    return new MonotonicId({ prefix });
  }

  /**
   * Create a monotonic sequence.
   *
   * @template T
   * @param {number} [maxLength] - Maximum length
   * @returns {MonotonicSequence<T>}
   */
  static sequence(maxLength) {
    return new MonotonicSequence({ maxLength });
  }
}
