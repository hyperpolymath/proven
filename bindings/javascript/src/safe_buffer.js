// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeBuffer - Bounded buffers and ring buffers.
 *
 * Provides safe buffer operations with capacity limits.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Bounded buffer with fixed capacity.
 *
 * @template T
 */
export class BoundedBuffer {
  /** @type {T[]} */
  #data;
  /** @type {number} */
  #capacity;

  /**
   * Create a bounded buffer.
   *
   * @param {number} capacity - Maximum capacity
   */
  constructor(capacity) {
    if (capacity < 1) {
      throw new Error('Capacity must be at least 1');
    }
    this.#capacity = capacity;
    this.#data = [];
  }

  /**
   * Get current length.
   *
   * @returns {number}
   */
  get length() {
    return this.#data.length;
  }

  /**
   * Get capacity.
   *
   * @returns {number}
   */
  get capacity() {
    return this.#capacity;
  }

  /**
   * Check if buffer is full.
   *
   * @returns {boolean}
   */
  isFull() {
    return this.#data.length >= this.#capacity;
  }

  /**
   * Check if buffer is empty.
   *
   * @returns {boolean}
   */
  isEmpty() {
    return this.#data.length === 0;
  }

  /**
   * Push an item (fails if full).
   *
   * @param {T} item - Item to push
   * @returns {boolean} True if successful
   */
  push(item) {
    if (this.isFull()) {
      return false;
    }
    this.#data.push(item);
    return true;
  }

  /**
   * Pop an item (fails if empty).
   *
   * @returns {T | undefined}
   */
  pop() {
    return this.#data.pop();
  }

  /**
   * Get item at index.
   *
   * @param {number} index - Index
   * @returns {T | undefined}
   */
  get(index) {
    if (index < 0 || index >= this.#data.length) {
      return undefined;
    }
    return this.#data[index];
  }

  /**
   * Peek at last item without removing.
   *
   * @returns {T | undefined}
   */
  peek() {
    return this.#data[this.#data.length - 1];
  }

  /**
   * Clear the buffer.
   */
  clear() {
    this.#data = [];
  }

  /**
   * Get all items as array.
   *
   * @returns {T[]}
   */
  toArray() {
    return [...this.#data];
  }

  /**
   * Get remaining capacity.
   *
   * @returns {number}
   */
  remaining() {
    return this.#capacity - this.#data.length;
  }
}

/**
 * Ring buffer (circular buffer) with fixed capacity.
 *
 * @template T
 */
export class RingBuffer {
  /** @type {(T | undefined)[]} */
  #data;
  /** @type {number} */
  #capacity;
  /** @type {number} */
  #head;
  /** @type {number} */
  #tail;
  /** @type {number} */
  #length;

  /**
   * Create a ring buffer.
   *
   * @param {number} capacity - Maximum capacity
   */
  constructor(capacity) {
    if (capacity < 1) {
      throw new Error('Capacity must be at least 1');
    }
    this.#capacity = capacity;
    this.#data = new Array(capacity);
    this.#head = 0;
    this.#tail = 0;
    this.#length = 0;
  }

  /**
   * Get current length.
   *
   * @returns {number}
   */
  get length() {
    return this.#length;
  }

  /**
   * Get capacity.
   *
   * @returns {number}
   */
  get capacity() {
    return this.#capacity;
  }

  /**
   * Check if buffer is full.
   *
   * @returns {boolean}
   */
  isFull() {
    return this.#length >= this.#capacity;
  }

  /**
   * Check if buffer is empty.
   *
   * @returns {boolean}
   */
  isEmpty() {
    return this.#length === 0;
  }

  /**
   * Push an item (overwrites oldest if full).
   *
   * @param {T} item - Item to push
   * @returns {T | undefined} Overwritten item, or undefined
   */
  push(item) {
    let overwritten;

    if (this.isFull()) {
      overwritten = this.#data[this.#head];
      this.#head = (this.#head + 1) % this.#capacity;
    } else {
      this.#length++;
    }

    this.#data[this.#tail] = item;
    this.#tail = (this.#tail + 1) % this.#capacity;

    return overwritten;
  }

  /**
   * Pop oldest item (FIFO).
   *
   * @returns {T | undefined}
   */
  pop() {
    if (this.isEmpty()) {
      return undefined;
    }

    const item = this.#data[this.#head];
    this.#data[this.#head] = undefined;
    this.#head = (this.#head + 1) % this.#capacity;
    this.#length--;

    return item;
  }

  /**
   * Peek at oldest item without removing.
   *
   * @returns {T | undefined}
   */
  peek() {
    if (this.isEmpty()) {
      return undefined;
    }
    return this.#data[this.#head];
  }

  /**
   * Peek at newest item without removing.
   *
   * @returns {T | undefined}
   */
  peekNewest() {
    if (this.isEmpty()) {
      return undefined;
    }
    const index = (this.#tail - 1 + this.#capacity) % this.#capacity;
    return this.#data[index];
  }

  /**
   * Get item at logical index (0 = oldest).
   *
   * @param {number} index - Logical index
   * @returns {T | undefined}
   */
  get(index) {
    if (index < 0 || index >= this.#length) {
      return undefined;
    }
    const actualIndex = (this.#head + index) % this.#capacity;
    return this.#data[actualIndex];
  }

  /**
   * Clear the buffer.
   */
  clear() {
    this.#data = new Array(this.#capacity);
    this.#head = 0;
    this.#tail = 0;
    this.#length = 0;
  }

  /**
   * Get all items as array (oldest first).
   *
   * @returns {T[]}
   */
  toArray() {
    const result = [];
    for (let offsetIndex = 0; offsetIndex < this.#length; offsetIndex++) {
      const actualIndex = (this.#head + offsetIndex) % this.#capacity;
      result.push(this.#data[actualIndex]);
    }
    return result;
  }

  /**
   * Iterate over items (oldest first).
   *
   * @returns {IterableIterator<T>}
   */
  *[Symbol.iterator]() {
    for (let offsetIndex = 0; offsetIndex < this.#length; offsetIndex++) {
      const actualIndex = (this.#head + offsetIndex) % this.#capacity;
      yield this.#data[actualIndex];
    }
  }
}
