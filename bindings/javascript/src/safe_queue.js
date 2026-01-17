// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeQueue - Bounded FIFO and priority queues.
 *
 * Provides safe queue operations with capacity limits.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Bounded FIFO queue.
 *
 * @template T
 */
export class BoundedQueue {
  /** @type {T[]} */
  #data;
  /** @type {number} */
  #capacity;

  /**
   * Create a bounded queue.
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
   * Check if queue is full.
   *
   * @returns {boolean}
   */
  isFull() {
    return this.#data.length >= this.#capacity;
  }

  /**
   * Check if queue is empty.
   *
   * @returns {boolean}
   */
  isEmpty() {
    return this.#data.length === 0;
  }

  /**
   * Enqueue an item (fails if full).
   *
   * @param {T} item - Item to enqueue
   * @returns {boolean} True if successful
   */
  enqueue(item) {
    if (this.isFull()) {
      return false;
    }
    this.#data.push(item);
    return true;
  }

  /**
   * Dequeue an item (fails if empty).
   *
   * @returns {T | undefined}
   */
  dequeue() {
    return this.#data.shift();
  }

  /**
   * Peek at front item without removing.
   *
   * @returns {T | undefined}
   */
  peek() {
    return this.#data[0];
  }

  /**
   * Clear the queue.
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
   * Iterate over items.
   *
   * @returns {IterableIterator<T>}
   */
  *[Symbol.iterator]() {
    yield* this.#data;
  }
}

/**
 * Priority queue (min-heap by default).
 *
 * @template T
 */
export class PriorityQueue {
  /** @type {T[]} */
  #heap;
  /** @type {(a: T, b: T) => number} */
  #comparator;
  /** @type {number} */
  #capacity;

  /**
   * Create a priority queue.
   *
   * @param {Object} options - Options
   * @param {number} [options.capacity=Infinity] - Maximum capacity
   * @param {(a: T, b: T) => number} [options.comparator] - Comparator (negative if a < b)
   */
  constructor(options = {}) {
    const { capacity = Infinity, comparator = (a, b) => (a < b ? -1 : a > b ? 1 : 0) } = options;

    this.#capacity = capacity;
    this.#comparator = comparator;
    this.#heap = [];
  }

  /**
   * Get current length.
   *
   * @returns {number}
   */
  get length() {
    return this.#heap.length;
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
   * Check if queue is empty.
   *
   * @returns {boolean}
   */
  isEmpty() {
    return this.#heap.length === 0;
  }

  /**
   * Check if queue is full.
   *
   * @returns {boolean}
   */
  isFull() {
    return this.#heap.length >= this.#capacity;
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

    this.#heap.push(item);
    this.#bubbleUp(this.#heap.length - 1);
    return true;
  }

  /**
   * Pop the highest priority item.
   *
   * @returns {T | undefined}
   */
  pop() {
    if (this.isEmpty()) {
      return undefined;
    }

    const top = this.#heap[0];
    const last = this.#heap.pop();

    if (this.#heap.length > 0 && last !== undefined) {
      this.#heap[0] = last;
      this.#bubbleDown(0);
    }

    return top;
  }

  /**
   * Peek at highest priority item without removing.
   *
   * @returns {T | undefined}
   */
  peek() {
    return this.#heap[0];
  }

  /**
   * Clear the queue.
   */
  clear() {
    this.#heap = [];
  }

  /**
   * Get all items as array (not in priority order).
   *
   * @returns {T[]}
   */
  toArray() {
    return [...this.#heap];
  }

  /**
   * Get all items in priority order.
   *
   * @returns {T[]}
   */
  toSortedArray() {
    const result = [];
    const copy = new PriorityQueue({
      capacity: this.#capacity,
      comparator: this.#comparator,
    });
    copy.#heap = [...this.#heap];

    while (!copy.isEmpty()) {
      result.push(copy.pop());
    }

    return result;
  }

  /**
   * Bubble up element at index.
   *
   * @param {number} index - Index
   */
  #bubbleUp(index) {
    while (index > 0) {
      const parentIndex = Math.floor((index - 1) / 2);
      if (this.#comparator(this.#heap[index], this.#heap[parentIndex]) >= 0) {
        break;
      }
      [this.#heap[index], this.#heap[parentIndex]] = [this.#heap[parentIndex], this.#heap[index]];
      index = parentIndex;
    }
  }

  /**
   * Bubble down element at index.
   *
   * @param {number} index - Index
   */
  #bubbleDown(index) {
    const length = this.#heap.length;

    while (true) {
      const leftChildIndex = 2 * index + 1;
      const rightChildIndex = 2 * index + 2;
      let smallestIndex = index;

      if (leftChildIndex < length && this.#comparator(this.#heap[leftChildIndex], this.#heap[smallestIndex]) < 0) {
        smallestIndex = leftChildIndex;
      }

      if (rightChildIndex < length && this.#comparator(this.#heap[rightChildIndex], this.#heap[smallestIndex]) < 0) {
        smallestIndex = rightChildIndex;
      }

      if (smallestIndex === index) {
        break;
      }

      [this.#heap[index], this.#heap[smallestIndex]] = [this.#heap[smallestIndex], this.#heap[index]];
      index = smallestIndex;
    }
  }
}
