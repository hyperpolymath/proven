// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * BoundedBuffer is a bounded stack-like buffer.
 */
export class BoundedBuffer<T> {
  private readonly data: T[];
  private readonly capacity: number;

  constructor(capacity: number) {
    this.capacity = Math.max(1, capacity);
    this.data = [];
  }

  /**
   * Get current length.
   */
  get length(): number {
    return this.data.length;
  }

  /**
   * Get capacity.
   */
  get cap(): number {
    return this.capacity;
  }

  /**
   * Check if buffer is full.
   */
  isFull(): boolean {
    return this.data.length >= this.capacity;
  }

  /**
   * Check if buffer is empty.
   */
  isEmpty(): boolean {
    return this.data.length === 0;
  }

  /**
   * Push an item (fails if full).
   */
  push(item: T): boolean {
    if (this.isFull()) {
      return false;
    }
    this.data.push(item);
    return true;
  }

  /**
   * Pop the last item.
   */
  pop(): Result<T> {
    if (this.isEmpty()) {
      return { ok: false, error: 'Buffer is empty' };
    }
    return { ok: true, value: this.data.pop()! };
  }

  /**
   * Peek at the last item without removing.
   */
  peek(): Result<T> {
    if (this.isEmpty()) {
      return { ok: false, error: 'Buffer is empty' };
    }
    return { ok: true, value: this.data[this.data.length - 1] };
  }

  /**
   * Get item at index.
   */
  get(index: number): Result<T> {
    if (index < 0 || index >= this.data.length) {
      return { ok: false, error: `Index ${index} out of bounds` };
    }
    return { ok: true, value: this.data[index] };
  }

  /**
   * Clear the buffer.
   */
  clear(): void {
    this.data.length = 0;
  }

  /**
   * Get remaining capacity.
   */
  remaining(): number {
    return this.capacity - this.data.length;
  }

  /**
   * Convert to array.
   */
  toArray(): T[] {
    return [...this.data];
  }
}

/**
 * RingBuffer is a circular buffer.
 */
export class RingBuffer<T> {
  private readonly data: (T | undefined)[];
  private readonly capacity: number;
  private head: number = 0;
  private tail: number = 0;
  private count: number = 0;

  constructor(capacity: number) {
    this.capacity = Math.max(1, capacity);
    this.data = new Array(this.capacity);
  }

  /**
   * Get current length.
   */
  get length(): number {
    return this.count;
  }

  /**
   * Get capacity.
   */
  get cap(): number {
    return this.capacity;
  }

  /**
   * Check if buffer is full.
   */
  isFull(): boolean {
    return this.count >= this.capacity;
  }

  /**
   * Check if buffer is empty.
   */
  isEmpty(): boolean {
    return this.count === 0;
  }

  /**
   * Push an item (overwrites oldest if full).
   * Returns the overwritten item if any.
   */
  push(item: T): { overwritten?: T } {
    let overwritten: T | undefined;

    if (this.isFull()) {
      overwritten = this.data[this.head];
      this.head = (this.head + 1) % this.capacity;
    } else {
      this.count++;
    }

    this.data[this.tail] = item;
    this.tail = (this.tail + 1) % this.capacity;

    return { overwritten };
  }

  /**
   * Pop the oldest item (FIFO).
   */
  pop(): Result<T> {
    if (this.isEmpty()) {
      return { ok: false, error: 'Buffer is empty' };
    }

    const item = this.data[this.head]!;
    this.data[this.head] = undefined;
    this.head = (this.head + 1) % this.capacity;
    this.count--;

    return { ok: true, value: item };
  }

  /**
   * Peek at the oldest item without removing.
   */
  peek(): Result<T> {
    if (this.isEmpty()) {
      return { ok: false, error: 'Buffer is empty' };
    }
    return { ok: true, value: this.data[this.head]! };
  }

  /**
   * Peek at the newest item without removing.
   */
  peekNewest(): Result<T> {
    if (this.isEmpty()) {
      return { ok: false, error: 'Buffer is empty' };
    }
    const index = (this.tail - 1 + this.capacity) % this.capacity;
    return { ok: true, value: this.data[index]! };
  }

  /**
   * Get item at logical index (0 = oldest).
   */
  get(index: number): Result<T> {
    if (index < 0 || index >= this.count) {
      return { ok: false, error: `Index ${index} out of bounds` };
    }
    const actualIndex = (this.head + index) % this.capacity;
    return { ok: true, value: this.data[actualIndex]! };
  }

  /**
   * Clear the buffer.
   */
  clear(): void {
    this.data.fill(undefined);
    this.head = 0;
    this.tail = 0;
    this.count = 0;
  }

  /**
   * Convert to array (oldest first).
   */
  toArray(): T[] {
    const result: T[] = [];
    for (let i = 0; i < this.count; i++) {
      const actualIndex = (this.head + i) % this.capacity;
      result.push(this.data[actualIndex]!);
    }
    return result;
  }
}

/**
 * GrowableBuffer is an auto-growing buffer with configurable limits.
 */
export class GrowableBuffer<T> {
  private data: T[];
  private maxCapacity: number;

  constructor(initialCapacity: number = 16, maxCapacity: number = Infinity) {
    this.data = [];
    this.data.length = Math.max(1, initialCapacity);
    this.data.length = 0;
    this.maxCapacity = maxCapacity;
  }

  /**
   * Get current length.
   */
  get length(): number {
    return this.data.length;
  }

  /**
   * Check if at max capacity.
   */
  isFull(): boolean {
    return this.data.length >= this.maxCapacity;
  }

  /**
   * Push an item (fails if at max capacity).
   */
  push(item: T): boolean {
    if (this.isFull()) {
      return false;
    }
    this.data.push(item);
    return true;
  }

  /**
   * Push multiple items.
   */
  pushMany(items: T[]): number {
    let added = 0;
    for (const item of items) {
      if (!this.push(item)) break;
      added++;
    }
    return added;
  }

  /**
   * Pop the last item.
   */
  pop(): Result<T> {
    if (this.data.length === 0) {
      return { ok: false, error: 'Buffer is empty' };
    }
    return { ok: true, value: this.data.pop()! };
  }

  /**
   * Get item at index.
   */
  get(index: number): Result<T> {
    if (index < 0 || index >= this.data.length) {
      return { ok: false, error: `Index ${index} out of bounds` };
    }
    return { ok: true, value: this.data[index] };
  }

  /**
   * Clear the buffer.
   */
  clear(): void {
    this.data.length = 0;
  }

  /**
   * Convert to array.
   */
  toArray(): T[] {
    return [...this.data];
  }
}

export const SafeBuffer = {
  BoundedBuffer,
  RingBuffer,
  GrowableBuffer,
};
