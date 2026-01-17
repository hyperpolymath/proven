// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * BoundedQueue is a bounded FIFO queue.
 */
export class BoundedQueue<T> {
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
   * Check if queue is full.
   */
  isFull(): boolean {
    return this.data.length >= this.capacity;
  }

  /**
   * Check if queue is empty.
   */
  isEmpty(): boolean {
    return this.data.length === 0;
  }

  /**
   * Enqueue an item (fails if full).
   */
  enqueue(item: T): boolean {
    if (this.isFull()) {
      return false;
    }
    this.data.push(item);
    return true;
  }

  /**
   * Dequeue the first item.
   */
  dequeue(): Result<T> {
    if (this.isEmpty()) {
      return { ok: false, error: 'Queue is empty' };
    }
    return { ok: true, value: this.data.shift()! };
  }

  /**
   * Peek at the first item without removing.
   */
  peek(): Result<T> {
    if (this.isEmpty()) {
      return { ok: false, error: 'Queue is empty' };
    }
    return { ok: true, value: this.data[0] };
  }

  /**
   * Clear the queue.
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

/**
 * PriorityQueue is a min-heap priority queue.
 */
export class PriorityQueue<T> {
  private readonly heap: T[];
  private readonly capacity: number;
  private readonly comparator: (a: T, b: T) => number;

  /**
   * Create a priority queue.
   * @param capacity Maximum capacity
   * @param comparator Comparison function (negative if a < b, positive if a > b, 0 if equal)
   */
  constructor(capacity: number, comparator: (a: T, b: T) => number) {
    this.capacity = Math.max(1, capacity);
    this.heap = [];
    this.comparator = comparator;
  }

  /**
   * Create a min-heap for numbers.
   */
  static minHeap(capacity: number): PriorityQueue<number> {
    return new PriorityQueue(capacity, (a, b) => a - b);
  }

  /**
   * Create a max-heap for numbers.
   */
  static maxHeap(capacity: number): PriorityQueue<number> {
    return new PriorityQueue(capacity, (a, b) => b - a);
  }

  /**
   * Get current length.
   */
  get length(): number {
    return this.heap.length;
  }

  /**
   * Get capacity.
   */
  get cap(): number {
    return this.capacity;
  }

  /**
   * Check if queue is full.
   */
  isFull(): boolean {
    return this.heap.length >= this.capacity;
  }

  /**
   * Check if queue is empty.
   */
  isEmpty(): boolean {
    return this.heap.length === 0;
  }

  /**
   * Push an item (fails if full).
   */
  push(item: T): boolean {
    if (this.isFull()) {
      return false;
    }
    this.heap.push(item);
    this.bubbleUp(this.heap.length - 1);
    return true;
  }

  /**
   * Pop the highest priority item.
   */
  pop(): Result<T> {
    if (this.isEmpty()) {
      return { ok: false, error: 'Queue is empty' };
    }

    const top = this.heap[0];
    const last = this.heap.pop()!;

    if (this.heap.length > 0) {
      this.heap[0] = last;
      this.bubbleDown(0);
    }

    return { ok: true, value: top };
  }

  /**
   * Peek at the highest priority item without removing.
   */
  peek(): Result<T> {
    if (this.isEmpty()) {
      return { ok: false, error: 'Queue is empty' };
    }
    return { ok: true, value: this.heap[0] };
  }

  /**
   * Clear the queue.
   */
  clear(): void {
    this.heap.length = 0;
  }

  /**
   * Convert to array (not in priority order).
   */
  toArray(): T[] {
    return [...this.heap];
  }

  private bubbleUp(index: number): void {
    while (index > 0) {
      const parent = Math.floor((index - 1) / 2);
      if (this.comparator(this.heap[index], this.heap[parent]) >= 0) {
        break;
      }
      [this.heap[index], this.heap[parent]] = [this.heap[parent], this.heap[index]];
      index = parent;
    }
  }

  private bubbleDown(index: number): void {
    const length = this.heap.length;

    while (true) {
      const left = 2 * index + 1;
      const right = 2 * index + 2;
      let smallest = index;

      if (left < length && this.comparator(this.heap[left], this.heap[smallest]) < 0) {
        smallest = left;
      }
      if (right < length && this.comparator(this.heap[right], this.heap[smallest]) < 0) {
        smallest = right;
      }

      if (smallest === index) {
        break;
      }

      [this.heap[index], this.heap[smallest]] = [this.heap[smallest], this.heap[index]];
      index = smallest;
    }
  }
}

/**
 * Deque (double-ended queue) with bounded capacity.
 */
export class BoundedDeque<T> {
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
   * Check if deque is full.
   */
  isFull(): boolean {
    return this.data.length >= this.capacity;
  }

  /**
   * Check if deque is empty.
   */
  isEmpty(): boolean {
    return this.data.length === 0;
  }

  /**
   * Push to front.
   */
  pushFront(item: T): boolean {
    if (this.isFull()) return false;
    this.data.unshift(item);
    return true;
  }

  /**
   * Push to back.
   */
  pushBack(item: T): boolean {
    if (this.isFull()) return false;
    this.data.push(item);
    return true;
  }

  /**
   * Pop from front.
   */
  popFront(): Result<T> {
    if (this.isEmpty()) {
      return { ok: false, error: 'Deque is empty' };
    }
    return { ok: true, value: this.data.shift()! };
  }

  /**
   * Pop from back.
   */
  popBack(): Result<T> {
    if (this.isEmpty()) {
      return { ok: false, error: 'Deque is empty' };
    }
    return { ok: true, value: this.data.pop()! };
  }

  /**
   * Peek front.
   */
  peekFront(): Result<T> {
    if (this.isEmpty()) {
      return { ok: false, error: 'Deque is empty' };
    }
    return { ok: true, value: this.data[0] };
  }

  /**
   * Peek back.
   */
  peekBack(): Result<T> {
    if (this.isEmpty()) {
      return { ok: false, error: 'Deque is empty' };
    }
    return { ok: true, value: this.data[this.data.length - 1] };
  }

  /**
   * Clear the deque.
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

export const SafeQueue = {
  BoundedQueue,
  PriorityQueue,
  BoundedDeque,
};
