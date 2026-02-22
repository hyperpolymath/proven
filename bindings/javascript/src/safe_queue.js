// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeQueue - Bounded FIFO queue.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Bounded FIFO queue backed by the libproven FFI.
 */
export class BoundedQueue {
  /** @type {Deno.PointerObject|null} */
  #ptr = null;

  /**
   * Create a bounded queue with the given capacity.
   *
   * @param {number} capacity - Maximum number of elements.
   * @returns {{ ok: true, value: BoundedQueue } | { ok: false, error: string }}
   */
  static create(capacity) {
    const symbols = getLib();
    const ptr = symbols.proven_queue_create(capacity);
    if (ptr === null) return err('Failed to create queue');
    const queue = new BoundedQueue();
    queue.#ptr = ptr;
    return ok(queue);
  }

  /**
   * Push a value to the queue.
   *
   * @param {number|bigint} value - The value to push.
   * @returns {boolean} True if push succeeded (queue not full).
   */
  push(value) {
    if (this.#ptr === null) return false;
    const symbols = getLib();
    return symbols.proven_queue_push(this.#ptr, BigInt(value));
  }

  /**
   * Pop a value from the queue.
   *
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  pop() {
    if (this.#ptr === null) return err('Queue is closed');
    const symbols = getLib();
    const result = symbols.proven_queue_pop(this.#ptr);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Get the current size of the queue.
   *
   * @returns {number}
   */
  size() {
    if (this.#ptr === null) return 0;
    const symbols = getLib();
    return Number(symbols.proven_queue_size(this.#ptr));
  }

  /**
   * Close and free the native queue.
   */
  close() {
    if (this.#ptr !== null) {
      const symbols = getLib();
      symbols.proven_queue_free(this.#ptr);
      this.#ptr = null;
    }
  }
}

/**
 * Priority queue (placeholder -- the FFI layer provides FIFO only;
 * priority queue will be added when the Idris implementation is complete).
 */
export class PriorityQueue extends BoundedQueue {}
