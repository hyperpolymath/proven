// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeBloom - Probabilistic set membership (Bloom filter).
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Bloom filter backed by the libproven FFI.
 */
export class BloomFilter {
  /** @type {Deno.PointerObject|null} */
  #ptr = null;

  /**
   * Create a bloom filter.
   *
   * @param {number} expectedElements - Expected number of elements.
   * @param {number} falsePositiveRate - Desired false positive rate (0-1).
   * @returns {{ ok: true, value: BloomFilter } | { ok: false, error: string }}
   */
  static create(expectedElements, falsePositiveRate) {
    const symbols = getLib();
    const ptr = symbols.proven_bloom_create(expectedElements, falsePositiveRate);
    if (ptr === null) return err('Failed to create bloom filter');
    const filter = new BloomFilter();
    filter.#ptr = ptr;
    return ok(filter);
  }

  /**
   * Add an element to the filter.
   *
   * @param {Uint8Array|string} element - The element to add.
   */
  add(element) {
    if (this.#ptr === null) return;
    const symbols = getLib();
    const bytes = typeof element === 'string' ? new TextEncoder().encode(element) : element;
    symbols.proven_bloom_add(this.#ptr, bytes, bytes.length);
  }

  /**
   * Check if an element might be in the filter.
   *
   * @param {Uint8Array|string} element - The element to check.
   * @returns {boolean} True if the element might be in the filter (may be false positive).
   */
  contains(element) {
    if (this.#ptr === null) return false;
    const symbols = getLib();
    const bytes = typeof element === 'string' ? new TextEncoder().encode(element) : element;
    return symbols.proven_bloom_contains(this.#ptr, bytes, bytes.length);
  }

  /**
   * Close and free the native bloom filter.
   */
  close() {
    if (this.#ptr !== null) {
      const symbols = getLib();
      symbols.proven_bloom_free(this.#ptr);
      this.#ptr = null;
    }
  }
}
