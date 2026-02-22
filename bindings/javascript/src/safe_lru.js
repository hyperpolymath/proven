// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeLRU - Least Recently Used cache.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * LRU cache backed by the libproven FFI.
 * Keys are u64, values are i64.
 */
export class LruCache {
  /** @type {Deno.PointerObject|null} */
  #ptr = null;

  /**
   * Create an LRU cache with the given capacity.
   *
   * @param {number} capacity - Maximum number of entries.
   * @returns {{ ok: true, value: LruCache } | { ok: false, error: string }}
   */
  static create(capacity) {
    const symbols = getLib();
    const ptr = symbols.proven_lru_create(capacity);
    if (ptr === null) return err('Failed to create LRU cache');
    const cache = new LruCache();
    cache.#ptr = ptr;
    return ok(cache);
  }

  /**
   * Get a value by key.
   *
   * @param {number|bigint} key - The key to look up.
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  get(key) {
    if (this.#ptr === null) return err('Cache is closed');
    const symbols = getLib();
    const result = symbols.proven_lru_get(this.#ptr, BigInt(key));
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Put a key-value pair into the cache.
   *
   * @param {number|bigint} key - The key.
   * @param {number|bigint} value - The value.
   * @returns {{ ok: true, value: undefined } | { ok: false, error: string }}
   */
  put(key, value) {
    if (this.#ptr === null) return err('Cache is closed');
    const symbols = getLib();
    const status = symbols.proven_lru_put(this.#ptr, BigInt(key), BigInt(value));
    if (status !== ProvenStatus.OK) return err(statusToError(status));
    return ok(undefined);
  }

  /**
   * Close and free the native LRU cache.
   */
  close() {
    if (this.#ptr !== null) {
      const symbols = getLib();
      symbols.proven_lru_free(this.#ptr);
      this.#ptr = null;
    }
  }
}
