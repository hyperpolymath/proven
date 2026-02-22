// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeBuffer - Bounded buffer operations.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Bounded buffer backed by the libproven FFI.
 * The native buffer is heap-allocated by the Zig/Idris runtime and
 * freed when close() is called.
 */
export class BoundedBuffer {
  /** @type {Deno.PointerObject|null} */
  #ptr = null;

  /**
   * Create a bounded buffer with the given capacity.
   *
   * @param {number} capacity - Maximum buffer size in bytes.
   * @returns {{ ok: true, value: BoundedBuffer } | { ok: false, error: string }}
   */
  static create(capacity) {
    const symbols = getLib();
    const result = symbols.proven_buffer_create(capacity);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    const buf = new BoundedBuffer();
    buf.#ptr = result[1];
    return ok(buf);
  }

  /**
   * Append bytes to the buffer.
   *
   * @param {Uint8Array|string} data - Data to append.
   * @returns {{ ok: true, value: undefined } | { ok: false, error: string }}
   */
  append(data) {
    if (this.#ptr === null) return err('Buffer is closed');
    const symbols = getLib();
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
    const status = symbols.proven_buffer_append(this.#ptr, bytes, bytes.length);
    if (status !== ProvenStatus.OK) return err(statusToError(status));
    return ok(undefined);
  }

  /**
   * Close and free the native buffer.
   */
  close() {
    if (this.#ptr !== null) {
      const symbols = getLib();
      symbols.proven_buffer_free(this.#ptr);
      this.#ptr = null;
    }
  }
}

/**
 * Ring buffer (alias for BoundedBuffer -- the FFI uses the same underlying
 * BoundedBuffer type with circular semantics).
 */
export class RingBuffer extends BoundedBuffer {}
