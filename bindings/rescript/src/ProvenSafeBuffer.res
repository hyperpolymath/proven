// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeBuffer - Typed wrapper for bounded buffer operations.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * The JS FFI layer provides BoundedBuffer, RingBuffer, and GrowableBuffer
 * classes backed by native buffer implementations.
 */

/** Opaque handle to a JavaScript BoundedBuffer instance. */
type boundedBuffer

/** Opaque handle to a JavaScript RingBuffer instance. */
type ringBuffer

/** Opaque handle to a JavaScript GrowableBuffer instance. */
type growableBuffer

/** JavaScript bindings to the SafeBuffer FFI wrapper. */
module SafeBufferJs = {
  @module("../../javascript/src/safe_buffer.js") @scope("SafeBuffer")
  external createBounded: int => boundedBuffer = "createBounded"

  @module("../../javascript/src/safe_buffer.js") @scope("SafeBuffer")
  external createRing: int => ringBuffer = "createRing"

  @module("../../javascript/src/safe_buffer.js") @scope("SafeBuffer")
  external createGrowable: int => growableBuffer = "createGrowable"
}

/**
 * Create a new bounded buffer with a maximum capacity.
 *
 * @param capacity Maximum number of elements.
 * @returns A new BoundedBuffer handle.
 */
let createBounded = SafeBufferJs.createBounded

/**
 * Create a new ring buffer with a fixed capacity.
 *
 * @param capacity Maximum number of elements (overwrites oldest on overflow).
 * @returns A new RingBuffer handle.
 */
let createRing = SafeBufferJs.createRing

/**
 * Create a new growable buffer with an initial capacity.
 *
 * @param initialCapacity Initial capacity.
 * @returns A new GrowableBuffer handle.
 */
let createGrowable = SafeBufferJs.createGrowable
