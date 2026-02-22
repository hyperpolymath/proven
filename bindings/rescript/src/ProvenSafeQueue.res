// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeQueue - Typed wrapper for bounded queues and priority queues.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * The JS FFI layer provides BoundedQueue, PriorityQueue, and BoundedDeque
 * classes backed by native queue implementations.
 */

/** Opaque handle to a JavaScript BoundedQueue instance. */
type boundedQueue

/** Opaque handle to a JavaScript PriorityQueue instance. */
type priorityQueue

/** Opaque handle to a JavaScript BoundedDeque instance. */
type boundedDeque

/** JavaScript bindings to the SafeQueue FFI wrapper. */
module SafeQueueJs = {
  @module("../../javascript/src/safe_queue.js") @scope("SafeQueue")
  external createBounded: int => boundedQueue = "createBounded"

  @module("../../javascript/src/safe_queue.js") @scope("SafeQueue")
  external createPriority: int => priorityQueue = "createPriority"

  @module("../../javascript/src/safe_queue.js") @scope("SafeQueue")
  external createDeque: int => boundedDeque = "createDeque"
}

/**
 * Create a new bounded queue.
 *
 * @param capacity Maximum number of elements.
 * @returns A new BoundedQueue handle.
 */
let createBounded = SafeQueueJs.createBounded

/**
 * Create a new priority queue.
 *
 * @param capacity Maximum number of elements.
 * @returns A new PriorityQueue handle.
 */
let createPriority = SafeQueueJs.createPriority

/**
 * Create a new bounded deque (double-ended queue).
 *
 * @param capacity Maximum number of elements.
 * @returns A new BoundedDeque handle.
 */
let createDeque = SafeQueueJs.createDeque
