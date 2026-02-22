// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeLRU - Typed wrapper for LRU cache with TTL support.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * The JS FFI layer provides LRUCache and TTLLRUCache classes
 * backed by native LRU cache implementations.
 */

/** Opaque handle to a JavaScript LRUCache instance. */
type lruCache

/** Opaque handle to a JavaScript TTLLRUCache instance. */
type ttlLruCache

/** JavaScript bindings to the SafeLRU FFI wrapper. */
module SafeLruJs = {
  @module("../../javascript/src/safe_lru.js") @scope("SafeLRU")
  external create: int => lruCache = "create"

  @module("../../javascript/src/safe_lru.js") @scope("SafeLRU")
  external createWithTtl: (int, int) => ttlLruCache = "createWithTtl"
}

/**
 * Create a new LRU cache.
 *
 * @param capacity Maximum number of entries.
 * @returns A new LRUCache handle.
 */
let create = SafeLruJs.create

/**
 * Create a new LRU cache with TTL support.
 *
 * @param capacity Maximum number of entries.
 * @param ttlMs Time-to-live in milliseconds.
 * @returns A new TTLLRUCache handle.
 */
let createWithTtl = SafeLruJs.createWithTtl
