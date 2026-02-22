// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeBloom - Typed wrapper for Bloom filter operations.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * The JS FFI layer provides BloomFilter and CountingBloomFilter classes
 * backed by native bloom filter implementations.
 */

/** Opaque handle to a JavaScript BloomFilter instance. */
type bloomFilter

/** Opaque handle to a JavaScript CountingBloomFilter instance. */
type countingBloomFilter

/** JavaScript bindings to the SafeBloom FFI wrapper. */
module SafeBloomJs = {
  @module("../../javascript/src/safe_bloom.js") @scope("SafeBloom")
  external create: (int, float) => bloomFilter = "create"

  @module("../../javascript/src/safe_bloom.js") @scope("SafeBloom")
  external createCounting: (int, float) => countingBloomFilter = "createCounting"
}

/**
 * Create a new Bloom filter.
 *
 * @param expectedItems Expected number of items.
 * @param falsePositiveRate Desired false positive rate (0.0 to 1.0).
 * @returns A new BloomFilter handle.
 */
let create = SafeBloomJs.create

/**
 * Create a new counting Bloom filter.
 *
 * @param expectedItems Expected number of items.
 * @param falsePositiveRate Desired false positive rate (0.0 to 1.0).
 * @returns A new CountingBloomFilter handle.
 */
let createCounting = SafeBloomJs.createCounting
