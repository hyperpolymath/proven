// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeBloom - Probabilistic set membership.
 *
 * Provides Bloom filter implementation with configurable parameters.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Bloom filter for probabilistic set membership.
 */
export class BloomFilter {
  /** @type {Uint8Array} */
  #bits;
  /** @type {number} */
  #numBits;
  /** @type {number} */
  #numHashFunctions;
  /** @type {number} */
  #count;

  /**
   * Create a Bloom filter.
   *
   * @param {Object} options - Options
   * @param {number} options.expectedItems - Expected number of items
   * @param {number} [options.falsePositiveRate=0.01] - Desired false positive rate
   */
  constructor(options) {
    const { expectedItems, falsePositiveRate = 0.01 } = options;

    if (expectedItems < 1) {
      throw new Error('Expected items must be at least 1');
    }
    if (falsePositiveRate <= 0 || falsePositiveRate >= 1) {
      throw new Error('False positive rate must be between 0 and 1');
    }

    // Calculate optimal parameters
    // m = -n * ln(p) / (ln(2)^2)
    const ln2Squared = Math.LN2 * Math.LN2;
    this.#numBits = Math.ceil((-expectedItems * Math.log(falsePositiveRate)) / ln2Squared);

    // k = m/n * ln(2)
    this.#numHashFunctions = Math.max(1, Math.round((this.#numBits / expectedItems) * Math.LN2));

    // Create bit array (using Uint8Array for efficiency)
    const numBytes = Math.ceil(this.#numBits / 8);
    this.#bits = new Uint8Array(numBytes);
    this.#count = 0;
  }

  /**
   * Get number of bits.
   *
   * @returns {number}
   */
  get numBits() {
    return this.#numBits;
  }

  /**
   * Get number of hash functions.
   *
   * @returns {number}
   */
  get numHashFunctions() {
    return this.#numHashFunctions;
  }

  /**
   * Get number of items added.
   *
   * @returns {number}
   */
  get count() {
    return this.#count;
  }

  /**
   * Add an item to the filter.
   *
   * @param {string | Uint8Array} item - Item to add
   */
  add(item) {
    const data = typeof item === 'string' ? new TextEncoder().encode(item) : item;
    const hashes = this.#getHashValues(data);

    for (const hash of hashes) {
      const bitIndex = hash % this.#numBits;
      const byteIndex = Math.floor(bitIndex / 8);
      const bitOffset = bitIndex % 8;
      this.#bits[byteIndex] |= 1 << bitOffset;
    }

    this.#count++;
  }

  /**
   * Check if item might be in the filter.
   *
   * @param {string | Uint8Array} item - Item to check
   * @returns {boolean} True if item might be present (false positives possible)
   */
  mightContain(item) {
    const data = typeof item === 'string' ? new TextEncoder().encode(item) : item;
    const hashes = this.#getHashValues(data);

    for (const hash of hashes) {
      const bitIndex = hash % this.#numBits;
      const byteIndex = Math.floor(bitIndex / 8);
      const bitOffset = bitIndex % 8;

      if ((this.#bits[byteIndex] & (1 << bitOffset)) === 0) {
        return false;
      }
    }

    return true;
  }

  /**
   * Clear the filter.
   */
  clear() {
    this.#bits.fill(0);
    this.#count = 0;
  }

  /**
   * Get the current false positive probability.
   *
   * @returns {number}
   */
  getFalsePositiveProbability() {
    // p = (1 - e^(-k*n/m))^k
    const exponent = (-this.#numHashFunctions * this.#count) / this.#numBits;
    return Math.pow(1 - Math.exp(exponent), this.#numHashFunctions);
  }

  /**
   * Generate hash values for an item using double hashing.
   *
   * @param {Uint8Array} data - Data to hash
   * @returns {number[]}
   */
  #getHashValues(data) {
    // Use FNV-1a for base hashes
    const hash1 = this.#fnv1a(data);
    const hash2 = this.#fnv1a(data, hash1);

    const hashes = [];
    for (let hashIndex = 0; hashIndex < this.#numHashFunctions; hashIndex++) {
      // Combine hashes using double hashing: h(i) = h1 + i * h2
      const combinedHash = (hash1 + hashIndex * hash2) >>> 0;
      hashes.push(combinedHash);
    }

    return hashes;
  }

  /**
   * FNV-1a 32-bit hash.
   *
   * @param {Uint8Array} data - Data to hash
   * @param {number} [seed=0x811c9dc5] - Initial hash value
   * @returns {number}
   */
  #fnv1a(data, seed = 0x811c9dc5) {
    let hash = seed;
    for (let byteIndex = 0; byteIndex < data.length; byteIndex++) {
      hash ^= data[byteIndex];
      hash = Math.imul(hash, 0x01000193) >>> 0;
    }
    return hash >>> 0;
  }

  /**
   * Get memory usage in bytes.
   *
   * @returns {number}
   */
  getMemoryUsage() {
    return this.#bits.length;
  }

  /**
   * Export filter state.
   *
   * @returns {Object}
   */
  export() {
    return {
      numBits: this.#numBits,
      numHashFunctions: this.#numHashFunctions,
      count: this.#count,
      bits: Array.from(this.#bits),
    };
  }

  /**
   * Import filter state.
   *
   * @param {Object} state - Exported state
   * @returns {BloomFilter}
   */
  static import(state) {
    const filter = Object.create(BloomFilter.prototype);
    filter.#numBits = state.numBits;
    filter.#numHashFunctions = state.numHashFunctions;
    filter.#count = state.count;
    filter.#bits = new Uint8Array(state.bits);
    return filter;
  }

  /**
   * Create with optimal parameters for given constraints.
   *
   * @param {number} expectedItems - Expected number of items
   * @param {number} falsePositiveRate - Desired false positive rate
   * @returns {BloomFilter}
   */
  static optimal(expectedItems, falsePositiveRate) {
    return new BloomFilter({ expectedItems, falsePositiveRate });
  }
}
