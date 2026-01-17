// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * BloomFilter is a probabilistic data structure for set membership testing.
 */
export class BloomFilter {
  private readonly bits: Uint8Array;
  private readonly size: number;
  private readonly hashCount: number;
  private count: number = 0;

  constructor(size: number, hashCount: number) {
    this.size = Math.max(1, size);
    this.hashCount = Math.max(1, hashCount);
    this.bits = new Uint8Array(Math.ceil(this.size / 8));
  }

  /**
   * Create an optimal bloom filter for expected items and false positive rate.
   */
  static optimal(expectedItems: number, falsePositiveRate: number = 0.01): BloomFilter {
    if (expectedItems < 1) expectedItems = 1;
    if (falsePositiveRate <= 0) falsePositiveRate = 0.01;
    if (falsePositiveRate >= 1) falsePositiveRate = 0.99;

    // m = -(n * ln(p)) / (ln(2)^2)
    const m = Math.ceil((-expectedItems * Math.log(falsePositiveRate)) / (Math.LN2 * Math.LN2));
    // k = (m/n) * ln(2)
    const k = Math.ceil((m / expectedItems) * Math.LN2);

    return new BloomFilter(m, k);
  }

  /**
   * Add an item to the filter.
   */
  add(item: string): void {
    for (let i = 0; i < this.hashCount; i++) {
      const index = this.hash(item, i);
      this.setBit(index);
    }
    this.count++;
  }

  /**
   * Check if an item might be in the set.
   */
  contains(item: string): boolean {
    for (let i = 0; i < this.hashCount; i++) {
      const index = this.hash(item, i);
      if (!this.getBit(index)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Get the number of items added.
   */
  get itemCount(): number {
    return this.count;
  }

  /**
   * Get the bit array size.
   */
  get bitSize(): number {
    return this.size;
  }

  /**
   * Get the number of hash functions.
   */
  get hashFunctionCount(): number {
    return this.hashCount;
  }

  /**
   * Estimate current false positive rate.
   */
  falsePositiveRate(): number {
    // (1 - e^(-k*n/m))^k
    const exponent = (-this.hashCount * this.count) / this.size;
    return Math.pow(1 - Math.exp(exponent), this.hashCount);
  }

  /**
   * Clear the filter.
   */
  clear(): void {
    this.bits.fill(0);
    this.count = 0;
  }

  /**
   * Union with another filter (same size and hash count).
   */
  union(other: BloomFilter): Result<BloomFilter> {
    if (this.size !== other.size || this.hashCount !== other.hashCount) {
      return { ok: false, error: 'Filters must have same size and hash count' };
    }

    const result = new BloomFilter(this.size, this.hashCount);
    for (let i = 0; i < this.bits.length; i++) {
      result.bits[i] = this.bits[i] | other.bits[i];
    }
    result.count = this.count + other.count;
    return { ok: true, value: result };
  }

  /**
   * Intersection with another filter.
   */
  intersection(other: BloomFilter): Result<BloomFilter> {
    if (this.size !== other.size || this.hashCount !== other.hashCount) {
      return { ok: false, error: 'Filters must have same size and hash count' };
    }

    const result = new BloomFilter(this.size, this.hashCount);
    for (let i = 0; i < this.bits.length; i++) {
      result.bits[i] = this.bits[i] & other.bits[i];
    }
    return { ok: true, value: result };
  }

  private hash(item: string, seed: number): number {
    // FNV-1a hash with seed
    let hash = 2166136261 ^ seed;
    for (let i = 0; i < item.length; i++) {
      hash ^= item.charCodeAt(i);
      hash = Math.imul(hash, 16777619);
    }
    return Math.abs(hash) % this.size;
  }

  private setBit(index: number): void {
    const byteIndex = Math.floor(index / 8);
    const bitIndex = index % 8;
    this.bits[byteIndex] |= 1 << bitIndex;
  }

  private getBit(index: number): boolean {
    const byteIndex = Math.floor(index / 8);
    const bitIndex = index % 8;
    return (this.bits[byteIndex] & (1 << bitIndex)) !== 0;
  }
}

/**
 * CountingBloomFilter allows removal of items.
 */
export class CountingBloomFilter {
  private readonly counters: Uint8Array;
  private readonly size: number;
  private readonly hashCount: number;
  private count: number = 0;

  constructor(size: number, hashCount: number) {
    this.size = Math.max(1, size);
    this.hashCount = Math.max(1, hashCount);
    this.counters = new Uint8Array(this.size);
  }

  /**
   * Add an item to the filter.
   */
  add(item: string): void {
    for (let i = 0; i < this.hashCount; i++) {
      const index = this.hash(item, i);
      if (this.counters[index] < 255) {
        this.counters[index]++;
      }
    }
    this.count++;
  }

  /**
   * Remove an item from the filter.
   */
  remove(item: string): boolean {
    if (!this.contains(item)) {
      return false;
    }

    for (let i = 0; i < this.hashCount; i++) {
      const index = this.hash(item, i);
      if (this.counters[index] > 0) {
        this.counters[index]--;
      }
    }
    this.count--;
    return true;
  }

  /**
   * Check if an item might be in the set.
   */
  contains(item: string): boolean {
    for (let i = 0; i < this.hashCount; i++) {
      const index = this.hash(item, i);
      if (this.counters[index] === 0) {
        return false;
      }
    }
    return true;
  }

  /**
   * Get the number of items added.
   */
  get itemCount(): number {
    return this.count;
  }

  /**
   * Clear the filter.
   */
  clear(): void {
    this.counters.fill(0);
    this.count = 0;
  }

  private hash(item: string, seed: number): number {
    let hash = 2166136261 ^ seed;
    for (let i = 0; i < item.length; i++) {
      hash ^= item.charCodeAt(i);
      hash = Math.imul(hash, 16777619);
    }
    return Math.abs(hash) % this.size;
  }
}

export const SafeBloom = {
  BloomFilter,
  CountingBloomFilter,
};
