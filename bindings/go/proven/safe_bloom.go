// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"hash/fnv"
	"math"
)

// BloomFilter is a probabilistic data structure for set membership testing.
type BloomFilter struct {
	bits    []bool
	size    int
	hashes  int
	count   int
}

// NewBloomFilter creates a bloom filter with given size and hash count.
func NewBloomFilter(size, hashes int) *BloomFilter {
	if size < 1 {
		size = 1
	}
	if hashes < 1 {
		hashes = 1
	}
	return &BloomFilter{
		bits:   make([]bool, size),
		size:   size,
		hashes: hashes,
	}
}

// OptimalBloomFilter creates a bloom filter optimized for expected items and false positive rate.
func OptimalBloomFilter(expectedItems int, falsePositiveRate float64) *BloomFilter {
	if expectedItems < 1 {
		expectedItems = 1
	}
	if falsePositiveRate <= 0 {
		falsePositiveRate = 0.01
	}
	if falsePositiveRate >= 1 {
		falsePositiveRate = 0.99
	}

	// m = -(n * ln(p)) / (ln(2)^2)
	m := int(math.Ceil(-float64(expectedItems) * math.Log(falsePositiveRate) / (math.Ln2 * math.Ln2)))
	// k = (m/n) * ln(2)
	k := int(math.Ceil(float64(m) / float64(expectedItems) * math.Ln2))

	return NewBloomFilter(m, k)
}

// Add adds an item to the bloom filter.
func (bf *BloomFilter) Add(item string) {
	for i := 0; i < bf.hashes; i++ {
		index := bf.hash(item, i)
		bf.bits[index] = true
	}
	bf.count++
}

// Contains checks if an item might be in the set.
// Returns true if the item might be present (possible false positive).
// Returns false if the item is definitely not present.
func (bf *BloomFilter) Contains(item string) bool {
	for i := 0; i < bf.hashes; i++ {
		index := bf.hash(item, i)
		if !bf.bits[index] {
			return false
		}
	}
	return true
}

// Count returns the number of items added.
func (bf *BloomFilter) Count() int {
	return bf.count
}

// Size returns the bit array size.
func (bf *BloomFilter) Size() int {
	return bf.size
}

// HashCount returns the number of hash functions.
func (bf *BloomFilter) HashCount() int {
	return bf.hashes
}

// FalsePositiveRate estimates the current false positive rate.
func (bf *BloomFilter) FalsePositiveRate() float64 {
	// (1 - e^(-k*n/m))^k
	exponent := -float64(bf.hashes) * float64(bf.count) / float64(bf.size)
	return math.Pow(1-math.Exp(exponent), float64(bf.hashes))
}

// Clear resets the bloom filter.
func (bf *BloomFilter) Clear() {
	for i := range bf.bits {
		bf.bits[i] = false
	}
	bf.count = 0
}

// Union combines two bloom filters (must have same size and hash count).
func (bf *BloomFilter) Union(other *BloomFilter) (*BloomFilter, bool) {
	if bf.size != other.size || bf.hashes != other.hashes {
		return nil, false
	}

	result := NewBloomFilter(bf.size, bf.hashes)
	for i := 0; i < bf.size; i++ {
		result.bits[i] = bf.bits[i] || other.bits[i]
	}
	result.count = bf.count + other.count // Approximation
	return result, true
}

// Intersection creates intersection of two bloom filters.
func (bf *BloomFilter) Intersection(other *BloomFilter) (*BloomFilter, bool) {
	if bf.size != other.size || bf.hashes != other.hashes {
		return nil, false
	}

	result := NewBloomFilter(bf.size, bf.hashes)
	for i := 0; i < bf.size; i++ {
		result.bits[i] = bf.bits[i] && other.bits[i]
	}
	return result, true
}

func (bf *BloomFilter) hash(item string, seed int) int {
	h := fnv.New64a()
	h.Write([]byte(item))
	h.Write([]byte{byte(seed)})
	return int(h.Sum64() % uint64(bf.size))
}

// CountingBloomFilter allows removal of items.
type CountingBloomFilter struct {
	counters []int
	size     int
	hashes   int
	count    int
}

// NewCountingBloomFilter creates a counting bloom filter.
func NewCountingBloomFilter(size, hashes int) *CountingBloomFilter {
	if size < 1 {
		size = 1
	}
	if hashes < 1 {
		hashes = 1
	}
	return &CountingBloomFilter{
		counters: make([]int, size),
		size:     size,
		hashes:   hashes,
	}
}

// Add adds an item to the counting bloom filter.
func (cbf *CountingBloomFilter) Add(item string) {
	for i := 0; i < cbf.hashes; i++ {
		index := cbf.hash(item, i)
		cbf.counters[index]++
	}
	cbf.count++
}

// Remove removes an item from the counting bloom filter.
func (cbf *CountingBloomFilter) Remove(item string) bool {
	if !cbf.Contains(item) {
		return false
	}

	for i := 0; i < cbf.hashes; i++ {
		index := cbf.hash(item, i)
		if cbf.counters[index] > 0 {
			cbf.counters[index]--
		}
	}
	cbf.count--
	return true
}

// Contains checks if an item might be in the set.
func (cbf *CountingBloomFilter) Contains(item string) bool {
	for i := 0; i < cbf.hashes; i++ {
		index := cbf.hash(item, i)
		if cbf.counters[index] == 0 {
			return false
		}
	}
	return true
}

// Count returns the number of items added.
func (cbf *CountingBloomFilter) Count() int {
	return cbf.count
}

// Clear resets the counting bloom filter.
func (cbf *CountingBloomFilter) Clear() {
	for i := range cbf.counters {
		cbf.counters[i] = 0
	}
	cbf.count = 0
}

func (cbf *CountingBloomFilter) hash(item string, seed int) int {
	h := fnv.New64a()
	h.Write([]byte(item))
	h.Write([]byte{byte(seed)})
	return int(h.Sum64() % uint64(cbf.size))
}
