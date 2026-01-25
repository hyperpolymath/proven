// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe Bloom filter implementation.
//!
//! Provides probabilistic set membership testing with tunable
//! false positive rates.

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

/// Bloom filter with configurable size and hash count.
#[derive(Debug, Clone)]
pub struct BloomFilter {
    bits: Vec<bool>,
    size: usize,
    num_hashes: usize,
}

impl BloomFilter {
    /// Create a new Bloom filter with the given size and hash count.
    pub fn new(size: usize, num_hashes: usize) -> Self {
        Self {
            bits: vec![false; size],
            size,
            num_hashes,
        }
    }

    /// Create with optimal parameters for expected items and false positive rate.
    pub fn with_rate(expected_items: usize, false_positive_rate: f64) -> Self {
        let size = optimal_size(expected_items, false_positive_rate);
        let num_hashes = optimal_hashes(size, expected_items);
        Self::new(size, num_hashes)
    }

    /// Insert an item into the filter.
    pub fn insert<T: Hash>(&mut self, item: &T) {
        for i in 0..self.num_hashes {
            let hash = self.compute_hash(item, i);
            self.bits[hash % self.size] = true;
        }
    }

    /// Check if an item might be in the filter.
    /// Returns false if definitely not present, true if possibly present.
    pub fn contains<T: Hash>(&self, item: &T) -> bool {
        for i in 0..self.num_hashes {
            let hash = self.compute_hash(item, i);
            if !self.bits[hash % self.size] {
                return false;
            }
        }
        true
    }

    /// Count set bits.
    pub fn count_ones(&self) -> usize {
        self.bits.iter().filter(|&&b| b).count()
    }

    /// Get fill ratio.
    pub fn fill_ratio(&self) -> f64 {
        self.count_ones() as f64 / self.size as f64
    }

    /// Estimate false positive rate.
    pub fn estimated_fpr(&self) -> f64 {
        self.fill_ratio().powi(self.num_hashes as i32)
    }

    /// Clear the filter.
    pub fn clear(&mut self) {
        self.bits.fill(false);
    }

    /// Union of two filters (must be same size).
    pub fn union_with(&mut self, other: &Self) {
        if self.size == other.size {
            for (a, b) in self.bits.iter_mut().zip(other.bits.iter()) {
                *a = *a || *b;
            }
        }
    }

    /// Intersection of two filters (must be same size).
    pub fn intersect_with(&mut self, other: &Self) {
        if self.size == other.size {
            for (a, b) in self.bits.iter_mut().zip(other.bits.iter()) {
                *a = *a && *b;
            }
        }
    }

    fn compute_hash<T: Hash>(&self, item: &T, seed: usize) -> usize {
        let mut hasher1 = DefaultHasher::new();
        item.hash(&mut hasher1);
        let h1 = hasher1.finish();

        let mut hasher2 = DefaultHasher::new();
        seed.hash(&mut hasher2);
        let h2 = hasher2.finish();

        (h1.wrapping_add(seed as u64 * h2)) as usize
    }
}

/// Calculate optimal size for given parameters.
pub fn optimal_size(expected_items: usize, false_positive_rate: f64) -> usize {
    let n = expected_items as f64;
    let p = false_positive_rate;
    let ln2_sq = 0.4804530139182014; // ln(2)^2
    let m = -(n * p.ln()) / ln2_sq;
    m.ceil() as usize
}

/// Calculate optimal hash count.
pub fn optimal_hashes(filter_size: usize, expected_items: usize) -> usize {
    let m = filter_size as f64;
    let n = expected_items as f64;
    let k = (m / n) * 0.6931471805599453; // ln(2)
    k.ceil().max(1.0) as usize
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bloom_filter() {
        let mut bf = BloomFilter::new(1024, 3);
        bf.insert(&"hello");
        bf.insert(&"world");

        assert!(bf.contains(&"hello"));
        assert!(bf.contains(&"world"));
        assert!(!bf.contains(&"definitely not present xyz123"));
    }

    #[test]
    fn test_optimal_size() {
        let size = optimal_size(1000, 0.01);
        assert!(size > 9000 && size < 10000);
    }

    #[test]
    fn test_with_rate() {
        let bf = BloomFilter::with_rate(1000, 0.01);
        assert!(bf.size > 0);
        assert!(bf.num_hashes > 0);
    }
}
