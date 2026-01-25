// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe monotonic sequence generation.
//!
//! Provides monotonically increasing counters and timestamps
//! that cannot go backwards.

use std::sync::atomic::{AtomicU64, Ordering};

/// Monotonically increasing counter.
#[derive(Debug)]
pub struct MonotonicCounter {
    value: AtomicU64,
}

impl MonotonicCounter {
    /// Create a new counter starting at 0.
    pub fn new() -> Self {
        Self {
            value: AtomicU64::new(0),
        }
    }

    /// Create with an initial value.
    pub fn with_initial(initial: u64) -> Self {
        Self {
            value: AtomicU64::new(initial),
        }
    }

    /// Get the current value.
    pub fn get(&self) -> u64 {
        self.value.load(Ordering::SeqCst)
    }

    /// Get and increment (returns the value before increment).
    pub fn next(&self) -> u64 {
        self.value.fetch_add(1, Ordering::SeqCst)
    }

    /// Increment by a specific amount.
    pub fn advance(&self, amount: u64) -> u64 {
        self.value.fetch_add(amount, Ordering::SeqCst)
    }

    /// Try to set a new value (only succeeds if new > current).
    pub fn try_set(&self, new_value: u64) -> bool {
        let mut current = self.value.load(Ordering::SeqCst);
        loop {
            if new_value <= current {
                return false;
            }
            match self.value.compare_exchange_weak(
                current,
                new_value,
                Ordering::SeqCst,
                Ordering::SeqCst,
            ) {
                Ok(_) => return true,
                Err(c) => current = c,
            }
        }
    }

    /// Ensure value is at least the given minimum.
    pub fn ensure_at_least(&self, min: u64) {
        let mut current = self.value.load(Ordering::SeqCst);
        while current < min {
            match self.value.compare_exchange_weak(
                current,
                min,
                Ordering::SeqCst,
                Ordering::SeqCst,
            ) {
                Ok(_) => return,
                Err(c) => current = c,
            }
        }
    }
}

impl Default for MonotonicCounter {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for MonotonicCounter {
    fn clone(&self) -> Self {
        Self::with_initial(self.get())
    }
}

/// High-water mark tracker (tracks maximum seen value).
#[derive(Debug)]
pub struct HighWaterMark {
    value: AtomicU64,
}

impl HighWaterMark {
    /// Create a new high-water mark starting at 0.
    pub fn new() -> Self {
        Self {
            value: AtomicU64::new(0),
        }
    }

    /// Get the current high-water mark.
    pub fn get(&self) -> u64 {
        self.value.load(Ordering::SeqCst)
    }

    /// Update with a new value, returning true if it's a new maximum.
    pub fn update(&self, value: u64) -> bool {
        let mut current = self.value.load(Ordering::SeqCst);
        loop {
            if value <= current {
                return false;
            }
            match self.value.compare_exchange_weak(
                current,
                value,
                Ordering::SeqCst,
                Ordering::SeqCst,
            ) {
                Ok(_) => return true,
                Err(c) => current = c,
            }
        }
    }
}

impl Default for HighWaterMark {
    fn default() -> Self {
        Self::new()
    }
}

/// Sequence ID generator with prefix.
#[derive(Debug)]
pub struct SequenceGenerator {
    counter: MonotonicCounter,
    prefix: String,
}

impl SequenceGenerator {
    /// Create a new sequence generator with a prefix.
    pub fn new(prefix: &str) -> Self {
        Self {
            counter: MonotonicCounter::new(),
            prefix: prefix.to_string(),
        }
    }

    /// Generate the next ID.
    pub fn next_id(&self) -> String {
        format!("{}-{}", self.prefix, self.counter.next())
    }

    /// Generate the next ID with padding.
    pub fn next_id_padded(&self, width: usize) -> String {
        format!("{}-{:0>width$}", self.prefix, self.counter.next(), width = width)
    }
}

/// Epoch-based ID generator (timestamp + sequence).
#[derive(Debug)]
pub struct EpochGenerator {
    epoch: u64,
    sequence: MonotonicCounter,
    last_timestamp: AtomicU64,
}

impl EpochGenerator {
    /// Create with a custom epoch.
    pub fn new(epoch: u64) -> Self {
        Self {
            epoch,
            sequence: MonotonicCounter::new(),
            last_timestamp: AtomicU64::new(0),
        }
    }

    /// Generate an ID from timestamp and sequence.
    pub fn next_id(&self, current_timestamp: u64) -> u64 {
        let adjusted = current_timestamp.saturating_sub(self.epoch);

        // Ensure monotonicity
        let last = self.last_timestamp.load(Ordering::SeqCst);
        let ts = adjusted.max(last);
        self.last_timestamp.store(ts, Ordering::SeqCst);

        // Combine timestamp and sequence
        let seq = self.sequence.next() & 0xFFFF; // 16 bits for sequence
        (ts << 16) | seq
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_monotonic_counter() {
        let counter = MonotonicCounter::new();
        assert_eq!(counter.next(), 0);
        assert_eq!(counter.next(), 1);
        assert_eq!(counter.next(), 2);
        assert_eq!(counter.get(), 3);
    }

    #[test]
    fn test_try_set() {
        let counter = MonotonicCounter::with_initial(10);
        assert!(!counter.try_set(5)); // Can't go backwards
        assert!(counter.try_set(15));
        assert_eq!(counter.get(), 15);
    }

    #[test]
    fn test_high_water_mark() {
        let hwm = HighWaterMark::new();
        assert!(hwm.update(10));
        assert!(hwm.update(20));
        assert!(!hwm.update(15)); // Not a new max
        assert_eq!(hwm.get(), 20);
    }

    #[test]
    fn test_sequence_generator() {
        let gen = SequenceGenerator::new("item");
        assert_eq!(gen.next_id(), "item-0");
        assert_eq!(gen.next_id(), "item-1");
        assert_eq!(gen.next_id_padded(5), "item-00002");
    }
}
