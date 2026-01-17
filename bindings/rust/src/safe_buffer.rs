// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe buffer operations with bounds checking.
//!
//! Provides bounded buffers and ring buffers with safe operations
//! that cannot overflow or underflow.

use crate::{Error, Result};

/// A bounded buffer with safe operations.
#[derive(Debug, Clone)]
pub struct BoundedBuffer<T> {
    data: Vec<T>,
    capacity: usize,
}

impl<T: Clone> BoundedBuffer<T> {
    /// Create a new bounded buffer with the given capacity.
    pub fn new(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
            capacity,
        }
    }

    /// Get current length.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Check if buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Check if buffer is full.
    pub fn is_full(&self) -> bool {
        self.data.len() >= self.capacity
    }

    /// Get remaining capacity.
    pub fn remaining(&self) -> usize {
        self.capacity.saturating_sub(self.data.len())
    }

    /// Push an element (fails if full).
    pub fn push(&mut self, value: T) -> Result<()> {
        if self.is_full() {
            return Err(Error::OutOfRange("Buffer full".into()));
        }
        self.data.push(value);
        Ok(())
    }

    /// Pop an element (fails if empty).
    pub fn pop(&mut self) -> Result<T> {
        self.data
            .pop()
            .ok_or_else(|| Error::OutOfRange("Buffer empty".into()))
    }

    /// Get element at index (fails if out of bounds).
    pub fn get(&self, index: usize) -> Result<&T> {
        self.data
            .get(index)
            .ok_or_else(|| Error::OutOfRange("Index out of bounds".into()))
    }

    /// Set element at index (fails if out of bounds).
    pub fn set(&mut self, index: usize, value: T) -> Result<()> {
        if index >= self.data.len() {
            return Err(Error::OutOfRange("Index out of bounds".into()));
        }
        self.data[index] = value;
        Ok(())
    }

    /// Clear all elements.
    pub fn clear(&mut self) {
        self.data.clear();
    }

    /// Get slice of valid data.
    pub fn as_slice(&self) -> &[T] {
        &self.data
    }
}

/// Ring buffer for FIFO operations.
#[derive(Debug, Clone)]
pub struct RingBuffer<T> {
    data: Vec<Option<T>>,
    head: usize,
    tail: usize,
    len: usize,
    capacity: usize,
}

impl<T: Clone> RingBuffer<T> {
    /// Create a new ring buffer with the given capacity.
    pub fn new(capacity: usize) -> Self {
        Self {
            data: vec![None; capacity],
            head: 0,
            tail: 0,
            len: 0,
            capacity,
        }
    }

    /// Check if buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Check if buffer is full.
    pub fn is_full(&self) -> bool {
        self.len >= self.capacity
    }

    /// Get current length.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Enqueue an element (fails if full).
    pub fn enqueue(&mut self, value: T) -> Result<()> {
        if self.is_full() {
            return Err(Error::OutOfRange("Ring buffer full".into()));
        }
        self.data[self.tail] = Some(value);
        self.tail = (self.tail + 1) % self.capacity;
        self.len += 1;
        Ok(())
    }

    /// Dequeue an element (fails if empty).
    pub fn dequeue(&mut self) -> Result<T> {
        if self.is_empty() {
            return Err(Error::OutOfRange("Ring buffer empty".into()));
        }
        let value = self.data[self.head]
            .take()
            .ok_or_else(|| Error::OutOfRange("Invalid state".into()))?;
        self.head = (self.head + 1) % self.capacity;
        self.len -= 1;
        Ok(value)
    }

    /// Peek at front without removing.
    pub fn peek(&self) -> Result<&T> {
        if self.is_empty() {
            return Err(Error::OutOfRange("Ring buffer empty".into()));
        }
        self.data[self.head]
            .as_ref()
            .ok_or_else(|| Error::OutOfRange("Invalid state".into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bounded_buffer() {
        let mut buf: BoundedBuffer<u32> = BoundedBuffer::new(4);
        assert!(buf.push(1).is_ok());
        assert!(buf.push(2).is_ok());
        assert_eq!(buf.len(), 2);
        assert_eq!(buf.pop().unwrap(), 2);
    }

    #[test]
    fn test_ring_buffer() {
        let mut ring: RingBuffer<u32> = RingBuffer::new(3);
        assert!(ring.enqueue(1).is_ok());
        assert!(ring.enqueue(2).is_ok());
        assert_eq!(ring.dequeue().unwrap(), 1);
    }
}
