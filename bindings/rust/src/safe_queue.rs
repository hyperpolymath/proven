// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe bounded queue operations.
//!
//! Provides FIFO queues and priority queues with bounded capacity
//! and safe operations.

use crate::{Error, Result};

/// Bounded FIFO queue with safe operations.
#[derive(Debug, Clone)]
pub struct BoundedQueue<T> {
    data: Vec<Option<T>>,
    head: usize,
    tail: usize,
    len: usize,
    capacity: usize,
}

impl<T: Clone> BoundedQueue<T> {
    /// Create a new bounded queue with the given capacity.
    pub fn new(capacity: usize) -> Self {
        Self {
            data: vec![None; capacity],
            head: 0,
            tail: 0,
            len: 0,
            capacity,
        }
    }

    /// Get current length.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Check if queue is empty.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Check if queue is full.
    pub fn is_full(&self) -> bool {
        self.len >= self.capacity
    }

    /// Get remaining capacity.
    pub fn remaining(&self) -> usize {
        self.capacity.saturating_sub(self.len)
    }

    /// Enqueue an element (fails if full).
    pub fn enqueue(&mut self, value: T) -> Result<()> {
        if self.is_full() {
            return Err(Error::OutOfRange("Queue full".into()));
        }
        self.data[self.tail] = Some(value);
        self.tail = (self.tail + 1) % self.capacity;
        self.len += 1;
        Ok(())
    }

    /// Enqueue, dropping oldest if full.
    pub fn enqueue_drop_oldest(&mut self, value: T) {
        if self.is_full() {
            let _ = self.dequeue();
        }
        let _ = self.enqueue(value);
    }

    /// Dequeue an element (fails if empty).
    pub fn dequeue(&mut self) -> Result<T> {
        if self.is_empty() {
            return Err(Error::OutOfRange("Queue empty".into()));
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
            return Err(Error::OutOfRange("Queue empty".into()));
        }
        self.data[self.head]
            .as_ref()
            .ok_or_else(|| Error::OutOfRange("Invalid state".into()))
    }

    /// Clear all elements.
    pub fn clear(&mut self) {
        self.data.iter_mut().for_each(|x| *x = None);
        self.head = 0;
        self.tail = 0;
        self.len = 0;
    }
}

/// Priority queue (min-heap).
#[derive(Debug, Clone)]
pub struct PriorityQueue<T> {
    data: Vec<T>,
    capacity: usize,
}

impl<T: Clone + Ord> PriorityQueue<T> {
    /// Create a new priority queue with the given capacity.
    pub fn new(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
            capacity,
        }
    }

    /// Check if queue is empty.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Check if queue is full.
    pub fn is_full(&self) -> bool {
        self.data.len() >= self.capacity
    }

    /// Get current length.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Push an element.
    pub fn push(&mut self, value: T) -> Result<()> {
        if self.is_full() {
            return Err(Error::OutOfRange("Queue full".into()));
        }
        self.data.push(value);
        self.sift_up(self.data.len() - 1);
        Ok(())
    }

    /// Pop the minimum element.
    pub fn pop(&mut self) -> Result<T> {
        if self.is_empty() {
            return Err(Error::OutOfRange("Queue empty".into()));
        }
        let result = self.data.swap_remove(0);
        if !self.data.is_empty() {
            self.sift_down(0);
        }
        Ok(result)
    }

    /// Peek at minimum without removing.
    pub fn peek(&self) -> Result<&T> {
        self.data
            .first()
            .ok_or_else(|| Error::OutOfRange("Queue empty".into()))
    }

    fn sift_up(&mut self, mut idx: usize) {
        while idx > 0 {
            let parent = (idx - 1) / 2;
            if self.data[idx] >= self.data[parent] {
                break;
            }
            self.data.swap(idx, parent);
            idx = parent;
        }
    }

    fn sift_down(&mut self, mut idx: usize) {
        loop {
            let left = 2 * idx + 1;
            let right = 2 * idx + 2;
            let mut smallest = idx;

            if left < self.data.len() && self.data[left] < self.data[smallest] {
                smallest = left;
            }
            if right < self.data.len() && self.data[right] < self.data[smallest] {
                smallest = right;
            }
            if smallest == idx {
                break;
            }
            self.data.swap(idx, smallest);
            idx = smallest;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bounded_queue() {
        let mut q: BoundedQueue<u32> = BoundedQueue::new(3);
        assert!(q.enqueue(1).is_ok());
        assert!(q.enqueue(2).is_ok());
        assert_eq!(q.dequeue().unwrap(), 1);
        assert_eq!(q.len(), 1);
    }

    #[test]
    fn test_priority_queue() {
        let mut pq: PriorityQueue<u32> = PriorityQueue::new(10);
        pq.push(3).unwrap();
        pq.push(1).unwrap();
        pq.push(2).unwrap();
        assert_eq!(pq.pop().unwrap(), 1);
        assert_eq!(pq.pop().unwrap(), 2);
        assert_eq!(pq.pop().unwrap(), 3);
    }
}
