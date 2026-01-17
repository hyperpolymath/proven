// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe buffer operations with bounds checking.

const std = @import("std");

/// Error types for buffer operations.
pub const BufferError = error{
    OutOfBounds,
    BufferFull,
    BufferEmpty,
    AllocationFailed,
};

/// A bounded buffer with safe operations.
pub fn BoundedBuffer(comptime T: type, comptime capacity: usize) type {
    return struct {
        const Self = @This();

        data: [capacity]T = undefined,
        len: usize = 0,

        /// Initialize an empty buffer.
        pub fn init() Self {
            return .{};
        }

        /// Get current length.
        pub fn length(self: *const Self) usize {
            return self.len;
        }

        /// Check if buffer is empty.
        pub fn isEmpty(self: *const Self) bool {
            return self.len == 0;
        }

        /// Check if buffer is full.
        pub fn isFull(self: *const Self) bool {
            return self.len >= capacity;
        }

        /// Get remaining capacity.
        pub fn remaining(self: *const Self) usize {
            return capacity - self.len;
        }

        /// Push an element (fails if full).
        pub fn push(self: *Self, value: T) BufferError!void {
            if (self.isFull()) return error.BufferFull;
            self.data[self.len] = value;
            self.len += 1;
        }

        /// Pop an element (fails if empty).
        pub fn pop(self: *Self) BufferError!T {
            if (self.isEmpty()) return error.BufferEmpty;
            self.len -= 1;
            return self.data[self.len];
        }

        /// Get element at index (fails if out of bounds).
        pub fn get(self: *const Self, index: usize) BufferError!T {
            if (index >= self.len) return error.OutOfBounds;
            return self.data[index];
        }

        /// Set element at index (fails if out of bounds).
        pub fn set(self: *Self, index: usize, value: T) BufferError!void {
            if (index >= self.len) return error.OutOfBounds;
            self.data[index] = value;
        }

        /// Clear all elements.
        pub fn clear(self: *Self) void {
            self.len = 0;
        }

        /// Get slice of valid data.
        pub fn slice(self: *const Self) []const T {
            return self.data[0..self.len];
        }
    };
}

/// Ring buffer for FIFO operations.
pub fn RingBuffer(comptime T: type, comptime capacity: usize) type {
    return struct {
        const Self = @This();

        data: [capacity]T = undefined,
        head: usize = 0,
        tail: usize = 0,
        len: usize = 0,

        pub fn init() Self {
            return .{};
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.len == 0;
        }

        pub fn isFull(self: *const Self) bool {
            return self.len >= capacity;
        }

        pub fn enqueue(self: *Self, value: T) BufferError!void {
            if (self.isFull()) return error.BufferFull;
            self.data[self.tail] = value;
            self.tail = (self.tail + 1) % capacity;
            self.len += 1;
        }

        pub fn dequeue(self: *Self) BufferError!T {
            if (self.isEmpty()) return error.BufferEmpty;
            const value = self.data[self.head];
            self.head = (self.head + 1) % capacity;
            self.len -= 1;
            return value;
        }

        pub fn peek(self: *const Self) BufferError!T {
            if (self.isEmpty()) return error.BufferEmpty;
            return self.data[self.head];
        }
    };
}

test "BoundedBuffer" {
    var buf = BoundedBuffer(u32, 4).init();
    try buf.push(1);
    try buf.push(2);
    try std.testing.expectEqual(@as(usize, 2), buf.length());
    try std.testing.expectEqual(@as(u32, 2), try buf.pop());
}

test "RingBuffer" {
    var ring = RingBuffer(u32, 3).init();
    try ring.enqueue(1);
    try ring.enqueue(2);
    try std.testing.expectEqual(@as(u32, 1), try ring.dequeue());
}
