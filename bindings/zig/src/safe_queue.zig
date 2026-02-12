// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe bounded queue operations.

const std = @import("std");

pub const QueueError = error{
    QueueFull,
    QueueEmpty,
};

/// Bounded FIFO queue with safe operations.
pub fn BoundedQueue(comptime T: type, comptime capacity: usize) type {
    return struct {
        const Self = @This();

        data: [capacity]T = undefined,
        head: usize = 0,
        tail: usize = 0,
        len: usize = 0,

        pub fn init() Self {
            return .{};
        }

        pub fn length(self: *const Self) usize {
            return self.len;
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.len == 0;
        }

        pub fn isFull(self: *const Self) bool {
            return self.len >= capacity;
        }

        pub fn remaining(self: *const Self) usize {
            return capacity - self.len;
        }

        /// Enqueue an element (fails if full).
        pub fn enqueue(self: *Self, value: T) QueueError!void {
            if (self.isFull()) return error.QueueFull;
            self.data[self.tail] = value;
            self.tail = (self.tail + 1) % capacity;
            self.len += 1;
        }

        /// Enqueue, dropping oldest if full.
        pub fn enqueueDropOldest(self: *Self, value: T) void {
            if (self.isFull()) {
                _ = self.dequeue() catch {};
            }
            self.enqueue(value) catch unreachable;
        }

        /// Dequeue an element (fails if empty).
        pub fn dequeue(self: *Self) QueueError!T {
            if (self.isEmpty()) return error.QueueEmpty;
            const value = self.data[self.head];
            self.head = (self.head + 1) % capacity;
            self.len -= 1;
            return value;
        }

        /// Peek at front without removing.
        pub fn peek(self: *const Self) QueueError!T {
            if (self.isEmpty()) return error.QueueEmpty;
            return self.data[self.head];
        }

        /// Clear all elements.
        pub fn clear(self: *Self) void {
            self.head = 0;
            self.tail = 0;
            self.len = 0;
        }
    };
}

/// Priority queue (min-heap).
pub fn PriorityQueue(comptime T: type, comptime capacity: usize, comptime lessThan: fn (T, T) bool) type {
    return struct {
        const Self = @This();

        data: [capacity]T = undefined,
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

        pub fn push(self: *Self, value: T) QueueError!void {
            if (self.isFull()) return error.QueueFull;
            self.data[self.len] = value;
            self.len += 1;
            self.siftUp(self.len - 1);
        }

        pub fn pop(self: *Self) QueueError!T {
            if (self.isEmpty()) return error.QueueEmpty;
            const result = self.data[0];
            self.len -= 1;
            if (self.len > 0) {
                self.data[0] = self.data[self.len];
                self.siftDown(0);
            }
            return result;
        }

        pub fn peek(self: *const Self) QueueError!T {
            if (self.isEmpty()) return error.QueueEmpty;
            return self.data[0];
        }

        fn siftUp(self: *Self, idx: usize) void {
            var i = idx;
            while (i > 0) {
                const parent = (i - 1) / 2;
                if (!lessThan(self.data[i], self.data[parent])) break;
                const tmp = self.data[i];
                self.data[i] = self.data[parent];
                self.data[parent] = tmp;
                i = parent;
            }
        }

        fn siftDown(self: *Self, idx: usize) void {
            var i = idx;
            while (true) {
                const left = 2 * i + 1;
                const right = 2 * i + 2;
                var smallest = i;

                if (left < self.len and lessThan(self.data[left], self.data[smallest])) {
                    smallest = left;
                }
                if (right < self.len and lessThan(self.data[right], self.data[smallest])) {
                    smallest = right;
                }
                if (smallest == i) break;

                const tmp = self.data[i];
                self.data[i] = self.data[smallest];
                self.data[smallest] = tmp;
                i = smallest;
            }
        }
    };
}

test "BoundedQueue" {
    var q = BoundedQueue(u32, 3).init();
    try q.enqueue(1);
    try q.enqueue(2);
    try std.testing.expectEqual(@as(u32, 1), try q.dequeue());
    try std.testing.expectEqual(@as(usize, 1), q.length());
}
