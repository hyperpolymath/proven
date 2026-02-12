// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe binary heap with bounded capacity operations.
//!
//! Provides min-heap and max-heap implementations with compile-time bounded
//! capacity. All operations are designed to fail safely without panics or
//! undefined behavior, returning errors instead of crashing on overflow.

const std = @import("std");

/// Heap-related error types
pub const HeapError = error{
    /// Heap has reached maximum capacity
    HeapFull,
    /// Heap is empty
    HeapEmpty,
    /// Index out of bounds
    IndexOutOfBounds,
};

/// Order direction for heap comparison
pub const HeapOrder = enum {
    /// Minimum element at root (min-heap)
    min,
    /// Maximum element at root (max-heap)
    max,
};

/// A bounded binary heap with safe operations.
///
/// Guarantees:
/// - Never exceeds compile-time capacity
/// - All operations return errors instead of panicking
/// - O(log n) insertion and removal
/// - O(1) peek at root element
pub fn BoundedHeap(comptime T: type, comptime capacity: usize, comptime order: HeapOrder) type {
    return struct {
        const Self = @This();

        data: [capacity]T = undefined,
        len: usize = 0,

        /// Create a new empty heap
        pub fn init() Self {
            return .{};
        }

        /// Get the number of elements in the heap
        pub fn length(self: *const Self) usize {
            return self.len;
        }

        /// Check if the heap is empty
        pub fn isEmpty(self: *const Self) bool {
            return self.len == 0;
        }

        /// Check if the heap is full
        pub fn isFull(self: *const Self) bool {
            return self.len >= capacity;
        }

        /// Get the remaining capacity
        pub fn remaining(self: *const Self) usize {
            return capacity - self.len;
        }

        /// Get the maximum capacity
        pub fn getCapacity(self: *const Self) usize {
            _ = self;
            return capacity;
        }

        /// Push an element onto the heap (fails if full)
        pub fn push(self: *Self, value: T) HeapError!void {
            if (self.isFull()) return error.HeapFull;

            self.data[self.len] = value;
            self.len += 1;
            self.siftUp(self.len - 1);
        }

        /// Push an element, dropping the root if full and new element should replace it
        /// For min-heap: drops minimum if new element is larger
        /// For max-heap: drops maximum if new element is smaller
        pub fn pushBounded(self: *Self, value: T) void {
            if (!self.isFull()) {
                self.push(value) catch unreachable;
                return;
            }

            // Check if we should replace the root
            // For min-heap (tracking top-K largest): replace if value > root
            // For max-heap (tracking top-K smallest): replace if value < root
            const should_replace = switch (order) {
                .min => self.compare(self.data[0], value), // root < value means value > root
                .max => self.compare(value, self.data[0]), // value > root means value < root (inverted)
            };

            if (should_replace) {
                self.data[0] = value;
                self.siftDown(0);
            }
        }

        /// Pop the root element (fails if empty)
        pub fn pop(self: *Self) HeapError!T {
            if (self.isEmpty()) return error.HeapEmpty;

            const result = self.data[0];
            self.len -= 1;

            if (self.len > 0) {
                self.data[0] = self.data[self.len];
                self.siftDown(0);
            }

            return result;
        }

        /// Peek at the root element without removing (fails if empty)
        pub fn peek(self: *const Self) HeapError!T {
            if (self.isEmpty()) return error.HeapEmpty;
            return self.data[0];
        }

        /// Peek at a specific index (fails if out of bounds)
        pub fn peekAt(self: *const Self, index: usize) HeapError!T {
            if (index >= self.len) return error.IndexOutOfBounds;
            return self.data[index];
        }

        /// Remove and return root, then push new value (fails if empty)
        /// More efficient than separate pop + push
        pub fn replace(self: *Self, value: T) HeapError!T {
            if (self.isEmpty()) return error.HeapEmpty;

            const result = self.data[0];
            self.data[0] = value;
            self.siftDown(0);

            return result;
        }

        /// Clear all elements from the heap
        pub fn clear(self: *Self) void {
            self.len = 0;
        }

        /// Get all elements as a slice (not in heap order)
        pub fn items(self: *const Self) []const T {
            return self.data[0..self.len];
        }

        /// Convert heap to sorted slice (destructive - empties the heap)
        pub fn toSortedSlice(self: *Self, buffer: []T) HeapError![]const T {
            if (buffer.len < self.len) return error.IndexOutOfBounds;

            var idx: usize = 0;
            while (!self.isEmpty()) {
                buffer[idx] = self.pop() catch unreachable;
                idx += 1;
            }

            return buffer[0..idx];
        }

        /// Check heap invariant (for debugging/verification)
        pub fn isValid(self: *const Self) bool {
            if (self.len <= 1) return true;

            var i: usize = 0;
            while (i < (self.len - 1) / 2 + 1) : (i += 1) {
                const left = 2 * i + 1;
                const right = 2 * i + 2;

                if (left < self.len and self.compare(self.data[left], self.data[i])) {
                    return false;
                }
                if (right < self.len and self.compare(self.data[right], self.data[i])) {
                    return false;
                }
            }

            return true;
        }

        /// Compare function based on heap order
        /// Returns true if a should be higher in heap than b
        fn compare(self: *const Self, a: T, b: T) bool {
            _ = self;
            return switch (order) {
                .min => a < b,
                .max => a > b,
            };
        }

        fn siftUp(self: *Self, start_index: usize) void {
            var index = start_index;
            while (index > 0) {
                const parent_index = (index - 1) / 2;
                if (!self.compare(self.data[index], self.data[parent_index])) break;

                const temp = self.data[index];
                self.data[index] = self.data[parent_index];
                self.data[parent_index] = temp;
                index = parent_index;
            }
        }

        fn siftDown(self: *Self, start_index: usize) void {
            var index = start_index;
            while (true) {
                const left_index = 2 * index + 1;
                const right_index = 2 * index + 2;
                var target_index = index;

                if (left_index < self.len and self.compare(self.data[left_index], self.data[target_index])) {
                    target_index = left_index;
                }
                if (right_index < self.len and self.compare(self.data[right_index], self.data[target_index])) {
                    target_index = right_index;
                }

                if (target_index == index) break;

                const temp = self.data[index];
                self.data[index] = self.data[target_index];
                self.data[target_index] = temp;
                index = target_index;
            }
        }
    };
}

/// Create a min-heap with the specified element type and capacity
pub fn MinHeap(comptime T: type, comptime capacity: usize) type {
    return BoundedHeap(T, capacity, .min);
}

/// Create a max-heap with the specified element type and capacity
pub fn MaxHeap(comptime T: type, comptime capacity: usize) type {
    return BoundedHeap(T, capacity, .max);
}

/// A bounded heap with custom comparison function
pub fn BoundedHeapWithComparator(
    comptime T: type,
    comptime capacity: usize,
    comptime compareFn: fn (T, T) bool,
) type {
    return struct {
        const Self = @This();

        data: [capacity]T = undefined,
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

        pub fn push(self: *Self, value: T) HeapError!void {
            if (self.isFull()) return error.HeapFull;

            self.data[self.len] = value;
            self.len += 1;
            self.siftUp(self.len - 1);
        }

        pub fn pop(self: *Self) HeapError!T {
            if (self.isEmpty()) return error.HeapEmpty;

            const result = self.data[0];
            self.len -= 1;

            if (self.len > 0) {
                self.data[0] = self.data[self.len];
                self.siftDown(0);
            }

            return result;
        }

        pub fn peek(self: *const Self) HeapError!T {
            if (self.isEmpty()) return error.HeapEmpty;
            return self.data[0];
        }

        pub fn clear(self: *Self) void {
            self.len = 0;
        }

        fn siftUp(self: *Self, start_index: usize) void {
            var index = start_index;
            while (index > 0) {
                const parent_index = (index - 1) / 2;
                if (!compareFn(self.data[index], self.data[parent_index])) break;

                const temp = self.data[index];
                self.data[index] = self.data[parent_index];
                self.data[parent_index] = temp;
                index = parent_index;
            }
        }

        fn siftDown(self: *Self, start_index: usize) void {
            var index = start_index;
            while (true) {
                const left_index = 2 * index + 1;
                const right_index = 2 * index + 2;
                var target_index = index;

                if (left_index < self.len and compareFn(self.data[left_index], self.data[target_index])) {
                    target_index = left_index;
                }
                if (right_index < self.len and compareFn(self.data[right_index], self.data[target_index])) {
                    target_index = right_index;
                }

                if (target_index == index) break;

                const temp = self.data[index];
                self.data[index] = self.data[target_index];
                self.data[target_index] = temp;
                index = target_index;
            }
        }
    };
}

/// Heap-based top-K tracker
pub fn TopK(comptime T: type, comptime k: usize) type {
    return struct {
        const Self = @This();

        // Use min-heap to track top-K largest elements
        // The smallest of the top-K is at the root
        heap: MinHeap(T, k) = MinHeap(T, k).init(),

        pub fn init() Self {
            return .{};
        }

        /// Add a value to the tracker
        pub fn add(self: *Self, value: T) void {
            self.heap.pushBounded(value);
        }

        /// Get the current top-K values (not sorted)
        pub fn getTopK(self: *const Self) []const T {
            return self.heap.items();
        }

        /// Get the minimum value among the top-K (threshold for inclusion)
        pub fn threshold(self: *const Self) HeapError!T {
            return self.heap.peek();
        }

        /// Check if a value would be included in top-K
        pub fn wouldInclude(self: *const Self, value: T) bool {
            if (!self.heap.isFull()) return true;
            const min_value = self.heap.peek() catch return true;
            return value > min_value;
        }

        /// Get the number of tracked elements
        pub fn count(self: *const Self) usize {
            return self.heap.length();
        }

        /// Clear all tracked elements
        pub fn clear(self: *Self) void {
            self.heap.clear();
        }
    };
}

// =============================================================================
// Tests
// =============================================================================

test "MinHeap basic operations" {
    var heap = MinHeap(u32, 10).init();

    try heap.push(5);
    try heap.push(3);
    try heap.push(7);
    try heap.push(1);

    try std.testing.expectEqual(@as(usize, 4), heap.length());
    try std.testing.expectEqual(@as(u32, 1), try heap.peek());
    try std.testing.expect(heap.isValid());

    try std.testing.expectEqual(@as(u32, 1), try heap.pop());
    try std.testing.expectEqual(@as(u32, 3), try heap.pop());
    try std.testing.expectEqual(@as(u32, 5), try heap.pop());
    try std.testing.expectEqual(@as(u32, 7), try heap.pop());

    try std.testing.expect(heap.isEmpty());
}

test "MaxHeap basic operations" {
    var heap = MaxHeap(u32, 10).init();

    try heap.push(5);
    try heap.push(3);
    try heap.push(7);
    try heap.push(1);

    try std.testing.expectEqual(@as(u32, 7), try heap.peek());
    try std.testing.expect(heap.isValid());

    try std.testing.expectEqual(@as(u32, 7), try heap.pop());
    try std.testing.expectEqual(@as(u32, 5), try heap.pop());
}

test "heap capacity limits" {
    var heap = MinHeap(u32, 3).init();

    try heap.push(1);
    try heap.push(2);
    try heap.push(3);

    try std.testing.expect(heap.isFull());
    try std.testing.expectError(error.HeapFull, heap.push(4));
}

test "heap empty errors" {
    var heap = MinHeap(u32, 3).init();

    try std.testing.expect(heap.isEmpty());
    try std.testing.expectError(error.HeapEmpty, heap.pop());
    try std.testing.expectError(error.HeapEmpty, heap.peek());
}

test "pushBounded drops correctly" {
    var heap = MinHeap(u32, 3).init();

    heap.pushBounded(5);
    heap.pushBounded(3);
    heap.pushBounded(7);

    try std.testing.expect(heap.isFull());
    try std.testing.expectEqual(@as(u32, 3), try heap.peek());

    // Push larger value - should drop 3 (min)
    heap.pushBounded(10);
    try std.testing.expectEqual(@as(u32, 5), try heap.peek());

    // Push smaller value - should not change heap
    heap.pushBounded(1);
    try std.testing.expectEqual(@as(u32, 5), try heap.peek());
}

test "heap replace" {
    var heap = MinHeap(u32, 5).init();

    try heap.push(3);
    try heap.push(1);
    try heap.push(4);

    const old = try heap.replace(2);
    try std.testing.expectEqual(@as(u32, 1), old);
    try std.testing.expectEqual(@as(u32, 2), try heap.peek());
}

test "heap toSortedSlice" {
    var heap = MinHeap(u32, 5).init();

    try heap.push(5);
    try heap.push(2);
    try heap.push(8);
    try heap.push(1);

    var buffer: [5]u32 = undefined;
    const sorted = try heap.toSortedSlice(&buffer);

    try std.testing.expectEqualSlices(u32, &[_]u32{ 1, 2, 5, 8 }, sorted);
    try std.testing.expect(heap.isEmpty());
}

test "TopK tracker" {
    var tracker = TopK(u32, 3).init();

    tracker.add(5);
    tracker.add(2);
    tracker.add(8);
    tracker.add(1);
    tracker.add(9);
    tracker.add(3);

    try std.testing.expectEqual(@as(usize, 3), tracker.count());

    // Top 3 should be 5, 8, 9 (threshold is 5)
    try std.testing.expectEqual(@as(u32, 5), try tracker.threshold());
    try std.testing.expect(tracker.wouldInclude(10));
    try std.testing.expect(!tracker.wouldInclude(4));
}

test "custom comparator heap" {
    const Task = struct {
        priority: u32,
        id: u32,
    };

    const compareFn = struct {
        fn call(a: Task, b: Task) bool {
            return a.priority > b.priority; // Higher priority first
        }
    }.call;

    var heap = BoundedHeapWithComparator(Task, 5, compareFn).init();

    try heap.push(.{ .priority = 1, .id = 1 });
    try heap.push(.{ .priority = 5, .id = 2 });
    try heap.push(.{ .priority = 3, .id = 3 });

    const top = try heap.pop();
    try std.testing.expectEqual(@as(u32, 5), top.priority);
    try std.testing.expectEqual(@as(u32, 2), top.id);
}
