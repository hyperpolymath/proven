// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe bounded set operations that cannot crash.
//!
//! Provides set data structures with bounded capacity to prevent
//! unbounded memory growth. All operations are safe and return
//! explicit errors rather than panicking.

const std = @import("std");

/// Error types for set operations.
pub const SetError = error{
    SetFull,
    ElementNotFound,
    InvalidCapacity,
    DuplicateElement,
};

/// Result of an insert operation.
pub const InsertResult = enum {
    inserted,
    already_present,
    set_full,
};

/// Bounded set with compile-time capacity for integer types.
pub fn BoundedIntSet(comptime T: type, comptime capacity: usize) type {
    return struct {
        const Self = @This();

        data: [capacity]T = undefined,
        len: usize = 0,

        pub fn init() Self {
            return .{};
        }

        /// Get the number of elements in the set.
        pub fn count(self: *const Self) usize {
            return self.len;
        }

        /// Check if the set is empty.
        pub fn isEmpty(self: *const Self) bool {
            return self.len == 0;
        }

        /// Check if the set is full.
        pub fn isFull(self: *const Self) bool {
            return self.len >= capacity;
        }

        /// Get remaining capacity.
        pub fn remaining(self: *const Self) usize {
            return capacity - self.len;
        }

        /// Check if element exists in the set.
        pub fn contains(self: *const Self, value: T) bool {
            for (self.data[0..self.len]) |elem| {
                if (elem == value) return true;
            }
            return false;
        }

        /// Insert an element (fails if full).
        pub fn insert(self: *Self, value: T) SetError!void {
            if (self.contains(value)) return; // Already present, no-op
            if (self.isFull()) return error.SetFull;
            self.data[self.len] = value;
            self.len += 1;
        }

        /// Insert with detailed result.
        pub fn insertResult(self: *Self, value: T) InsertResult {
            if (self.contains(value)) return .already_present;
            if (self.isFull()) return .set_full;
            self.data[self.len] = value;
            self.len += 1;
            return .inserted;
        }

        /// Remove an element.
        pub fn remove(self: *Self, value: T) SetError!void {
            for (self.data[0..self.len], 0..) |elem, i| {
                if (elem == value) {
                    // Swap with last element
                    self.len -= 1;
                    if (i < self.len) {
                        self.data[i] = self.data[self.len];
                    }
                    return;
                }
            }
            return error.ElementNotFound;
        }

        /// Remove if present (no error if missing).
        pub fn discard(self: *Self, value: T) bool {
            self.remove(value) catch return false;
            return true;
        }

        /// Clear all elements.
        pub fn clear(self: *Self) void {
            self.len = 0;
        }

        /// Get elements as a slice.
        pub fn items(self: *const Self) []const T {
            return self.data[0..self.len];
        }

        /// Union with another set (modifies self).
        pub fn unionWith(self: *Self, other: *const Self) usize {
            var added: usize = 0;
            for (other.data[0..other.len]) |elem| {
                if (self.insertResult(elem) == .inserted) {
                    added += 1;
                }
            }
            return added;
        }

        /// Intersection with another set (modifies self).
        pub fn intersectWith(self: *Self, other: *const Self) usize {
            var removed: usize = 0;
            var i: usize = 0;
            while (i < self.len) {
                if (!other.contains(self.data[i])) {
                    // Remove this element
                    self.len -= 1;
                    if (i < self.len) {
                        self.data[i] = self.data[self.len];
                    }
                    removed += 1;
                } else {
                    i += 1;
                }
            }
            return removed;
        }

        /// Difference with another set (modifies self, removes elements in other).
        pub fn differenceWith(self: *Self, other: *const Self) usize {
            var removed: usize = 0;
            var i: usize = 0;
            while (i < self.len) {
                if (other.contains(self.data[i])) {
                    // Remove this element
                    self.len -= 1;
                    if (i < self.len) {
                        self.data[i] = self.data[self.len];
                    }
                    removed += 1;
                } else {
                    i += 1;
                }
            }
            return removed;
        }

        /// Symmetric difference (elements in either but not both).
        pub fn symmetricDifferenceWith(self: *Self, other: *const Self) void {
            // Elements in other but not in self -> add
            for (other.data[0..other.len]) |elem| {
                if (!self.contains(elem)) {
                    _ = self.insertResult(elem);
                }
            }
            // Elements in both -> remove
            var i: usize = 0;
            while (i < self.len) {
                if (other.contains(self.data[i])) {
                    self.len -= 1;
                    if (i < self.len) {
                        self.data[i] = self.data[self.len];
                    }
                } else {
                    i += 1;
                }
            }
        }

        /// Check if self is subset of other.
        pub fn isSubsetOf(self: *const Self, other: *const Self) bool {
            for (self.data[0..self.len]) |elem| {
                if (!other.contains(elem)) return false;
            }
            return true;
        }

        /// Check if self is proper subset of other.
        pub fn isProperSubsetOf(self: *const Self, other: *const Self) bool {
            return self.len < other.len and self.isSubsetOf(other);
        }

        /// Check if self is superset of other.
        pub fn isSupersetOf(self: *const Self, other: *const Self) bool {
            return other.isSubsetOf(self);
        }

        /// Check if sets are disjoint (no common elements).
        pub fn isDisjointWith(self: *const Self, other: *const Self) bool {
            for (self.data[0..self.len]) |elem| {
                if (other.contains(elem)) return false;
            }
            return true;
        }

        /// Check equality with another set.
        pub fn eql(self: *const Self, other: *const Self) bool {
            if (self.len != other.len) return false;
            return self.isSubsetOf(other);
        }

        /// Get the minimum element.
        pub fn min(self: *const Self) ?T {
            if (self.len == 0) return null;
            var result = self.data[0];
            for (self.data[1..self.len]) |elem| {
                if (elem < result) result = elem;
            }
            return result;
        }

        /// Get the maximum element.
        pub fn max(self: *const Self) ?T {
            if (self.len == 0) return null;
            var result = self.data[0];
            for (self.data[1..self.len]) |elem| {
                if (elem > result) result = elem;
            }
            return result;
        }

        /// Pop and return an arbitrary element.
        pub fn pop(self: *Self) ?T {
            if (self.len == 0) return null;
            self.len -= 1;
            return self.data[self.len];
        }
    };
}

/// Bounded string set (stores slices by reference).
pub fn BoundedStringSet(comptime capacity: usize) type {
    return struct {
        const Self = @This();

        data: [capacity][]const u8 = undefined,
        len: usize = 0,

        pub fn init() Self {
            return .{};
        }

        pub fn count(self: *const Self) usize {
            return self.len;
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.len == 0;
        }

        pub fn isFull(self: *const Self) bool {
            return self.len >= capacity;
        }

        pub fn contains(self: *const Self, value: []const u8) bool {
            for (self.data[0..self.len]) |elem| {
                if (std.mem.eql(u8, elem, value)) return true;
            }
            return false;
        }

        pub fn insert(self: *Self, value: []const u8) SetError!void {
            if (self.contains(value)) return;
            if (self.isFull()) return error.SetFull;
            self.data[self.len] = value;
            self.len += 1;
        }

        pub fn insertResult(self: *Self, value: []const u8) InsertResult {
            if (self.contains(value)) return .already_present;
            if (self.isFull()) return .set_full;
            self.data[self.len] = value;
            self.len += 1;
            return .inserted;
        }

        pub fn remove(self: *Self, value: []const u8) SetError!void {
            for (self.data[0..self.len], 0..) |elem, i| {
                if (std.mem.eql(u8, elem, value)) {
                    self.len -= 1;
                    if (i < self.len) {
                        self.data[i] = self.data[self.len];
                    }
                    return;
                }
            }
            return error.ElementNotFound;
        }

        pub fn discard(self: *Self, value: []const u8) bool {
            self.remove(value) catch return false;
            return true;
        }

        pub fn clear(self: *Self) void {
            self.len = 0;
        }

        pub fn items(self: *const Self) []const []const u8 {
            return self.data[0..self.len];
        }
    };
}

/// Ordered bounded set that maintains insertion order.
pub fn OrderedBoundedSet(comptime T: type, comptime capacity: usize) type {
    return struct {
        const Self = @This();

        data: [capacity]T = undefined,
        len: usize = 0,

        pub fn init() Self {
            return .{};
        }

        pub fn count(self: *const Self) usize {
            return self.len;
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.len == 0;
        }

        pub fn isFull(self: *const Self) bool {
            return self.len >= capacity;
        }

        pub fn contains(self: *const Self, value: T) bool {
            for (self.data[0..self.len]) |elem| {
                if (elem == value) return true;
            }
            return false;
        }

        /// Get element at index (in insertion order).
        pub fn get(self: *const Self, index: usize) ?T {
            if (index >= self.len) return null;
            return self.data[index];
        }

        /// Get index of element.
        pub fn indexOf(self: *const Self, value: T) ?usize {
            for (self.data[0..self.len], 0..) |elem, i| {
                if (elem == value) return i;
            }
            return null;
        }

        /// Insert preserving order.
        pub fn insert(self: *Self, value: T) SetError!void {
            if (self.contains(value)) return;
            if (self.isFull()) return error.SetFull;
            self.data[self.len] = value;
            self.len += 1;
        }

        /// Remove preserving order of remaining elements.
        pub fn remove(self: *Self, value: T) SetError!void {
            const idx = self.indexOf(value) orelse return error.ElementNotFound;

            // Shift elements left
            var i = idx;
            while (i < self.len - 1) : (i += 1) {
                self.data[i] = self.data[i + 1];
            }
            self.len -= 1;
        }

        /// Get first element.
        pub fn first(self: *const Self) ?T {
            return self.get(0);
        }

        /// Get last element.
        pub fn last(self: *const Self) ?T {
            if (self.len == 0) return null;
            return self.data[self.len - 1];
        }

        pub fn clear(self: *Self) void {
            self.len = 0;
        }

        pub fn items(self: *const Self) []const T {
            return self.data[0..self.len];
        }
    };
}

/// Sorted bounded set that maintains elements in sorted order.
pub fn SortedBoundedSet(comptime T: type, comptime capacity: usize) type {
    return struct {
        const Self = @This();

        data: [capacity]T = undefined,
        len: usize = 0,

        pub fn init() Self {
            return .{};
        }

        pub fn count(self: *const Self) usize {
            return self.len;
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.len == 0;
        }

        pub fn isFull(self: *const Self) bool {
            return self.len >= capacity;
        }

        /// Binary search for element position.
        fn findPosition(self: *const Self, value: T) struct { found: bool, index: usize } {
            if (self.len == 0) return .{ .found = false, .index = 0 };

            var left: usize = 0;
            var right: usize = self.len;

            while (left < right) {
                const mid = left + (right - left) / 2;
                if (self.data[mid] == value) {
                    return .{ .found = true, .index = mid };
                } else if (self.data[mid] < value) {
                    left = mid + 1;
                } else {
                    right = mid;
                }
            }

            return .{ .found = false, .index = left };
        }

        pub fn contains(self: *const Self, value: T) bool {
            return self.findPosition(value).found;
        }

        /// Insert in sorted position.
        pub fn insert(self: *Self, value: T) SetError!void {
            const pos = self.findPosition(value);
            if (pos.found) return; // Already present
            if (self.isFull()) return error.SetFull;

            // Shift elements right
            var i = self.len;
            while (i > pos.index) : (i -= 1) {
                self.data[i] = self.data[i - 1];
            }
            self.data[pos.index] = value;
            self.len += 1;
        }

        /// Remove element.
        pub fn remove(self: *Self, value: T) SetError!void {
            const pos = self.findPosition(value);
            if (!pos.found) return error.ElementNotFound;

            // Shift elements left
            var i = pos.index;
            while (i < self.len - 1) : (i += 1) {
                self.data[i] = self.data[i + 1];
            }
            self.len -= 1;
        }

        /// Get element at index (in sorted order).
        pub fn get(self: *const Self, index: usize) ?T {
            if (index >= self.len) return null;
            return self.data[index];
        }

        /// Get minimum element (first in sorted order).
        pub fn min(self: *const Self) ?T {
            return self.get(0);
        }

        /// Get maximum element (last in sorted order).
        pub fn max(self: *const Self) ?T {
            if (self.len == 0) return null;
            return self.data[self.len - 1];
        }

        /// Get range of elements in [low, high].
        pub fn range(self: *const Self, low: T, high: T, buffer: []T) []T {
            var count_written: usize = 0;
            for (self.data[0..self.len]) |elem| {
                if (elem >= low and elem <= high) {
                    if (count_written >= buffer.len) break;
                    buffer[count_written] = elem;
                    count_written += 1;
                }
            }
            return buffer[0..count_written];
        }

        pub fn clear(self: *Self) void {
            self.len = 0;
        }

        pub fn items(self: *const Self) []const T {
            return self.data[0..self.len];
        }
    };
}

/// Bit set for efficient storage of boolean flags.
pub fn BoundedBitSet(comptime max_value: usize) type {
    const num_words = (max_value + 63) / 64;

    return struct {
        const Self = @This();

        bits: [num_words]u64 = [_]u64{0} ** num_words,
        count_cache: usize = 0,

        pub fn init() Self {
            return .{};
        }

        pub fn count(self: *const Self) usize {
            return self.count_cache;
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.count_cache == 0;
        }

        fn wordAndBit(value: usize) struct { word: usize, bit: u6 } {
            return .{
                .word = value / 64,
                .bit = @intCast(value % 64),
            };
        }

        pub fn contains(self: *const Self, value: usize) bool {
            if (value >= max_value) return false;
            const pos = wordAndBit(value);
            return (self.bits[pos.word] & (@as(u64, 1) << pos.bit)) != 0;
        }

        pub fn insert(self: *Self, value: usize) SetError!void {
            if (value >= max_value) return error.SetFull;
            const pos = wordAndBit(value);
            const mask = @as(u64, 1) << pos.bit;
            if ((self.bits[pos.word] & mask) == 0) {
                self.bits[pos.word] |= mask;
                self.count_cache += 1;
            }
        }

        pub fn remove(self: *Self, value: usize) SetError!void {
            if (value >= max_value) return error.ElementNotFound;
            const pos = wordAndBit(value);
            const mask = @as(u64, 1) << pos.bit;
            if ((self.bits[pos.word] & mask) != 0) {
                self.bits[pos.word] &= ~mask;
                self.count_cache -= 1;
            } else {
                return error.ElementNotFound;
            }
        }

        pub fn toggle(self: *Self, value: usize) void {
            if (value >= max_value) return;
            const pos = wordAndBit(value);
            const mask = @as(u64, 1) << pos.bit;
            if ((self.bits[pos.word] & mask) != 0) {
                self.bits[pos.word] &= ~mask;
                self.count_cache -= 1;
            } else {
                self.bits[pos.word] |= mask;
                self.count_cache += 1;
            }
        }

        pub fn clear(self: *Self) void {
            for (&self.bits) |*word| {
                word.* = 0;
            }
            self.count_cache = 0;
        }

        pub fn setAll(self: *Self) void {
            for (&self.bits) |*word| {
                word.* = ~@as(u64, 0);
            }
            // Mask out invalid high bits in last word
            const remainder = max_value % 64;
            if (remainder != 0) {
                self.bits[num_words - 1] &= (@as(u64, 1) << @intCast(remainder)) - 1;
            }
            self.count_cache = max_value;
        }

        /// Union with another bit set.
        pub fn unionWith(self: *Self, other: *const Self) void {
            for (&self.bits, other.bits) |*a, b| {
                a.* |= b;
            }
            self.recountBits();
        }

        /// Intersection with another bit set.
        pub fn intersectWith(self: *Self, other: *const Self) void {
            for (&self.bits, other.bits) |*a, b| {
                a.* &= b;
            }
            self.recountBits();
        }

        /// Difference with another bit set.
        pub fn differenceWith(self: *Self, other: *const Self) void {
            for (&self.bits, other.bits) |*a, b| {
                a.* &= ~b;
            }
            self.recountBits();
        }

        fn recountBits(self: *Self) void {
            var total: usize = 0;
            for (self.bits) |word| {
                total += @popCount(word);
            }
            self.count_cache = total;
        }
    };
}

test "BoundedIntSet basic" {
    var set = BoundedIntSet(u32, 5).init();

    try set.insert(1);
    try set.insert(2);
    try set.insert(3);

    try std.testing.expectEqual(@as(usize, 3), set.count());
    try std.testing.expect(set.contains(1));
    try std.testing.expect(set.contains(2));
    try std.testing.expect(!set.contains(4));
}

test "BoundedIntSet duplicate" {
    var set = BoundedIntSet(u32, 5).init();

    try set.insert(1);
    try set.insert(1); // Should be no-op
    try std.testing.expectEqual(@as(usize, 1), set.count());
}

test "BoundedIntSet full" {
    var set = BoundedIntSet(u32, 3).init();

    try set.insert(1);
    try set.insert(2);
    try set.insert(3);
    try std.testing.expectError(error.SetFull, set.insert(4));
}

test "BoundedIntSet operations" {
    var set1 = BoundedIntSet(u32, 10).init();
    var set2 = BoundedIntSet(u32, 10).init();

    try set1.insert(1);
    try set1.insert(2);
    try set1.insert(3);

    try set2.insert(2);
    try set2.insert(3);
    try set2.insert(4);

    // Intersection
    var intersection = set1;
    _ = intersection.intersectWith(&set2);
    try std.testing.expectEqual(@as(usize, 2), intersection.count());
    try std.testing.expect(intersection.contains(2));
    try std.testing.expect(intersection.contains(3));
}

test "BoundedIntSet min/max" {
    var set = BoundedIntSet(i32, 10).init();

    try std.testing.expect(set.min() == null);
    try std.testing.expect(set.max() == null);

    try set.insert(5);
    try set.insert(2);
    try set.insert(8);

    try std.testing.expectEqual(@as(i32, 2), set.min().?);
    try std.testing.expectEqual(@as(i32, 8), set.max().?);
}

test "SortedBoundedSet" {
    var set = SortedBoundedSet(u32, 10).init();

    try set.insert(5);
    try set.insert(2);
    try set.insert(8);
    try set.insert(1);

    const items_slice = set.items();
    try std.testing.expectEqual(@as(u32, 1), items_slice[0]);
    try std.testing.expectEqual(@as(u32, 2), items_slice[1]);
    try std.testing.expectEqual(@as(u32, 5), items_slice[2]);
    try std.testing.expectEqual(@as(u32, 8), items_slice[3]);
}

test "BoundedBitSet" {
    var set = BoundedBitSet(100).init();

    try set.insert(5);
    try set.insert(50);
    try set.insert(99);

    try std.testing.expectEqual(@as(usize, 3), set.count());
    try std.testing.expect(set.contains(5));
    try std.testing.expect(set.contains(50));
    try std.testing.expect(!set.contains(6));

    try set.remove(50);
    try std.testing.expectEqual(@as(usize, 2), set.count());
    try std.testing.expect(!set.contains(50));
}

test "BoundedStringSet" {
    var set = BoundedStringSet(5).init();

    try set.insert("hello");
    try set.insert("world");
    try set.insert("hello"); // Duplicate

    try std.testing.expectEqual(@as(usize, 2), set.count());
    try std.testing.expect(set.contains("hello"));
    try std.testing.expect(set.contains("world"));
    try std.testing.expect(!set.contains("foo"));
}

test "InsertResult" {
    var set = BoundedIntSet(u32, 2).init();

    try std.testing.expectEqual(InsertResult.inserted, set.insertResult(1));
    try std.testing.expectEqual(InsertResult.already_present, set.insertResult(1));
    try std.testing.expectEqual(InsertResult.inserted, set.insertResult(2));
    try std.testing.expectEqual(InsertResult.set_full, set.insertResult(3));
}
