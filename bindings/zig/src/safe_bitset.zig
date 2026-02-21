// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe fixed-size bitset operations with bounds checking that cannot crash.
//!
//! This module provides a safe wrapper around bitset operations with explicit
//! bounds checking. All operations return errors on invalid indices rather
//! than causing undefined behavior.

const std = @import("std");

/// Error types for bitset operations.
pub const BitSetError = error{
    IndexOutOfBounds,
    SizeMismatch,
    InvalidRange,
    Overflow,
};

/// A fixed-size bitset with safe operations.
pub fn SafeBitSet(comptime size: usize) type {
    return struct {
        const Self = @This();
        const WordType = usize;
        const word_bits = @bitSizeOf(WordType);
        const word_count = (size + word_bits - 1) / word_bits;

        words: [word_count]WordType,

        /// Initialize with all bits set to zero.
        pub fn initEmpty() Self {
            return .{ .words = [_]WordType{0} ** word_count };
        }

        /// Initialize with all bits set to one.
        pub fn initFull() Self {
            var result = Self{ .words = [_]WordType{~@as(WordType, 0)} ** word_count };
            // Clear unused bits in the last word
            const extra_bits = size % word_bits;
            if (extra_bits != 0) {
                const mask = (@as(WordType, 1) << @intCast(extra_bits)) - 1;
                result.words[word_count - 1] &= mask;
            }
            return result;
        }

        /// Get the capacity (number of bits).
        pub fn capacity() usize {
            return size;
        }

        /// Safely set a bit at the given index.
        pub fn set(self: *Self, index: usize) BitSetError!void {
            if (index >= size) return error.IndexOutOfBounds;
            const word_index = index / word_bits;
            const bit_index: std.math.Log2Int(WordType) = @intCast(index % word_bits);
            self.words[word_index] |= @as(WordType, 1) << bit_index;
        }

        /// Safely unset (clear) a bit at the given index.
        pub fn unset(self: *Self, index: usize) BitSetError!void {
            if (index >= size) return error.IndexOutOfBounds;
            const word_index = index / word_bits;
            const bit_index: std.math.Log2Int(WordType) = @intCast(index % word_bits);
            self.words[word_index] &= ~(@as(WordType, 1) << bit_index);
        }

        /// Safely toggle a bit at the given index.
        pub fn toggle(self: *Self, index: usize) BitSetError!void {
            if (index >= size) return error.IndexOutOfBounds;
            const word_index = index / word_bits;
            const bit_index: std.math.Log2Int(WordType) = @intCast(index % word_bits);
            self.words[word_index] ^= @as(WordType, 1) << bit_index;
        }

        /// Safely check if a bit is set at the given index.
        pub fn isSet(self: *const Self, index: usize) BitSetError!bool {
            if (index >= size) return error.IndexOutOfBounds;
            const word_index = index / word_bits;
            const bit_index: std.math.Log2Int(WordType) = @intCast(index % word_bits);
            return (self.words[word_index] & (@as(WordType, 1) << bit_index)) != 0;
        }

        /// Get bit value (returns null if out of bounds).
        pub fn get(self: *const Self, index: usize) ?bool {
            return self.isSet(index) catch null;
        }

        /// Set bit value (returns false if out of bounds).
        pub fn put(self: *Self, index: usize, value: bool) bool {
            if (value) {
                self.set(index) catch return false;
            } else {
                self.unset(index) catch return false;
            }
            return true;
        }

        /// Set a range of bits [start, end).
        pub fn setRange(self: *Self, start: usize, end: usize) BitSetError!void {
            if (start > end) return error.InvalidRange;
            if (end > size) return error.IndexOutOfBounds;

            var index = start;
            while (index < end) : (index += 1) {
                try self.set(index);
            }
        }

        /// Unset a range of bits [start, end).
        pub fn unsetRange(self: *Self, start: usize, end: usize) BitSetError!void {
            if (start > end) return error.InvalidRange;
            if (end > size) return error.IndexOutOfBounds;

            var index = start;
            while (index < end) : (index += 1) {
                try self.unset(index);
            }
        }

        /// Count the number of set bits (popcount).
        pub fn count(self: *const Self) usize {
            var total: usize = 0;
            for (self.words) |word| {
                total += @popCount(word);
            }
            return total;
        }

        /// Count the number of unset bits.
        pub fn countZeros(self: *const Self) usize {
            return size - self.count();
        }

        /// Check if all bits are zero.
        pub fn isEmpty(self: *const Self) bool {
            for (self.words) |word| {
                if (word != 0) return false;
            }
            return true;
        }

        /// Check if all bits are one.
        pub fn isFull(self: *const Self) bool {
            return self.count() == size;
        }

        /// Check if any bit is set.
        pub fn any(self: *const Self) bool {
            return !self.isEmpty();
        }

        /// Check if no bits are set.
        pub fn none(self: *const Self) bool {
            return self.isEmpty();
        }

        /// Find the first set bit, return null if none.
        pub fn findFirstSet(self: *const Self) ?usize {
            for (self.words, 0..) |word, word_index| {
                if (word != 0) {
                    const bit_pos = @ctz(word);
                    const result = word_index * word_bits + bit_pos;
                    if (result < size) return result;
                }
            }
            return null;
        }

        /// Find the last set bit, return null if none.
        pub fn findLastSet(self: *const Self) ?usize {
            var word_index = word_count;
            while (word_index > 0) {
                word_index -= 1;
                const word = self.words[word_index];
                if (word != 0) {
                    const bit_pos = word_bits - 1 - @clz(word);
                    const result = word_index * word_bits + bit_pos;
                    if (result < size) return result;
                }
            }
            return null;
        }

        /// Find the first unset bit, return null if all are set.
        pub fn findFirstUnset(self: *const Self) ?usize {
            for (self.words, 0..) |word, word_index| {
                const inverted = ~word;
                if (inverted != 0) {
                    const bit_pos = @ctz(inverted);
                    const result = word_index * word_bits + bit_pos;
                    if (result < size) return result;
                }
            }
            return null;
        }

        /// Clear all bits.
        pub fn clear(self: *Self) void {
            self.* = initEmpty();
        }

        /// Set all bits.
        pub fn fill(self: *Self) void {
            self.* = initFull();
        }

        /// Complement (invert) all bits.
        pub fn complement(self: *Self) void {
            for (&self.words) |*word| {
                word.* = ~word.*;
            }
            // Clear unused bits in the last word
            const extra_bits = size % word_bits;
            if (extra_bits != 0) {
                const mask = (@as(WordType, 1) << @intCast(extra_bits)) - 1;
                self.words[word_count - 1] &= mask;
            }
        }

        /// Bitwise AND with another bitset.
        pub fn setIntersection(self: *Self, other: *const Self) void {
            for (&self.words, other.words) |*a, b| {
                a.* &= b;
            }
        }

        /// Bitwise OR with another bitset.
        pub fn setUnion(self: *Self, other: *const Self) void {
            for (&self.words, other.words) |*a, b| {
                a.* |= b;
            }
        }

        /// Bitwise XOR with another bitset.
        pub fn setXor(self: *Self, other: *const Self) void {
            for (&self.words, other.words) |*a, b| {
                a.* ^= b;
            }
        }

        /// Bitwise AND NOT (set difference): self & ~other.
        pub fn setDifference(self: *Self, other: *const Self) void {
            for (&self.words, other.words) |*a, b| {
                a.* &= ~b;
            }
        }

        /// Check if two bitsets are equal.
        pub fn eql(self: *const Self, other: *const Self) bool {
            return std.mem.eql(WordType, &self.words, &other.words);
        }

        /// Check if this bitset is a subset of another.
        pub fn isSubsetOf(self: *const Self, other: *const Self) bool {
            for (self.words, other.words) |a, b| {
                if ((a & ~b) != 0) return false;
            }
            return true;
        }

        /// Check if this bitset is a superset of another.
        pub fn isSupersetOf(self: *const Self, other: *const Self) bool {
            return other.isSubsetOf(self);
        }

        /// Check if two bitsets are disjoint (no common set bits).
        pub fn isDisjoint(self: *const Self, other: *const Self) bool {
            for (self.words, other.words) |a, b| {
                if ((a & b) != 0) return false;
            }
            return true;
        }

        /// Return a new bitset that is the intersection.
        pub fn intersection(self: *const Self, other: *const Self) Self {
            var result = self.*;
            result.setIntersection(other);
            return result;
        }

        /// Return a new bitset that is the union.
        pub fn unionWith(self: *const Self, other: *const Self) Self {
            var result = self.*;
            result.setUnion(other);
            return result;
        }

        /// Return a new bitset that is the XOR.
        pub fn xorWith(self: *const Self, other: *const Self) Self {
            var result = self.*;
            result.setXor(other);
            return result;
        }

        /// Return a new bitset that is the difference.
        pub fn difference(self: *const Self, other: *const Self) Self {
            var result = self.*;
            result.setDifference(other);
            return result;
        }

        /// Iterator over set bits.
        pub const Iterator = struct {
            bitset: *const Self,
            current: usize,

            pub fn next(self: *Iterator) ?usize {
                while (self.current < size) {
                    const index = self.current;
                    self.current += 1;
                    if (self.bitset.get(index) orelse false) {
                        return index;
                    }
                }
                return null;
            }
        };

        /// Get an iterator over set bits.
        pub fn iterator(self: *const Self) Iterator {
            return .{ .bitset = self, .current = 0 };
        }

        /// Convert to slice of set indices.
        pub fn toIndexSlice(self: *const Self, buffer: []usize) []usize {
            var count_val: usize = 0;
            var iter = self.iterator();
            while (iter.next()) |index| {
                if (count_val >= buffer.len) break;
                buffer[count_val] = index;
                count_val += 1;
            }
            return buffer[0..count_val];
        }
    };
}

/// Create a bitset from a slice of indices.
pub fn fromIndices(comptime size: usize, indices: []const usize) BitSetError!SafeBitSet(size) {
    var result = SafeBitSet(size).initEmpty();
    for (indices) |index| {
        try result.set(index);
    }
    return result;
}

/// Hamming distance between two bitsets (count of differing bits).
pub fn hammingDistance(comptime size: usize, a: *const SafeBitSet(size), b: *const SafeBitSet(size)) usize {
    const xored = a.xorWith(b);
    return xored.count();
}

/// Jaccard similarity coefficient between two bitsets.
pub fn jaccardSimilarity(comptime size: usize, a: *const SafeBitSet(size), b: *const SafeBitSet(size)) f64 {
    const intersect = a.intersection(b);
    const union_set = a.unionWith(b);

    const intersect_count = intersect.count();
    const union_count = union_set.count();

    if (union_count == 0) return 1.0; // Both empty sets are considered identical
    return @as(f64, @floatFromInt(intersect_count)) / @as(f64, @floatFromInt(union_count));
}

test "SafeBitSet basic operations" {
    var bs = SafeBitSet(64).initEmpty();

    try bs.set(0);
    try bs.set(10);
    try bs.set(63);

    try std.testing.expect(try bs.isSet(0));
    try std.testing.expect(try bs.isSet(10));
    try std.testing.expect(try bs.isSet(63));
    try std.testing.expect(!(try bs.isSet(1)));

    try std.testing.expectEqual(@as(usize, 3), bs.count());
}

test "SafeBitSet bounds checking" {
    var bs = SafeBitSet(32).initEmpty();

    try std.testing.expectError(error.IndexOutOfBounds, bs.set(32));
    try std.testing.expectError(error.IndexOutOfBounds, bs.set(100));
    try std.testing.expectError(error.IndexOutOfBounds, bs.isSet(32));
}

test "SafeBitSet toggle" {
    var bs = SafeBitSet(16).initEmpty();

    try bs.toggle(5);
    try std.testing.expect(try bs.isSet(5));

    try bs.toggle(5);
    try std.testing.expect(!(try bs.isSet(5)));
}

test "SafeBitSet initFull" {
    const bs = SafeBitSet(10).initFull();
    try std.testing.expectEqual(@as(usize, 10), bs.count());
    try std.testing.expect(bs.isFull());
}

test "SafeBitSet setRange" {
    var bs = SafeBitSet(32).initEmpty();
    try bs.setRange(4, 8);

    try std.testing.expect(try bs.isSet(4));
    try std.testing.expect(try bs.isSet(7));
    try std.testing.expect(!(try bs.isSet(3)));
    try std.testing.expect(!(try bs.isSet(8)));
    try std.testing.expectEqual(@as(usize, 4), bs.count());
}

test "SafeBitSet findFirstSet" {
    var bs = SafeBitSet(64).initEmpty();
    try std.testing.expectEqual(@as(?usize, null), bs.findFirstSet());

    try bs.set(42);
    try std.testing.expectEqual(@as(?usize, 42), bs.findFirstSet());

    try bs.set(10);
    try std.testing.expectEqual(@as(?usize, 10), bs.findFirstSet());
}

test "SafeBitSet set operations" {
    var a = SafeBitSet(16).initEmpty();
    var b = SafeBitSet(16).initEmpty();

    try a.set(1);
    try a.set(2);
    try a.set(3);

    try b.set(2);
    try b.set(3);
    try b.set(4);

    const intersect = a.intersection(&b);
    try std.testing.expectEqual(@as(usize, 2), intersect.count());

    const union_set = a.unionWith(&b);
    try std.testing.expectEqual(@as(usize, 4), union_set.count());

    try std.testing.expect(!a.isDisjoint(&b));
}

test "SafeBitSet iterator" {
    var bs = SafeBitSet(32).initEmpty();
    try bs.set(3);
    try bs.set(7);
    try bs.set(15);

    var iter = bs.iterator();
    try std.testing.expectEqual(@as(?usize, 3), iter.next());
    try std.testing.expectEqual(@as(?usize, 7), iter.next());
    try std.testing.expectEqual(@as(?usize, 15), iter.next());
    try std.testing.expectEqual(@as(?usize, null), iter.next());
}

test "fromIndices" {
    const indices = [_]usize{ 1, 5, 10 };
    const bs = try fromIndices(16, &indices);
    try std.testing.expectEqual(@as(usize, 3), bs.count());
    try std.testing.expect(bs.get(1).?);
    try std.testing.expect(bs.get(5).?);
    try std.testing.expect(bs.get(10).?);
}

test "hammingDistance" {
    var a = SafeBitSet(8).initEmpty();
    var b = SafeBitSet(8).initEmpty();

    try a.set(0);
    try a.set(1);
    try a.set(2);

    try b.set(1);
    try b.set(2);
    try b.set(3);

    try std.testing.expectEqual(@as(usize, 2), hammingDistance(8, &a, &b));
}

test "jaccardSimilarity" {
    var a = SafeBitSet(8).initEmpty();
    var b = SafeBitSet(8).initEmpty();

    try a.set(0);
    try a.set(1);

    try b.set(1);
    try b.set(2);

    // Intersection: {1}, Union: {0, 1, 2}
    // Jaccard = 1/3 = 0.333...
    const similarity = jaccardSimilarity(8, &a, &b);
    try std.testing.expectApproxEqAbs(@as(f64, 1.0 / 3.0), similarity, 0.001);
}
