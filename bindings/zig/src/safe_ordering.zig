// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe total ordering and comparison operations that cannot crash.
//!
//! Provides type-safe comparison utilities with well-defined behavior for
//! edge cases including NaN handling, null comparisons, and custom ordering.

const std = @import("std");

pub const OrderingError = error{
    IncomparableValues,
    InvalidComparator,
    CycleDetected,
};

/// Result of a three-way comparison.
pub const Ordering = enum(i8) {
    less = -1,
    equal = 0,
    greater = 1,

    /// Reverse the ordering.
    pub fn reverse(self: Ordering) Ordering {
        return switch (self) {
            .less => .greater,
            .equal => .equal,
            .greater => .less,
        };
    }

    /// Chain two orderings (use second if first is equal).
    pub fn then(self: Ordering, other: Ordering) Ordering {
        return if (self == .equal) other else self;
    }

    /// Convert to integer (-1, 0, 1).
    pub fn toInt(self: Ordering) i8 {
        return @intFromEnum(self);
    }

    /// Create from integer comparison result.
    pub fn fromInt(value: anytype) Ordering {
        if (value < 0) return .less;
        if (value > 0) return .greater;
        return .equal;
    }
};

/// Compare two values of the same type using natural ordering.
pub fn compare(comptime T: type, a: T, b: T) Ordering {
    if (comptime isFloatType(T)) {
        return compareFloats(a, b);
    }

    if (a < b) return .less;
    if (a > b) return .greater;
    return .equal;
}

/// Compare two optional values (null is considered less than any value).
pub fn compareOptional(comptime T: type, a: ?T, b: ?T) Ordering {
    if (a == null and b == null) return .equal;
    if (a == null) return .less;
    if (b == null) return .greater;
    return compare(T, a.?, b.?);
}

/// Compare two slices lexicographically.
pub fn compareSlices(comptime T: type, a: []const T, b: []const T) Ordering {
    const min_len = @min(a.len, b.len);

    for (0..min_len) |i| {
        const ord = compare(T, a[i], b[i]);
        if (ord != .equal) return ord;
    }

    return compare(usize, a.len, b.len);
}

/// Compare two strings lexicographically.
pub fn compareStrings(a: []const u8, b: []const u8) Ordering {
    return compareSlices(u8, a, b);
}

/// Compare two strings case-insensitively.
pub fn compareStringsIgnoreCase(a: []const u8, b: []const u8) Ordering {
    const min_len = @min(a.len, b.len);

    for (0..min_len) |i| {
        const ca = std.ascii.toLower(a[i]);
        const cb = std.ascii.toLower(b[i]);
        const ord = compare(u8, ca, cb);
        if (ord != .equal) return ord;
    }

    return compare(usize, a.len, b.len);
}

/// Compare floating point values with NaN handling (NaN is greater than all values).
pub fn compareFloats(a: anytype, b: @TypeOf(a)) Ordering {
    const T = @TypeOf(a);
    const a_nan = std.math.isNan(a);
    const b_nan = std.math.isNan(b);

    if (a_nan and b_nan) return .equal;
    if (a_nan) return .greater;
    if (b_nan) return .less;

    if (a < b) return .less;
    if (a > b) return .greater;

    // Handle -0.0 vs +0.0
    if (comptime T == f32 or T == f64) {
        const a_neg_zero = a == 0 and std.math.signbit(a);
        const b_neg_zero = b == 0 and std.math.signbit(b);
        if (a_neg_zero and !b_neg_zero) return .less;
        if (!a_neg_zero and b_neg_zero) return .greater;
    }

    return .equal;
}

/// Minimum of two values.
pub fn min(comptime T: type, a: T, b: T) T {
    return if (compare(T, a, b) == .less) a else b;
}

/// Maximum of two values.
pub fn max(comptime T: type, a: T, b: T) T {
    return if (compare(T, a, b) == .greater) a else b;
}

/// Clamp a value to a range [lower, upper].
pub fn clamp(comptime T: type, value: T, lower: T, upper: T) T {
    return max(T, lower, min(T, upper, value));
}

/// Check if a value is within a range [lower, upper].
pub fn inRange(comptime T: type, value: T, lower: T, upper: T) bool {
    return compare(T, value, lower) != .less and compare(T, value, upper) != .greater;
}

/// Find minimum value in a slice.
pub fn minSlice(comptime T: type, slice: []const T) ?T {
    if (slice.len == 0) return null;
    var result = slice[0];
    for (slice[1..]) |item| {
        if (compare(T, item, result) == .less) {
            result = item;
        }
    }
    return result;
}

/// Find maximum value in a slice.
pub fn maxSlice(comptime T: type, slice: []const T) ?T {
    if (slice.len == 0) return null;
    var result = slice[0];
    for (slice[1..]) |item| {
        if (compare(T, item, result) == .greater) {
            result = item;
        }
    }
    return result;
}

/// Check if a slice is sorted in ascending order.
pub fn isSorted(comptime T: type, slice: []const T) bool {
    if (slice.len <= 1) return true;
    for (0..slice.len - 1) |i| {
        if (compare(T, slice[i], slice[i + 1]) == .greater) return false;
    }
    return true;
}

/// Check if a slice is sorted in descending order.
pub fn isSortedDescending(comptime T: type, slice: []const T) bool {
    if (slice.len <= 1) return true;
    for (0..slice.len - 1) |i| {
        if (compare(T, slice[i], slice[i + 1]) == .less) return false;
    }
    return true;
}

/// Comparator type for custom ordering.
pub fn Comparator(comptime T: type) type {
    return *const fn (T, T) Ordering;
}

/// Create a reverse comparator from an existing comparator.
pub fn reverseComparator(comptime T: type, cmp: Comparator(T)) Comparator(T) {
    const S = struct {
        var inner: Comparator(T) = undefined;

        fn reversed(a: T, b: T) Ordering {
            return inner(a, b).reverse();
        }
    };
    S.inner = cmp;
    return S.reversed;
}

/// Natural comparator using default ordering.
pub fn naturalComparator(comptime T: type) Comparator(T) {
    return struct {
        fn cmp(a: T, b: T) Ordering {
            return compare(T, a, b);
        }
    }.cmp;
}

fn isFloatType(comptime T: type) bool {
    return T == f16 or T == f32 or T == f64 or T == f80 or T == f128;
}

// ============================================================================
// Tests
// ============================================================================

test "Ordering basic" {
    try std.testing.expectEqual(Ordering.less, compare(i32, 1, 2));
    try std.testing.expectEqual(Ordering.equal, compare(i32, 5, 5));
    try std.testing.expectEqual(Ordering.greater, compare(i32, 10, 3));
}

test "Ordering reverse" {
    try std.testing.expectEqual(Ordering.greater, Ordering.less.reverse());
    try std.testing.expectEqual(Ordering.less, Ordering.greater.reverse());
    try std.testing.expectEqual(Ordering.equal, Ordering.equal.reverse());
}

test "Ordering then" {
    try std.testing.expectEqual(Ordering.less, Ordering.less.then(.greater));
    try std.testing.expectEqual(Ordering.greater, Ordering.equal.then(.greater));
}

test "compareOptional" {
    try std.testing.expectEqual(Ordering.equal, compareOptional(i32, null, null));
    try std.testing.expectEqual(Ordering.less, compareOptional(i32, null, 5));
    try std.testing.expectEqual(Ordering.greater, compareOptional(i32, 5, null));
    try std.testing.expectEqual(Ordering.less, compareOptional(i32, 3, 5));
}

test "compareStrings" {
    try std.testing.expectEqual(Ordering.less, compareStrings("apple", "banana"));
    try std.testing.expectEqual(Ordering.equal, compareStrings("test", "test"));
    try std.testing.expectEqual(Ordering.greater, compareStrings("zebra", "ant"));
    try std.testing.expectEqual(Ordering.less, compareStrings("ab", "abc"));
}

test "compareStringsIgnoreCase" {
    try std.testing.expectEqual(Ordering.equal, compareStringsIgnoreCase("Hello", "hello"));
    try std.testing.expectEqual(Ordering.less, compareStringsIgnoreCase("Apple", "BANANA"));
}

test "compareFloats NaN handling" {
    try std.testing.expectEqual(Ordering.greater, compareFloats(std.math.nan(f64), 1.0));
    try std.testing.expectEqual(Ordering.less, compareFloats(1.0, std.math.nan(f64)));
    try std.testing.expectEqual(Ordering.equal, compareFloats(std.math.nan(f64), std.math.nan(f64)));
}

test "min max clamp" {
    try std.testing.expectEqual(@as(i32, 3), min(i32, 3, 7));
    try std.testing.expectEqual(@as(i32, 7), max(i32, 3, 7));
    try std.testing.expectEqual(@as(i32, 5), clamp(i32, 10, 0, 5));
    try std.testing.expectEqual(@as(i32, 0), clamp(i32, -5, 0, 5));
    try std.testing.expectEqual(@as(i32, 3), clamp(i32, 3, 0, 5));
}

test "inRange" {
    try std.testing.expect(inRange(i32, 5, 0, 10));
    try std.testing.expect(inRange(i32, 0, 0, 10));
    try std.testing.expect(inRange(i32, 10, 0, 10));
    try std.testing.expect(!inRange(i32, 11, 0, 10));
    try std.testing.expect(!inRange(i32, -1, 0, 10));
}

test "minSlice maxSlice" {
    const data = [_]i32{ 5, 2, 8, 1, 9, 3 };
    try std.testing.expectEqual(@as(i32, 1), minSlice(i32, &data).?);
    try std.testing.expectEqual(@as(i32, 9), maxSlice(i32, &data).?);
    try std.testing.expectEqual(@as(?i32, null), minSlice(i32, &[_]i32{}));
}

test "isSorted" {
    try std.testing.expect(isSorted(i32, &[_]i32{ 1, 2, 3, 4, 5 }));
    try std.testing.expect(!isSorted(i32, &[_]i32{ 1, 3, 2, 4, 5 }));
    try std.testing.expect(isSorted(i32, &[_]i32{}));
    try std.testing.expect(isSorted(i32, &[_]i32{42}));
}

test "isSortedDescending" {
    try std.testing.expect(isSortedDescending(i32, &[_]i32{ 5, 4, 3, 2, 1 }));
    try std.testing.expect(!isSortedDescending(i32, &[_]i32{ 5, 4, 6, 2, 1 }));
}
