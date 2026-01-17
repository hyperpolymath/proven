// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe numeric interval operations with bounds checking that cannot crash.
//! Provides closed, open, and half-open interval types with set operations.

const std = @import("std");

/// Error types for interval operations.
pub const IntervalError = error{
    InvalidBounds,
    EmptyInterval,
    DisjointIntervals,
    Overflow,
};

/// Bound type for interval endpoints.
pub const BoundType = enum {
    closed,      // Includes the endpoint [a, b]
    open,        // Excludes the endpoint (a, b)
    unbounded,   // No bound (extends to infinity)
};

/// Generic interval type for numeric values.
pub fn Interval(comptime T: type) type {
    return struct {
        const Self = @This();

        lower: ?T,
        upper: ?T,
        lower_bound: BoundType,
        upper_bound: BoundType,

        /// Create a closed interval [lower, upper].
        pub fn closed(lower: T, upper: T) IntervalError!Self {
            if (lower > upper) return error.InvalidBounds;
            return Self{
                .lower = lower,
                .upper = upper,
                .lower_bound = .closed,
                .upper_bound = .closed,
            };
        }

        /// Create an open interval (lower, upper).
        pub fn open(lower: T, upper: T) IntervalError!Self {
            if (lower >= upper) return error.InvalidBounds;
            return Self{
                .lower = lower,
                .upper = upper,
                .lower_bound = .open,
                .upper_bound = .open,
            };
        }

        /// Create a half-open interval [lower, upper).
        pub fn closedOpen(lower: T, upper: T) IntervalError!Self {
            if (lower >= upper) return error.InvalidBounds;
            return Self{
                .lower = lower,
                .upper = upper,
                .lower_bound = .closed,
                .upper_bound = .open,
            };
        }

        /// Create a half-open interval (lower, upper].
        pub fn openClosed(lower: T, upper: T) IntervalError!Self {
            if (lower >= upper) return error.InvalidBounds;
            return Self{
                .lower = lower,
                .upper = upper,
                .lower_bound = .open,
                .upper_bound = .closed,
            };
        }

        /// Create an interval from lower bound to positive infinity [lower, +inf).
        pub fn atLeast(lower: T) Self {
            return Self{
                .lower = lower,
                .upper = null,
                .lower_bound = .closed,
                .upper_bound = .unbounded,
            };
        }

        /// Create an interval from lower bound to positive infinity (lower, +inf).
        pub fn greaterThan(lower: T) Self {
            return Self{
                .lower = lower,
                .upper = null,
                .lower_bound = .open,
                .upper_bound = .unbounded,
            };
        }

        /// Create an interval from negative infinity to upper bound (-inf, upper].
        pub fn atMost(upper: T) Self {
            return Self{
                .lower = null,
                .upper = upper,
                .lower_bound = .unbounded,
                .upper_bound = .closed,
            };
        }

        /// Create an interval from negative infinity to upper bound (-inf, upper).
        pub fn lessThan(upper: T) Self {
            return Self{
                .lower = null,
                .upper = upper,
                .lower_bound = .unbounded,
                .upper_bound = .open,
            };
        }

        /// Create an interval covering all values (-inf, +inf).
        pub fn all() Self {
            return Self{
                .lower = null,
                .upper = null,
                .lower_bound = .unbounded,
                .upper_bound = .unbounded,
            };
        }

        /// Create a singleton interval [value, value].
        pub fn singleton(value: T) Self {
            return Self{
                .lower = value,
                .upper = value,
                .lower_bound = .closed,
                .upper_bound = .closed,
            };
        }

        /// Check if this interval is empty.
        pub fn isEmpty(self: Self) bool {
            if (self.lower_bound == .unbounded or self.upper_bound == .unbounded) {
                return false;
            }
            const l = self.lower orelse return false;
            const u = self.upper orelse return false;

            if (l > u) return true;
            if (l == u) {
                return self.lower_bound == .open or self.upper_bound == .open;
            }
            return false;
        }

        /// Check if this interval contains a value.
        pub fn contains(self: Self, value: T) bool {
            // Check lower bound
            if (self.lower) |l| {
                switch (self.lower_bound) {
                    .closed => if (value < l) return false,
                    .open => if (value <= l) return false,
                    .unbounded => {},
                }
            }

            // Check upper bound
            if (self.upper) |u| {
                switch (self.upper_bound) {
                    .closed => if (value > u) return false,
                    .open => if (value >= u) return false,
                    .unbounded => {},
                }
            }

            return true;
        }

        /// Check if this interval fully contains another interval.
        pub fn containsInterval(self: Self, other: Self) bool {
            // Check lower bound
            if (self.lower) |l| {
                if (other.lower) |ol| {
                    if (ol < l) return false;
                    if (ol == l and self.lower_bound == .open and other.lower_bound == .closed) {
                        return false;
                    }
                } else {
                    // other has no lower bound but self does
                    return false;
                }
            }

            // Check upper bound
            if (self.upper) |u| {
                if (other.upper) |ou| {
                    if (ou > u) return false;
                    if (ou == u and self.upper_bound == .open and other.upper_bound == .closed) {
                        return false;
                    }
                } else {
                    // other has no upper bound but self does
                    return false;
                }
            }

            return true;
        }

        /// Check if this interval overlaps with another.
        pub fn overlaps(self: Self, other: Self) bool {
            // Check if self's upper is below other's lower
            if (self.upper) |su| {
                if (other.lower) |ol| {
                    if (su < ol) return false;
                    if (su == ol) {
                        if (self.upper_bound == .open or other.lower_bound == .open) {
                            return false;
                        }
                    }
                }
            }

            // Check if other's upper is below self's lower
            if (other.upper) |ou| {
                if (self.lower) |sl| {
                    if (ou < sl) return false;
                    if (ou == sl) {
                        if (other.upper_bound == .open or self.lower_bound == .open) {
                            return false;
                        }
                    }
                }
            }

            return true;
        }

        /// Compute the intersection of two intervals.
        pub fn intersection(self: Self, other: Self) ?Self {
            if (!self.overlaps(other)) return null;

            // Determine new lower bound
            var new_lower: ?T = null;
            var new_lower_bound: BoundType = .unbounded;

            if (self.lower != null and other.lower != null) {
                const sl = self.lower.?;
                const ol = other.lower.?;
                if (sl > ol) {
                    new_lower = sl;
                    new_lower_bound = self.lower_bound;
                } else if (ol > sl) {
                    new_lower = ol;
                    new_lower_bound = other.lower_bound;
                } else {
                    new_lower = sl;
                    // Use more restrictive bound
                    new_lower_bound = if (self.lower_bound == .open or other.lower_bound == .open) .open else .closed;
                }
            } else if (self.lower != null) {
                new_lower = self.lower;
                new_lower_bound = self.lower_bound;
            } else if (other.lower != null) {
                new_lower = other.lower;
                new_lower_bound = other.lower_bound;
            }

            // Determine new upper bound
            var new_upper: ?T = null;
            var new_upper_bound: BoundType = .unbounded;

            if (self.upper != null and other.upper != null) {
                const su = self.upper.?;
                const ou = other.upper.?;
                if (su < ou) {
                    new_upper = su;
                    new_upper_bound = self.upper_bound;
                } else if (ou < su) {
                    new_upper = ou;
                    new_upper_bound = other.upper_bound;
                } else {
                    new_upper = su;
                    // Use more restrictive bound
                    new_upper_bound = if (self.upper_bound == .open or other.upper_bound == .open) .open else .closed;
                }
            } else if (self.upper != null) {
                new_upper = self.upper;
                new_upper_bound = self.upper_bound;
            } else if (other.upper != null) {
                new_upper = other.upper;
                new_upper_bound = other.upper_bound;
            }

            const result = Self{
                .lower = new_lower,
                .upper = new_upper,
                .lower_bound = new_lower_bound,
                .upper_bound = new_upper_bound,
            };

            if (result.isEmpty()) return null;
            return result;
        }

        /// Compute the span (convex hull) of two intervals.
        pub fn span(self: Self, other: Self) Self {
            // Determine new lower bound
            var new_lower: ?T = null;
            var new_lower_bound: BoundType = .unbounded;

            if (self.lower_bound == .unbounded or other.lower_bound == .unbounded) {
                new_lower_bound = .unbounded;
            } else if (self.lower != null and other.lower != null) {
                const sl = self.lower.?;
                const ol = other.lower.?;
                if (sl < ol) {
                    new_lower = sl;
                    new_lower_bound = self.lower_bound;
                } else if (ol < sl) {
                    new_lower = ol;
                    new_lower_bound = other.lower_bound;
                } else {
                    new_lower = sl;
                    // Use less restrictive bound
                    new_lower_bound = if (self.lower_bound == .closed or other.lower_bound == .closed) .closed else .open;
                }
            } else if (self.lower != null) {
                new_lower = self.lower;
                new_lower_bound = self.lower_bound;
            } else if (other.lower != null) {
                new_lower = other.lower;
                new_lower_bound = other.lower_bound;
            }

            // Determine new upper bound
            var new_upper: ?T = null;
            var new_upper_bound: BoundType = .unbounded;

            if (self.upper_bound == .unbounded or other.upper_bound == .unbounded) {
                new_upper_bound = .unbounded;
            } else if (self.upper != null and other.upper != null) {
                const su = self.upper.?;
                const ou = other.upper.?;
                if (su > ou) {
                    new_upper = su;
                    new_upper_bound = self.upper_bound;
                } else if (ou > su) {
                    new_upper = ou;
                    new_upper_bound = other.upper_bound;
                } else {
                    new_upper = su;
                    // Use less restrictive bound
                    new_upper_bound = if (self.upper_bound == .closed or other.upper_bound == .closed) .closed else .open;
                }
            } else if (self.upper != null) {
                new_upper = self.upper;
                new_upper_bound = self.upper_bound;
            } else if (other.upper != null) {
                new_upper = other.upper;
                new_upper_bound = other.upper_bound;
            }

            return Self{
                .lower = new_lower,
                .upper = new_upper,
                .lower_bound = new_lower_bound,
                .upper_bound = new_upper_bound,
            };
        }

        /// Get the length/size of the interval (upper - lower).
        /// Returns null for unbounded intervals.
        pub fn length(self: Self) ?T {
            const l = self.lower orelse return null;
            const u = self.upper orelse return null;
            return u - l;
        }

        /// Get the midpoint of the interval.
        /// Returns null for unbounded intervals.
        pub fn midpoint(self: Self) ?T {
            const l = self.lower orelse return null;
            const u = self.upper orelse return null;
            return l + @divTrunc(u - l, 2);
        }

        /// Clamp a value to this interval.
        pub fn clamp(self: Self, value: T) T {
            var result = value;

            if (self.lower) |l| {
                switch (self.lower_bound) {
                    .closed => if (result < l) {
                        result = l;
                    },
                    .open => if (result <= l) {
                        result = l + 1; // Best effort for open bound
                    },
                    .unbounded => {},
                }
            }

            if (self.upper) |u| {
                switch (self.upper_bound) {
                    .closed => if (result > u) {
                        result = u;
                    },
                    .open => if (result >= u) {
                        result = u - 1; // Best effort for open bound
                    },
                    .unbounded => {},
                }
            }

            return result;
        }

        /// Check if two intervals are equal.
        pub fn eql(self: Self, other: Self) bool {
            if (self.lower_bound != other.lower_bound) return false;
            if (self.upper_bound != other.upper_bound) return false;

            if (self.lower != null and other.lower != null) {
                if (self.lower.? != other.lower.?) return false;
            } else if (self.lower != null or other.lower != null) {
                return false;
            }

            if (self.upper != null and other.upper != null) {
                if (self.upper.? != other.upper.?) return false;
            } else if (self.upper != null or other.upper != null) {
                return false;
            }

            return true;
        }

        /// Check if this interval is a proper subset of another.
        pub fn isProperSubsetOf(self: Self, other: Self) bool {
            return other.containsInterval(self) and !self.eql(other);
        }

        /// Format the interval as a string.
        pub fn format(self: Self, buffer: []u8) []u8 {
            var pos: usize = 0;

            // Lower bound bracket
            buffer[pos] = if (self.lower_bound == .closed) '[' else '(';
            pos += 1;

            // Lower value
            if (self.lower) |l| {
                const written = std.fmt.bufPrint(buffer[pos..], "{d}", .{l}) catch return buffer[0..pos];
                pos += written.len;
            } else {
                @memcpy(buffer[pos..][0..4], "-inf");
                pos += 4;
            }

            // Separator
            @memcpy(buffer[pos..][0..2], ", ");
            pos += 2;

            // Upper value
            if (self.upper) |u| {
                const written = std.fmt.bufPrint(buffer[pos..], "{d}", .{u}) catch return buffer[0..pos];
                pos += written.len;
            } else {
                @memcpy(buffer[pos..][0..4], "+inf");
                pos += 4;
            }

            // Upper bound bracket
            buffer[pos] = if (self.upper_bound == .closed) ']' else ')';
            pos += 1;

            return buffer[0..pos];
        }
    };
}

/// Type alias for common interval types.
pub const IntInterval = Interval(i64);
pub const FloatInterval = Interval(f64);

/// Create a closed integer interval [lower, upper].
pub fn closedInt(lower: i64, upper: i64) IntervalError!IntInterval {
    return IntInterval.closed(lower, upper);
}

/// Create an open integer interval (lower, upper).
pub fn openInt(lower: i64, upper: i64) IntervalError!IntInterval {
    return IntInterval.open(lower, upper);
}

/// Create a closed float interval [lower, upper].
pub fn closedFloat(lower: f64, upper: f64) IntervalError!FloatInterval {
    return FloatInterval.closed(lower, upper);
}

/// Create an open float interval (lower, upper).
pub fn openFloat(lower: f64, upper: f64) IntervalError!FloatInterval {
    return FloatInterval.open(lower, upper);
}

/// Check if a value is in a closed range [lower, upper].
pub fn inRange(comptime T: type, value: T, lower: T, upper: T) bool {
    return value >= lower and value <= upper;
}

/// Check if a value is in an open range (lower, upper).
pub fn inOpenRange(comptime T: type, value: T, lower: T, upper: T) bool {
    return value > lower and value < upper;
}

/// Clamp a value to a closed range [lower, upper].
pub fn clampToRange(comptime T: type, value: T, lower: T, upper: T) T {
    return @max(lower, @min(upper, value));
}

test "Interval closed" {
    const interval = try IntInterval.closed(0, 10);
    try std.testing.expect(interval.contains(0));
    try std.testing.expect(interval.contains(5));
    try std.testing.expect(interval.contains(10));
    try std.testing.expect(!interval.contains(-1));
    try std.testing.expect(!interval.contains(11));
}

test "Interval open" {
    const interval = try IntInterval.open(0, 10);
    try std.testing.expect(!interval.contains(0));
    try std.testing.expect(interval.contains(5));
    try std.testing.expect(!interval.contains(10));
}

test "Interval half-open" {
    const interval = try IntInterval.closedOpen(0, 10);
    try std.testing.expect(interval.contains(0));
    try std.testing.expect(interval.contains(5));
    try std.testing.expect(!interval.contains(10));
}

test "Interval unbounded" {
    const at_least = IntInterval.atLeast(5);
    try std.testing.expect(!at_least.contains(4));
    try std.testing.expect(at_least.contains(5));
    try std.testing.expect(at_least.contains(1000));

    const at_most = IntInterval.atMost(5);
    try std.testing.expect(at_most.contains(-1000));
    try std.testing.expect(at_most.contains(5));
    try std.testing.expect(!at_most.contains(6));
}

test "Interval overlaps" {
    const a = try IntInterval.closed(0, 10);
    const b = try IntInterval.closed(5, 15);
    const c = try IntInterval.closed(11, 20);

    try std.testing.expect(a.overlaps(b));
    try std.testing.expect(b.overlaps(a));
    try std.testing.expect(!a.overlaps(c));
    try std.testing.expect(b.overlaps(c));
}

test "Interval intersection" {
    const a = try IntInterval.closed(0, 10);
    const b = try IntInterval.closed(5, 15);

    const inter = a.intersection(b) orelse unreachable;
    try std.testing.expectEqual(@as(?i64, 5), inter.lower);
    try std.testing.expectEqual(@as(?i64, 10), inter.upper);
}

test "Interval span" {
    const a = try IntInterval.closed(0, 5);
    const b = try IntInterval.closed(10, 15);

    const hull = a.span(b);
    try std.testing.expectEqual(@as(?i64, 0), hull.lower);
    try std.testing.expectEqual(@as(?i64, 15), hull.upper);
}

test "Interval length and midpoint" {
    const interval = try IntInterval.closed(0, 10);
    try std.testing.expectEqual(@as(?i64, 10), interval.length());
    try std.testing.expectEqual(@as(?i64, 5), interval.midpoint());
}

test "Interval clamp" {
    const interval = try IntInterval.closed(0, 10);
    try std.testing.expectEqual(@as(i64, 0), interval.clamp(-5));
    try std.testing.expectEqual(@as(i64, 5), interval.clamp(5));
    try std.testing.expectEqual(@as(i64, 10), interval.clamp(15));
}

test "Interval format" {
    var buffer: [64]u8 = undefined;

    const closed = try IntInterval.closed(0, 10);
    try std.testing.expectEqualStrings("[0, 10]", closed.format(&buffer));

    const open = try IntInterval.open(0, 10);
    try std.testing.expectEqualStrings("(0, 10)", open.format(&buffer));

    const unbounded = IntInterval.atLeast(5);
    try std.testing.expectEqualStrings("[5, +inf)", unbounded.format(&buffer));
}

test "Interval singleton" {
    const single = IntInterval.singleton(42);
    try std.testing.expect(single.contains(42));
    try std.testing.expect(!single.contains(41));
    try std.testing.expect(!single.contains(43));
    try std.testing.expectEqual(@as(?i64, 0), single.length());
}

test "Interval isEmpty" {
    // Open interval with equal bounds is empty
    const empty_open = IntInterval{
        .lower = 5,
        .upper = 5,
        .lower_bound = .open,
        .upper_bound = .open,
    };
    try std.testing.expect(empty_open.isEmpty());

    // Closed interval with equal bounds is not empty (singleton)
    const singleton = IntInterval.singleton(5);
    try std.testing.expect(!singleton.isEmpty());
}

test "inRange and clampToRange" {
    try std.testing.expect(inRange(i32, 5, 0, 10));
    try std.testing.expect(!inRange(i32, 15, 0, 10));
    try std.testing.expect(inOpenRange(i32, 5, 0, 10));
    try std.testing.expect(!inOpenRange(i32, 0, 0, 10));

    try std.testing.expectEqual(@as(i32, 0), clampToRange(i32, -5, 0, 10));
    try std.testing.expectEqual(@as(i32, 5), clampToRange(i32, 5, 0, 10));
    try std.testing.expectEqual(@as(i32, 10), clampToRange(i32, 15, 0, 10));
}
