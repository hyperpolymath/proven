// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe monotonic counters and logical clocks.

const std = @import("std");

/// Monotonically increasing counter (can never decrease).
pub const MonotonicCounter = struct {
    value: u64,

    pub fn init() MonotonicCounter {
        return .{ .value = 0 };
    }

    pub fn fromValue(v: u64) MonotonicCounter {
        return .{ .value = v };
    }

    pub fn get(self: *const MonotonicCounter) u64 {
        return self.value;
    }

    pub fn increment(self: *MonotonicCounter) u64 {
        self.value += 1;
        return self.value;
    }

    pub fn incrementBy(self: *MonotonicCounter, amount: u64) u64 {
        self.value += amount;
        return self.value;
    }

    /// Advance to at least target value (keeps current if higher).
    pub fn advanceTo(self: *MonotonicCounter, target: u64) u64 {
        self.value = @max(self.value, target);
        return self.value;
    }
};

/// Lamport logical clock for distributed ordering.
pub const LamportClock = struct {
    timestamp: u64,
    node_id: u64,

    pub fn init(node_id: u64) LamportClock {
        return .{ .timestamp = 0, .node_id = node_id };
    }

    /// Local event: increment the clock.
    pub fn tick(self: *LamportClock) u64 {
        self.timestamp += 1;
        return self.timestamp;
    }

    /// Send event: increment and return timestamp.
    pub fn send(self: *LamportClock) u64 {
        return self.tick();
    }

    /// Receive event: update clock based on received timestamp.
    pub fn receive(self: *LamportClock, received: u64) u64 {
        self.timestamp = @max(self.timestamp, received) + 1;
        return self.timestamp;
    }

    /// Compare clocks (total ordering).
    pub fn compare(self: *const LamportClock, other: *const LamportClock) std.math.Order {
        if (self.timestamp != other.timestamp) {
            return std.math.order(self.timestamp, other.timestamp);
        }
        return std.math.order(self.node_id, other.node_id);
    }

    /// Check if event a happened before event b.
    pub fn happenedBefore(self: *const LamportClock, other: *const LamportClock) bool {
        return self.timestamp < other.timestamp;
    }
};

/// High-water mark for tracking processed offsets.
pub const HighWaterMark = struct {
    mark: u64,

    pub fn init() HighWaterMark {
        return .{ .mark = 0 };
    }

    pub fn get(self: *const HighWaterMark) u64 {
        return self.mark;
    }

    /// Update if offset is higher.
    pub fn update(self: *HighWaterMark, offset: u64) void {
        self.mark = @max(self.mark, offset);
    }

    /// Check if an offset has been processed.
    pub fn isProcessed(self: *const HighWaterMark, offset: u64) bool {
        return offset <= self.mark;
    }
};

/// Vector clock entry.
pub const VectorClockEntry = struct {
    node_id: u64,
    timestamp: u64,
};

/// Simple vector clock (fixed size for compile-time allocation).
pub fn VectorClock(comptime max_nodes: usize) type {
    return struct {
        const Self = @This();

        entries: [max_nodes]?VectorClockEntry = [_]?VectorClockEntry{null} ** max_nodes,
        self_id: u64,
        node_count: usize = 0,

        pub fn init(self_id: u64) Self {
            var vc = Self{ .self_id = self_id };
            vc.entries[0] = .{ .node_id = self_id, .timestamp = 0 };
            vc.node_count = 1;
            return vc;
        }

        fn findOrCreate(self: *Self, node_id: u64) ?*VectorClockEntry {
            for (&self.entries) |*entry_opt| {
                if (entry_opt.*) |*entry| {
                    if (entry.node_id == node_id) return entry;
                }
            }
            // Create new
            if (self.node_count < max_nodes) {
                for (&self.entries) |*entry_opt| {
                    if (entry_opt.* == null) {
                        entry_opt.* = .{ .node_id = node_id, .timestamp = 0 };
                        self.node_count += 1;
                        return &entry_opt.*.?;
                    }
                }
            }
            return null;
        }

        pub fn tick(self: *Self) void {
            if (self.findOrCreate(self.self_id)) |entry| {
                entry.timestamp += 1;
            }
        }

        pub fn merge(self: *Self, other_entries: []const VectorClockEntry) void {
            for (other_entries) |other| {
                if (self.findOrCreate(other.node_id)) |entry| {
                    entry.timestamp = @max(entry.timestamp, other.timestamp);
                }
            }
        }
    };
}

test "MonotonicCounter" {
    var counter = MonotonicCounter.init();
    try std.testing.expectEqual(@as(u64, 1), counter.increment());
    try std.testing.expectEqual(@as(u64, 2), counter.increment());
    try std.testing.expectEqual(@as(u64, 5), counter.advanceTo(5));
    try std.testing.expectEqual(@as(u64, 5), counter.advanceTo(3)); // No decrease
}

test "LamportClock" {
    var clock1 = LamportClock.init(1);
    var clock2 = LamportClock.init(2);

    _ = clock1.send();
    _ = clock2.receive(clock1.timestamp);

    try std.testing.expect(clock2.timestamp > clock1.timestamp);
}
