// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe LRU (Least Recently Used) cache.

const std = @import("std");

/// LRU cache with bounded capacity.
pub fn LRUCache(comptime K: type, comptime V: type, comptime capacity: usize) type {
    return struct {
        const Self = @This();

        const Entry = struct {
            key: K,
            value: V,
            access_order: u64,
        };

        entries: [capacity]?Entry = [_]?Entry{null} ** capacity,
        counter: u64 = 0,
        len: usize = 0,

        pub fn init() Self {
            return .{};
        }

        pub fn size(self: *const Self) usize {
            return self.len;
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.len == 0;
        }

        pub fn isFull(self: *const Self) bool {
            return self.len >= capacity;
        }

        /// Get a value, updating access order.
        pub fn get(self: *Self, key: K) ?V {
            for (&self.entries) |*entry_opt| {
                if (entry_opt.*) |*entry| {
                    if (std.meta.eql(entry.key, key)) {
                        self.counter += 1;
                        entry.access_order = self.counter;
                        return entry.value;
                    }
                }
            }
            return null;
        }

        /// Peek at a value without updating access order.
        pub fn peek(self: *const Self, key: K) ?V {
            for (self.entries) |entry_opt| {
                if (entry_opt) |entry| {
                    if (std.meta.eql(entry.key, key)) {
                        return entry.value;
                    }
                }
            }
            return null;
        }

        /// Put a value, evicting LRU if full.
        pub fn put(self: *Self, key: K, value: V) void {
            self.counter += 1;

            // Check if key exists
            for (&self.entries) |*entry_opt| {
                if (entry_opt.*) |*entry| {
                    if (std.meta.eql(entry.key, key)) {
                        entry.value = value;
                        entry.access_order = self.counter;
                        return;
                    }
                }
            }

            // Find empty slot or LRU
            var target_idx: usize = 0;
            var min_order: u64 = std.math.maxInt(u64);

            for (self.entries, 0..) |entry_opt, i| {
                if (entry_opt == null) {
                    target_idx = i;
                    break;
                } else if (entry_opt.?.access_order < min_order) {
                    min_order = entry_opt.?.access_order;
                    target_idx = i;
                }
            }

            if (self.entries[target_idx] == null) {
                self.len += 1;
            }

            self.entries[target_idx] = Entry{
                .key = key,
                .value = value,
                .access_order = self.counter,
            };
        }

        /// Remove a key.
        pub fn remove(self: *Self, key: K) bool {
            for (&self.entries) |*entry_opt| {
                if (entry_opt.*) |entry| {
                    if (std.meta.eql(entry.key, key)) {
                        entry_opt.* = null;
                        self.len -= 1;
                        return true;
                    }
                }
            }
            return false;
        }

        /// Check if key exists.
        pub fn contains(self: *const Self, key: K) bool {
            return self.peek(key) != null;
        }

        /// Clear all entries.
        pub fn clear(self: *Self) void {
            self.entries = [_]?Entry{null} ** capacity;
            self.len = 0;
            self.counter = 0;
        }

        /// Get fill ratio.
        pub fn fillRatio(self: *const Self) f64 {
            return @as(f64, @floatFromInt(self.len)) / @as(f64, @floatFromInt(capacity));
        }
    };
}

test "LRUCache" {
    var cache = LRUCache(u32, []const u8, 3).init();

    cache.put(1, "one");
    cache.put(2, "two");
    cache.put(3, "three");

    try std.testing.expectEqualStrings("two", cache.get(2).?);

    // This should evict key 1 (least recently used)
    cache.put(4, "four");

    try std.testing.expect(cache.peek(1) == null);
    try std.testing.expect(cache.peek(2) != null);
}
