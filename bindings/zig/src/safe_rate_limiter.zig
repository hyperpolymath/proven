// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe rate limiting algorithms.

const std = @import("std");

pub const RateLimitResult = union(enum) {
    allowed: void,
    denied: u64, // retry_after in time units
};

/// Token bucket rate limiter.
pub const TokenBucket = struct {
    capacity: u64,
    tokens: u64,
    refill_rate: u64, // tokens per time unit
    last_refill: u64,

    pub fn init(cap: u64, rate: u64) TokenBucket {
        return .{
            .capacity = cap,
            .tokens = cap,
            .refill_rate = rate,
            .last_refill = 0,
        };
    }

    /// Refill tokens based on elapsed time.
    pub fn refill(self: *TokenBucket, current_time: u64) void {
        const elapsed = current_time -| self.last_refill;
        const new_tokens = self.refill_rate * elapsed;
        self.tokens = @min(self.capacity, self.tokens + new_tokens);
        self.last_refill = current_time;
    }

    /// Try to acquire tokens.
    pub fn tryAcquire(self: *TokenBucket, count: u64, current_time: u64) RateLimitResult {
        self.refill(current_time);

        if (self.tokens >= count) {
            self.tokens -= count;
            return .allowed;
        }

        const needed = count - self.tokens;
        const wait_time = (needed + self.refill_rate - 1) / self.refill_rate;
        return .{ .denied = wait_time };
    }

    /// Check if request would be allowed (without consuming).
    pub fn wouldAllow(self: *TokenBucket, count: u64, current_time: u64) bool {
        var copy = self.*;
        copy.refill(current_time);
        return copy.tokens >= count;
    }

    /// Get current token count.
    pub fn currentTokens(self: *TokenBucket, current_time: u64) u64 {
        var copy = self.*;
        copy.refill(current_time);
        return copy.tokens;
    }
};

/// Sliding window rate limiter.
pub fn SlidingWindow(comptime max_entries: usize) type {
    return struct {
        const Self = @This();

        max_requests: u64,
        window_size: u64,
        requests: [max_entries]u64 = [_]u64{0} ** max_entries,
        request_count: usize = 0,

        pub fn init(max_req: u64, win_size: u64) Self {
            return .{
                .max_requests = max_req,
                .window_size = win_size,
            };
        }

        /// Prune expired requests.
        fn prune(self: *Self, current_time: u64) void {
            const cutoff = current_time -| self.window_size;
            var write_idx: usize = 0;

            for (self.requests[0..self.request_count]) |ts| {
                if (ts >= cutoff) {
                    self.requests[write_idx] = ts;
                    write_idx += 1;
                }
            }
            self.request_count = write_idx;
        }

        /// Try to make a request.
        pub fn tryRequest(self: *Self, current_time: u64) RateLimitResult {
            self.prune(current_time);

            if (self.request_count < self.max_requests and self.request_count < max_entries) {
                self.requests[self.request_count] = current_time;
                self.request_count += 1;
                return .allowed;
            }

            if (self.request_count == 0) {
                return .{ .denied = self.window_size };
            }

            // Find oldest request
            var oldest = self.requests[0];
            for (self.requests[1..self.request_count]) |ts| {
                oldest = @min(oldest, ts);
            }

            const retry_after = self.window_size -| (current_time -| oldest);
            return .{ .denied = retry_after };
        }

        /// Get current request count in window.
        pub fn currentCount(self: *Self, current_time: u64) usize {
            self.prune(current_time);
            return self.request_count;
        }

        /// Get remaining allowed requests.
        pub fn remaining(self: *Self, current_time: u64) u64 {
            self.prune(current_time);
            return self.max_requests -| @as(u64, @intCast(self.request_count));
        }
    };
}

/// Fixed window counter.
pub const FixedWindow = struct {
    max_requests: u64,
    window_size: u64,
    window_start: u64,
    count: u64,

    pub fn init(max_req: u64, win_size: u64) FixedWindow {
        return .{
            .max_requests = max_req,
            .window_size = win_size,
            .window_start = 0,
            .count = 0,
        };
    }

    pub fn tryRequest(self: *FixedWindow, current_time: u64) RateLimitResult {
        const window_end = self.window_start + self.window_size;

        // New window?
        if (current_time >= window_end) {
            self.window_start = (current_time / self.window_size) * self.window_size;
            self.count = 0;
        }

        if (self.count < self.max_requests) {
            self.count += 1;
            return .allowed;
        }

        const retry_after = (self.window_start + self.window_size) -| current_time;
        return .{ .denied = retry_after };
    }
};

test "TokenBucket" {
    var bucket = TokenBucket.init(10, 1);
    try std.testing.expectEqual(RateLimitResult.allowed, bucket.tryAcquire(5, 0));
    try std.testing.expectEqual(@as(u64, 5), bucket.currentTokens(0));
}

test "SlidingWindow" {
    var window = SlidingWindow(100).init(3, 10);
    try std.testing.expectEqual(RateLimitResult.allowed, window.tryRequest(0));
    try std.testing.expectEqual(RateLimitResult.allowed, window.tryRequest(1));
    try std.testing.expectEqual(RateLimitResult.allowed, window.tryRequest(2));
    try std.testing.expect(window.tryRequest(3) != .allowed);
}
