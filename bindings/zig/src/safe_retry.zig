// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe retry strategies with backoff algorithms.

const std = @import("std");

pub const BackoffStrategy = union(enum) {
    fixed: u64,
    linear: struct { initial: u64, increment: u64 },
    exponential: struct { initial: u64, multiplier: u64 },
    jittered: struct { initial: u64, multiplier: u64, max_jitter: u64 },
};

/// Retry configuration.
pub const RetryConfig = struct {
    max_attempts: u32 = 3,
    backoff: BackoffStrategy = .{ .exponential = .{ .initial = 100, .multiplier = 2 } },
    max_delay: u64 = 30000,
};

/// Retry state.
pub const RetryState = struct {
    attempt: u32 = 0,
    total_delay: u64 = 0,
    last_error: ?u32 = null,
};

/// Calculate delay for a given attempt.
pub fn calculateDelay(strategy: BackoffStrategy, attempt: u32) u64 {
    return switch (strategy) {
        .fixed => |d| d,
        .linear => |l| l.initial + l.increment * attempt,
        .exponential => |e| e.initial * std.math.pow(u64, e.multiplier, attempt),
        .jittered => |j| j.initial * std.math.pow(u64, j.multiplier, attempt),
    };
}

/// Calculate delay with cap.
pub fn calculateDelayWithCap(config: RetryConfig, attempt: u32) u64 {
    const raw_delay = calculateDelay(config.backoff, attempt);
    return @min(config.max_delay, raw_delay);
}

/// Retry executor.
pub const RetryExecutor = struct {
    config: RetryConfig,
    state: RetryState,

    pub fn init(config: RetryConfig) RetryExecutor {
        return .{ .config = config, .state = .{} };
    }

    pub fn initDefault() RetryExecutor {
        return init(.{});
    }

    /// Check if more retries are allowed.
    pub fn canRetry(self: *const RetryExecutor) bool {
        return self.state.attempt < self.config.max_attempts;
    }

    /// Get remaining attempts.
    pub fn remainingAttempts(self: *const RetryExecutor) u32 {
        return self.config.max_attempts -| self.state.attempt;
    }

    /// Advance to next attempt, returning delay.
    pub fn nextAttempt(self: *RetryExecutor, error_code: ?u32) ?u64 {
        if (!self.canRetry()) return null;

        const delay = calculateDelayWithCap(self.config, self.state.attempt);
        self.state.attempt += 1;
        self.state.total_delay += delay;
        self.state.last_error = error_code;

        return delay;
    }

    /// Get current attempt number.
    pub fn currentAttempt(self: *const RetryExecutor) u32 {
        return self.state.attempt;
    }

    /// Get total delay accumulated.
    pub fn totalDelay(self: *const RetryExecutor) u64 {
        return self.state.total_delay;
    }

    /// Reset state.
    pub fn reset(self: *RetryExecutor) void {
        self.state = .{};
    }
};

/// Retry result.
pub fn RetryResult(comptime T: type) type {
    return union(enum) {
        success: T,
        exhausted: struct { attempts: u32, last_error: ?u32 },
        no_retry: u32,
    };
}

test "calculateDelay" {
    try std.testing.expectEqual(@as(u64, 100), calculateDelay(.{ .fixed = 100 }, 0));
    try std.testing.expectEqual(@as(u64, 100), calculateDelay(.{ .fixed = 100 }, 5));

    try std.testing.expectEqual(@as(u64, 100), calculateDelay(.{ .exponential = .{ .initial = 100, .multiplier = 2 } }, 0));
    try std.testing.expectEqual(@as(u64, 200), calculateDelay(.{ .exponential = .{ .initial = 100, .multiplier = 2 } }, 1));
    try std.testing.expectEqual(@as(u64, 400), calculateDelay(.{ .exponential = .{ .initial = 100, .multiplier = 2 } }, 2));
}

test "RetryExecutor" {
    var executor = RetryExecutor.init(.{ .max_attempts = 3 });

    try std.testing.expect(executor.canRetry());
    _ = executor.nextAttempt(null);
    try std.testing.expectEqual(@as(u32, 1), executor.currentAttempt());

    _ = executor.nextAttempt(null);
    _ = executor.nextAttempt(null);
    try std.testing.expect(!executor.canRetry());
}
