// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe circuit breaker pattern for fault tolerance.

const std = @import("std");

pub const CircuitState = enum {
    closed,    // Normal operation
    open,      // Failing, reject requests
    half_open, // Testing recovery
};

/// Circuit breaker configuration.
pub const CircuitConfig = struct {
    failure_threshold: u32 = 5,
    success_threshold: u32 = 2,
    timeout: u64 = 30,
    half_open_max_calls: u32 = 3,
};

/// Circuit breaker for fault tolerance.
pub const CircuitBreaker = struct {
    config: CircuitConfig,
    state: CircuitState,
    failures: u32,
    successes: u32,
    last_failure_time: u64,
    half_open_calls: u32,

    pub fn init(config: CircuitConfig) CircuitBreaker {
        return .{
            .config = config,
            .state = .closed,
            .failures = 0,
            .successes = 0,
            .last_failure_time = 0,
            .half_open_calls = 0,
        };
    }

    pub fn initDefault() CircuitBreaker {
        return init(.{});
    }

    /// Check if should transition to half-open.
    fn shouldTransitionToHalfOpen(self: *const CircuitBreaker, current_time: u64) bool {
        return self.state == .open and
            current_time >= self.last_failure_time + self.config.timeout;
    }

    /// Update state based on current time.
    pub fn updateState(self: *CircuitBreaker, current_time: u64) void {
        if (self.shouldTransitionToHalfOpen(current_time)) {
            self.state = .half_open;
            self.successes = 0;
            self.half_open_calls = 0;
        }
    }

    /// Check if a request can be executed.
    pub fn canExecute(self: *CircuitBreaker, current_time: u64) bool {
        self.updateState(current_time);

        return switch (self.state) {
            .closed => true,
            .open => false,
            .half_open => self.half_open_calls < self.config.half_open_max_calls,
        };
    }

    /// Record a successful call.
    pub fn recordSuccess(self: *CircuitBreaker) void {
        switch (self.state) {
            .closed => {
                self.failures = 0;
            },
            .half_open => {
                self.successes += 1;
                if (self.successes >= self.config.success_threshold) {
                    self.state = .closed;
                    self.failures = 0;
                    self.successes = 0;
                }
            },
            .open => {},
        }
    }

    /// Record a failed call.
    pub fn recordFailure(self: *CircuitBreaker, current_time: u64) void {
        self.last_failure_time = current_time;

        switch (self.state) {
            .closed => {
                self.failures += 1;
                if (self.failures >= self.config.failure_threshold) {
                    self.state = .open;
                }
            },
            .half_open => {
                self.state = .open;
                self.failures += 1;
            },
            .open => {
                self.failures += 1;
            },
        }
    }

    /// Record an attempt (for half-open tracking).
    pub fn recordAttempt(self: *CircuitBreaker) void {
        if (self.state == .half_open) {
            self.half_open_calls += 1;
        }
    }

    /// Execute with circuit breaker logic.
    pub fn execute(self: *CircuitBreaker, current_time: u64, success: bool) bool {
        self.updateState(current_time);

        if (!self.canExecute(current_time)) {
            return false;
        }

        self.recordAttempt();

        if (success) {
            self.recordSuccess();
        } else {
            self.recordFailure(current_time);
        }

        return true;
    }

    /// Check if circuit is healthy.
    pub fn isHealthy(self: *const CircuitBreaker) bool {
        return self.state == .closed;
    }

    /// Time until circuit might close.
    pub fn timeUntilRetry(self: *const CircuitBreaker, current_time: u64) u64 {
        if (self.state != .open) return 0;
        const retry_time = self.last_failure_time + self.config.timeout;
        return if (current_time >= retry_time) 0 else retry_time - current_time;
    }

    /// Force reset.
    pub fn reset(self: *CircuitBreaker) void {
        self.state = .closed;
        self.failures = 0;
        self.successes = 0;
        self.half_open_calls = 0;
    }
};

test "CircuitBreaker" {
    var cb = CircuitBreaker.init(.{ .failure_threshold = 3 });

    // Record failures
    _ = cb.execute(0, false);
    _ = cb.execute(1, false);
    try std.testing.expectEqual(CircuitState.closed, cb.state);

    _ = cb.execute(2, false);
    try std.testing.expectEqual(CircuitState.open, cb.state);
    try std.testing.expect(!cb.canExecute(3));
}
