// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeRateLimiter - FFI bindings to libproven rate limiting operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for rate limiter operations.
pub const RateLimiterError = error{
    CreationFailed,
};

/// Token bucket rate limiter backed by libproven.
pub const RateLimiter = struct {
    ptr: *c.ProvenRateLimiter,

    /// Try to acquire tokens from the rate limiter via libproven.
    /// Returns true if tokens were acquired, false if insufficient tokens.
    pub fn tryAcquire(self: RateLimiter, tokens: f64) bool {
        return c.proven_rate_limiter_try_acquire(self.ptr, tokens);
    }

    /// Free rate limiter via libproven.
    pub fn deinit(self: RateLimiter) void {
        c.proven_rate_limiter_free(self.ptr);
    }
};

/// Create a rate limiter via libproven.
/// capacity: Maximum token capacity.
/// refill_rate: Tokens per second to refill.
pub fn create(capacity: f64, refill_rate: f64) RateLimiterError!RateLimiter {
    const ptr = c.proven_rate_limiter_create(capacity, refill_rate);
    if (ptr == null) return error.CreationFailed;
    return RateLimiter{ .ptr = ptr.? };
}

test "create and acquire" {
    const limiter = try create(10.0, 1.0);
    defer limiter.deinit();
    try std.testing.expect(limiter.tryAcquire(5.0));
}
