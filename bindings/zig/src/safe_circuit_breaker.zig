// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeCircuitBreaker - FFI bindings to libproven circuit breaker operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for circuit breaker operations.
pub const CircuitBreakerError = error{
    CreationFailed,
};

/// Circuit breaker state.
pub const CircuitState = enum(c_int) {
    closed = c.PROVEN_CIRCUIT_CLOSED,
    open = c.PROVEN_CIRCUIT_OPEN,
    half_open = c.PROVEN_CIRCUIT_HALF_OPEN,
};

/// Circuit breaker backed by libproven.
pub const CircuitBreaker = struct {
    ptr: *c.ProvenCircuitBreaker,

    /// Check if a request should be allowed via libproven.
    pub fn allow(self: CircuitBreaker) bool {
        return c.proven_circuit_breaker_allow(self.ptr);
    }

    /// Record a successful operation via libproven.
    pub fn recordSuccess(self: CircuitBreaker) void {
        c.proven_circuit_breaker_success(self.ptr);
    }

    /// Record a failed operation via libproven.
    pub fn recordFailure(self: CircuitBreaker) void {
        c.proven_circuit_breaker_failure(self.ptr);
    }

    /// Get current circuit state via libproven.
    pub fn state(self: CircuitBreaker) CircuitState {
        return @enumFromInt(c.proven_circuit_breaker_state(self.ptr));
    }

    /// Free circuit breaker via libproven.
    pub fn deinit(self: CircuitBreaker) void {
        c.proven_circuit_breaker_free(self.ptr);
    }
};

/// Create a circuit breaker via libproven.
pub fn create(failure_threshold: u32, success_threshold: u32, timeout_ms: i64) CircuitBreakerError!CircuitBreaker {
    const ptr = c.proven_circuit_breaker_create(failure_threshold, success_threshold, timeout_ms);
    if (ptr == null) return error.CreationFailed;
    return CircuitBreaker{ .ptr = ptr.? };
}

test "create" {
    const cb = try create(3, 2, 30000);
    defer cb.deinit();
    try std.testing.expectEqual(CircuitState.closed, cb.state());
    try std.testing.expect(cb.allow());
}
