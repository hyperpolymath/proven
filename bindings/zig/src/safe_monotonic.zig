// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeMonotonic - FFI bindings to libproven monotonic counter operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for monotonic operations.
pub const MonotonicError = error{
    CreationFailed,
    Overflow,
    ProvenError,
};

/// Monotonic counter backed by libproven.
pub const MonotonicCounter = struct {
    ptr: *c.ProvenMonotonicCounter,

    /// Get next value and increment via libproven.
    pub fn next(self: MonotonicCounter) MonotonicError!i64 {
        const result = c.proven_monotonic_next(self.ptr);
        return switch (result.status) {
            c.PROVEN_OK => result.value,
            c.PROVEN_ERR_OVERFLOW => error.Overflow,
            else => error.ProvenError,
        };
    }

    /// Free monotonic counter via libproven.
    pub fn deinit(self: MonotonicCounter) void {
        c.proven_monotonic_free(self.ptr);
    }
};

/// Create a monotonic counter via libproven.
pub fn create(initial: u64, max_value: u64) MonotonicError!MonotonicCounter {
    const ptr = c.proven_monotonic_create(initial, max_value);
    if (ptr == null) return error.CreationFailed;
    return MonotonicCounter{ .ptr = ptr.? };
}

test "create and next" {
    const counter = try create(0, 100);
    defer counter.deinit();
    const val = try counter.next();
    try std.testing.expectEqual(@as(i64, 0), val);
}
