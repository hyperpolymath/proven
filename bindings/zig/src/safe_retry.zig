// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeRetry - FFI bindings to libproven retry operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Retry configuration.
pub const RetryConfig = struct {
    max_attempts: u32,
    base_delay_ms: u64,
    max_delay_ms: u64,
    multiplier: f64,
};

/// Calculate delay for a given attempt via libproven.
/// Returns delay in milliseconds (with jitter). 0 if attempt exceeds max.
pub fn delay(config: RetryConfig, attempt: u32) u64 {
    const c_config = c.ProvenRetryConfig{
        .max_attempts = config.max_attempts,
        .base_delay_ms = config.base_delay_ms,
        .max_delay_ms = config.max_delay_ms,
        .multiplier = config.multiplier,
    };
    return c.proven_retry_delay(c_config, attempt);
}

/// Check if retry should be attempted via libproven.
pub fn shouldRetry(config: RetryConfig, attempt: u32) bool {
    const c_config = c.ProvenRetryConfig{
        .max_attempts = config.max_attempts,
        .base_delay_ms = config.base_delay_ms,
        .max_delay_ms = config.max_delay_ms,
        .multiplier = config.multiplier,
    };
    return c.proven_retry_should_retry(c_config, attempt);
}

test "shouldRetry" {
    const config = RetryConfig{
        .max_attempts = 3,
        .base_delay_ms = 100,
        .max_delay_ms = 5000,
        .multiplier = 2.0,
    };
    try std.testing.expect(shouldRetry(config, 1));
    try std.testing.expect(shouldRetry(config, 2));
    try std.testing.expect(!shouldRetry(config, 4));
}

test "delay" {
    const config = RetryConfig{
        .max_attempts = 3,
        .base_delay_ms = 100,
        .max_delay_ms = 5000,
        .multiplier = 2.0,
    };
    const d = delay(config, 1);
    try std.testing.expect(d > 0);
}
