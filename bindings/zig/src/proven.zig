// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Proven - FFI bindings to libproven, a formally verified safety library.
//!
//! All computation is performed in verified Idris 2 code via libproven.
//! This Zig binding is a thin FFI wrapper; it does NOT reimplement any logic.
//!
//! Usage:
//!   const proven = @import("proven");
//!   proven.init();
//!   defer proven.deinit();
//!   const result = proven.safe_math.div(10, 0);

const std = @import("std");

/// Raw C FFI bindings from proven.h
pub const c = @cImport(@cInclude("proven.h"));

/// Library version (mirrors libproven)
pub const VERSION = c.PROVEN_VERSION_STRING;

/// Total module count in libproven
pub const MODULE_COUNT = c.PROVEN_MODULE_COUNT;

// ============================================================================
// Lifecycle
// ============================================================================

/// Initialize the Proven runtime (includes Idris 2 runtime).
/// Must be called before any other Proven function. Safe to call multiple times.
pub fn init() !void {
    const status = c.proven_init();
    if (status != c.PROVEN_OK) return error.InitFailed;
}

/// Cleanup the Proven runtime.
pub fn deinit() void {
    c.proven_deinit();
}

/// Check if the runtime is initialized.
pub fn isInitialized() bool {
    return c.proven_is_initialized();
}

/// Get FFI ABI version for compatibility checking.
pub fn ffiAbiVersion() u32 {
    return c.proven_ffi_abi_version();
}

// ============================================================================
// Core Modules (with libproven C ABI coverage)
// ============================================================================

pub const safe_math = @import("safe_math.zig");
pub const safe_string = @import("safe_string.zig");
pub const safe_path = @import("safe_path.zig");
pub const safe_email = @import("safe_email.zig");
pub const safe_url = @import("safe_url.zig");
pub const safe_network = @import("safe_network.zig");
pub const safe_crypto = @import("safe_crypto.zig");
pub const safe_uuid = @import("safe_uuid.zig");
pub const safe_currency = @import("safe_currency.zig");
pub const safe_phone = @import("safe_phone.zig");
pub const safe_hex = @import("safe_hex.zig");
pub const safe_json = @import("safe_json.zig");
pub const safe_datetime = @import("safe_datetime.zig");
pub const safe_float = @import("safe_float.zig");
pub const safe_version = @import("safe_version.zig");
pub const safe_color = @import("safe_color.zig");
pub const safe_angle = @import("safe_angle.zig");
pub const safe_unit = @import("safe_unit.zig");
pub const safe_header = @import("safe_header.zig");
pub const safe_cookie = @import("safe_cookie.zig");
pub const safe_content_type = @import("safe_content_type.zig");
pub const safe_password = @import("safe_password.zig");
pub const safe_geo = @import("safe_geo.zig");
pub const safe_checksum = @import("safe_checksum.zig");
pub const safe_probability = @import("safe_probability.zig");
pub const safe_calculator = @import("safe_calculator.zig");
pub const safe_buffer = @import("safe_buffer.zig");
pub const safe_rate_limiter = @import("safe_rate_limiter.zig");
pub const safe_circuit_breaker = @import("safe_circuit_breaker.zig");
pub const safe_retry = @import("safe_retry.zig");
pub const safe_monotonic = @import("safe_monotonic.zig");
pub const safe_state_machine = @import("safe_state_machine.zig");
pub const safe_tensor = @import("safe_tensor.zig");
pub const safe_ml = @import("safe_ml.zig");
pub const safe_lru = @import("safe_lru.zig");
pub const safe_graph = @import("safe_graph.zig");
pub const safe_queue = @import("safe_queue.zig");
pub const safe_bloom = @import("safe_bloom.zig");
pub const safe_http = @import("safe_http.zig");

// ============================================================================
// Modules without libproven C ABI coverage (placeholder stubs)
//
// These modules exist in the Zig binding but do not yet have corresponding
// C ABI functions in proven.h. They are thin stubs that return
// NotImplemented until libproven exposes the relevant functions.
// ============================================================================

pub const safe_env = @import("safe_env.zig");
pub const safe_file = @import("safe_file.zig");
pub const safe_finite_field = @import("safe_finite_field.zig");
pub const safe_dns = @import("safe_dns.zig");
pub const safe_decimal = @import("safe_decimal.zig");
pub const safe_toml = @import("safe_toml.zig");
pub const safe_cron = @import("safe_cron.zig");
pub const safe_complex = @import("safe_complex.zig");
pub const safe_rational = @import("safe_rational.zig");
pub const safe_bitset = @import("safe_bitset.zig");
pub const safe_heap = @import("safe_heap.zig");
pub const safe_tree = @import("safe_tree.zig");
pub const safe_union_find = @import("safe_union_find.zig");
pub const safe_consensus = @import("safe_consensus.zig");
pub const safe_semaphore = @import("safe_semaphore.zig");
pub const safe_transaction = @import("safe_transaction.zig");
pub const safe_ordering = @import("safe_ordering.zig");
pub const safe_cert = @import("safe_cert.zig");
pub const safe_command = @import("safe_command.zig");
pub const safe_jwt = @import("safe_jwt.zig");
pub const safe_policy = @import("safe_policy.zig");
pub const safe_regex = @import("safe_regex.zig");
pub const safe_git = @import("safe_git.zig");
pub const safe_ssh = @import("safe_ssh.zig");
pub const safe_resource = @import("safe_resource.zig");
pub const safe_provenance = @import("safe_provenance.zig");

test {
    std.testing.refAllDecls(@This());
}
