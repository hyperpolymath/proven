// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeNetwork - Network address operations.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * Note: proven_network_parse_ipv4 returns a struct with an embedded array.
 * proven_network_ipv4_is_private and is_loopback take struct params.
 * These require buffer-based marshaling and will be completed when the
 * buffer protocol is finalized. This module provides the FFI infrastructure.
 *
 * @module
 */

// Note: The network FFI functions (proven_network_parse_ipv4,
// proven_network_ipv4_is_private, proven_network_ipv4_is_loopback)
// require struct-by-value parameters which need buffer-based marshaling.
// This module is a placeholder that will be wired when that protocol
// is available. Currently no reimplemented logic -- the module only
// re-exports the FFI infrastructure.

export { getLib, isAvailable } from './ffi.ts';
