// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeUUID - UUID operations.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * Note: The UUID FFI functions deal with UUID struct (16 bytes) and are
 * handled via buffer-based marshaling. This module provides the FFI
 * infrastructure. Individual functions will be wired as the buffer
 * protocol is finalized.
 *
 * @module
 */

// Note: proven_uuid_* functions require 16-byte UUID struct marshaling
// via buffers. This module is a placeholder that will be fully wired
// when the buffer protocol for struct-by-value is available.
// Currently no reimplemented logic.

export { getLib, isAvailable } from './ffi.ts';
