// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeVersion - Semantic version parsing and comparison.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * Note: proven_version_parse returns VersionResult (complex struct) and
 * proven_version_compare takes two SemanticVersion structs. Both require
 * buffer-based marshaling. This module provides the FFI infrastructure.
 *
 * @module
 */

// Note: proven_version_* functions require complex struct marshaling.
// This module is a placeholder that will be fully wired when the
// buffer protocol for struct-by-value parameters is available.
// Currently no reimplemented logic.

export { getLib, isAvailable } from './ffi.ts';
