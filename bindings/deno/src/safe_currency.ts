// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeCurrency - Currency operations.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * Note: proven_currency_parse returns CurrencyResult and
 * proven_currency_format takes [3]u8 + u8 structs, both requiring
 * buffer-based marshaling. This module provides the FFI infrastructure.
 *
 * @module
 */

// Note: proven_currency_* functions require complex struct marshaling.
// This module is a placeholder that will be fully wired when the
// buffer protocol for struct-by-value parameters is available.
// Currently no reimplemented logic.

export { getLib, isAvailable } from './ffi.ts';
