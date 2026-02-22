// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeColor - Color parsing, conversion, and manipulation.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * Note: proven_color_parse_hex returns ColorParseResult (status + 3 u8s),
 * proven_color_rgb_to_hsl and proven_color_to_hex take RGBColor structs.
 * Both require buffer-based marshaling. This module provides the FFI
 * infrastructure.
 *
 * @module
 */

// Note: proven_color_* functions require struct-by-value marshaling
// (RGBColor = 3 u8s, ColorParseResult = status + 3 u8s).
// This module is a placeholder that will be fully wired when the
// buffer protocol is available.
// Currently no reimplemented logic.

export { getLib, isAvailable } from './ffi.ts';
