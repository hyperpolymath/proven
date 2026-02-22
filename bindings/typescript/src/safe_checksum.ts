// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeChecksum - Typed re-exports from the JavaScript FFI binding.
 * Delegates all computation to libproven via the JavaScript FFI layer.
 * @module
 */

export { SafeChecksum, Crc32, Adler32, Fnv, Luhn } from '../../javascript/src/safe_checksum.js';
