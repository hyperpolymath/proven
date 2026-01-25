// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

@@warning("-27")

/**
 * Proven - Code that cannot crash
 *
 * ReScript FFI bindings for verified safety functions.
 * Calls Idris 2 verified code via Zig ABI (WASM/WASI).
 *
 * IMPORTANT: This is a minimal FFI-only version.
 * Language bindings MUST be thin wrappers, not reimplementations.
 * See ADR-008 in META.scm and ARCHITECTURE-CLEANUP-2026-01-25.md
 */

// ============================================================================
// FFI Layer - The ONLY layer that calls Idris2
// ============================================================================

module FFI = Proven_FFI

let version = "0.9.1-ffi"  // Breaking change from v0.9.0

/** Initialize the WASM module
 * @param wasmPath Path to the proven.wasm file
 */
let init = Proven_FFI.init

/** Check if initialized */
let isInitialized = Proven_FFI.isInitialized

/** Cleanup */
let deinit = Proven_FFI.deinit

/**
 * Architecture Note:
 *
 * This module previously contained 87 module references to "Safe*" modules.
 * Those modules were REIMPLEMENTATIONS in ReScript, not FFI wrappers.
 * They bypassed Idris2 verification entirely.
 *
 * They have been deleted (backup: .UNSAFE-ALL-BINDINGS-DELETED-FINAL-20260125/).
 *
 * Future work:
 * 1. Build proper Zig FFI bridge (ffi/zig/)
 * 2. Create thin FFI wrappers in ReScript that call the Zig bridge
 * 3. Add modules back here as FFI wrappers only
 *
 * Until then, only the FFI module is exported.
 */
