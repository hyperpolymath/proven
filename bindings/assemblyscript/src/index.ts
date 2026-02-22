// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Proven Safety Primitives for AssemblyScript (WASM)
 *
 * This is the main entry point for the Proven AssemblyScript binding.
 * All computation is delegated to the formally verified libproven library
 * via WASM host function imports. The AssemblyScript code only provides
 * type-safe wrappers and data marshaling.
 *
 * Architecture:
 *   AssemblyScript wrapper (this) -> WASM host imports -> libproven.so
 *   (data marshaling only)          (host runtime)       (Idris 2 verified)
 *
 * The WASM host environment must provide all functions declared in the
 * "proven" import module (see ffi.ts for the complete list).
 */

// Re-export common types
export { ProvenStatus, Result, Option } from "./common";
export {
  RESULT_BUF,
  readIntResult,
  readBoolResult,
  readFloatResult,
  readStringResult,
  encodeString,
  encodedPtr,
  encodedLen,
} from "./common";

// Re-export domain modules
export { SafeMath } from "./safe_math";
export { SafeString } from "./safe_string";
export { SafePath } from "./safe_path";
export { SafeEmail } from "./safe_email";

// Re-export all FFI declarations for advanced users who want direct access
export * from "./ffi";

// ============================================================================
// LIFECYCLE
// ============================================================================

import {
  proven_init as _proven_init,
  proven_deinit as _proven_deinit,
  proven_is_initialized as _proven_is_initialized,
} from "./ffi";
import { ProvenStatus } from "./common";

/**
 * Initialize the Proven runtime (includes Idris 2 runtime).
 * Must be called before any other Proven function.
 * Safe to call multiple times.
 *
 * Delegates to proven_init().
 *
 * @returns ProvenStatus.Ok on success, error code otherwise.
 */
export function init(): ProvenStatus {
  return _proven_init() as ProvenStatus;
}

/**
 * Cleanup the Proven runtime.
 * Call when done using Proven functions.
 *
 * Delegates to proven_deinit().
 */
export function deinit(): void {
  _proven_deinit();
}

/**
 * Check if the Proven runtime is initialized.
 *
 * Delegates to proven_is_initialized().
 *
 * @returns true if initialized.
 */
export function isInitialized(): bool {
  return _proven_is_initialized() != 0;
}

// ============================================================================
// VERSION INFORMATION
// ============================================================================

import {
  proven_version_major as _proven_version_major,
  proven_version_minor as _proven_version_minor,
  proven_version_patch as _proven_version_patch,
  proven_ffi_abi_version as _proven_ffi_abi_version,
  proven_module_count as _proven_module_count,
} from "./ffi";

/**
 * Get major version number of libproven.
 * Delegates to proven_version_major().
 */
export function versionMajor(): u32 {
  return _proven_version_major();
}

/**
 * Get minor version number of libproven.
 * Delegates to proven_version_minor().
 */
export function versionMinor(): u32 {
  return _proven_version_minor();
}

/**
 * Get patch version number of libproven.
 * Delegates to proven_version_patch().
 */
export function versionPatch(): u32 {
  return _proven_version_patch();
}

/**
 * Get the FFI ABI version for compatibility checking.
 * Delegates to proven_ffi_abi_version().
 */
export function ffiAbiVersion(): u32 {
  return _proven_ffi_abi_version();
}

/**
 * Get total module count in libproven.
 * Delegates to proven_module_count().
 */
export function moduleCount(): u32 {
  return _proven_module_count();
}
