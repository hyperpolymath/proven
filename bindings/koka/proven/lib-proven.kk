// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// lib-proven.kk -- Core FFI declarations and result types for libproven.
//
// This module provides the low-level C FFI bindings to libproven. All
// computation is performed by the Idris 2 verified implementation via the
// Zig FFI bridge. No logic is reimplemented in Koka.
//
// Higher-level safe-* modules provide idiomatic Koka wrappers using
// `maybe<t>` for error handling and Koka's effect system.
module proven/lib-proven

import std/num/int64
import std/num/float64

// ============================================================================
// Status codes (matches ProvenStatus enum in C ABI)
// ============================================================================

// Status codes returned by libproven functions.
pub type proven-status
  Ok                    // 0: Operation succeeded
  ErrNullPointer        // -1: Null pointer passed
  ErrInvalidArgument    // -2: Invalid argument
  ErrOverflow           // -3: Integer overflow
  ErrUnderflow          // -4: Integer underflow
  ErrDivisionByZero     // -5: Division by zero
  ErrParseFailure       // -6: Parse failure
  ErrValidationFailed   // -7: Validation failed
  ErrOutOfBounds        // -8: Index out of bounds
  ErrEncodingError      // -9: Encoding error
  ErrAllocationFailed   // -10: Memory allocation failed
  ErrNotImplemented     // -99: Not implemented
  ErrUnknown(code: int32) // Unknown error code

// Convert a raw C status code to a proven-status value.
pub fun status-from-int32(code: int32): proven-status
  match code
    0i32  -> Ok
    -1i32 -> ErrNullPointer
    -2i32 -> ErrInvalidArgument
    -3i32 -> ErrOverflow
    -4i32 -> ErrUnderflow
    -5i32 -> ErrDivisionByZero
    -6i32 -> ErrParseFailure
    -7i32 -> ErrValidationFailed
    -8i32 -> ErrOutOfBounds
    -9i32 -> ErrEncodingError
    -10i32 -> ErrAllocationFailed
    -99i32 -> ErrNotImplemented
    other  -> ErrUnknown(other)

// Check if a status indicates success.
pub fun is-ok(s: proven-status): bool
  match s
    Ok -> True
    _  -> False

// ============================================================================
// Raw C FFI result structures
// ============================================================================

// Raw integer result from C FFI (status + value).
pub struct int-result
  status: int32
  value: int64

// Raw boolean result from C FFI (status + value).
pub struct bool-result
  status: int32
  value: int32  // C bool as int32

// Raw float result from C FFI (status + value).
pub struct float-result
  status: int32
  value: float64

// ============================================================================
// Result conversion helpers
// ============================================================================

// Convert a raw int-result to maybe<int64>.
// Returns Nothing on any error status.
pub fun int-result-to-maybe(r: int-result): maybe<int64>
  if r.status == 0i32 then Just(r.value)
  else Nothing

// Convert a raw bool-result to maybe<bool>.
// Returns Nothing on any error status.
pub fun bool-result-to-maybe(r: bool-result): maybe<bool>
  if r.status == 0i32 then Just(r.value != 0i32)
  else Nothing

// Convert a raw float-result to maybe<float64>.
// Returns Nothing on any error status.
pub fun float-result-to-maybe(r: float-result): maybe<float64>
  if r.status == 0i32 then Just(r.value)
  else Nothing

// ============================================================================
// Lifecycle FFI declarations
// ============================================================================

// Initialize the proven runtime (includes Idris 2 runtime).
// Must be called before any other proven function.
extern proven-init(): int32
  c inline "proven_init()"

// Deinitialize the proven runtime.
// Call when done using proven functions.
extern proven-deinit(): ()
  c inline "proven_deinit()"

// Check if the proven runtime is initialized.
extern proven-is-initialized(): bool
  c inline "proven_is_initialized()"

// ============================================================================
// Version FFI declarations
// ============================================================================

// Get the FFI ABI version number.
pub extern proven-abi-version(): int32
  c inline "(int32_t)proven_ffi_abi_version()"

// Get the major version number.
pub extern proven-version-major(): int32
  c inline "(int32_t)proven_version_major()"

// Get the minor version number.
pub extern proven-version-minor(): int32
  c inline "(int32_t)proven_version_minor()"

// Get the patch version number.
pub extern proven-version-patch(): int32
  c inline "(int32_t)proven_version_patch()"

// Get the total module count.
pub extern proven-module-count(): int32
  c inline "(int32_t)proven_module_count()"

// ============================================================================
// Memory management
// ============================================================================

// Free a string allocated by libproven.
// Must be called for every ProvenStringResult.value that is non-null.
extern proven-free-string(ptr: intptr_t): ()
  c inline "proven_free_string((char*)#1)"

// ============================================================================
// Safe lifecycle wrapper
// ============================================================================

// Initialize the proven runtime and return success status.
pub fun init(): proven-status
  val code = proven-init()
  status-from-int32(code)

// Deinitialize the proven runtime.
pub fun deinit(): ()
  proven-deinit()

// Check if the runtime is initialized.
pub fun is-initialized(): bool
  proven-is-initialized()
