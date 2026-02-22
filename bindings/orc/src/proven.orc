{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

{-
  proven.orc - Main module for proven library bindings in Orc

  Provides formally verified safety operations via FFI to libproven
  (Idris 2 + Zig). All computation is performed by the verified core;
  this module is a thin orchestration wrapper that marshals data across
  the FFI boundary using Orc's concurrent site-call mechanism.

  Architecture:
    Orc (.orc) concurrent orchestration
      | import site -> JNA bridge
      v
    libproven C ABI -> Zig FFI -> Idris 2

  Orc's strength is concurrent orchestration. This binding leverages that
  by allowing multiple proven calls to be composed concurrently using
  Orc's `|` (parallel), `>` (sequence), and pruning operators.
-}

include "src/ffi.orc"

-- ============================================================================
-- Library Version
-- ============================================================================

val PROVEN_VERSION_MAJOR = 0
val PROVEN_VERSION_MINOR = 4
val PROVEN_VERSION_PATCH = 0

-- ============================================================================
-- Error Handling
-- ============================================================================

-- ProvenError signal: published when an FFI call returns a non-zero status.
-- Orc programs can handle this with `try ... catch` or pruning.
def ProvenError(code, message) = Error("Proven[" + code + "]: " + message)

-- Translate a status code to a human-readable message.
def status_message(code) =
  if code = 0 then "Ok"
  else if code = -1 then "Null pointer"
  else if code = -2 then "Invalid argument"
  else if code = -3 then "Integer overflow"
  else if code = -4 then "Integer underflow"
  else if code = -5 then "Division by zero"
  else if code = -6 then "Parse failure"
  else if code = -7 then "Validation failed"
  else if code = -8 then "Out of bounds"
  else if code = -9 then "Encoding error"
  else if code = -10 then "Allocation failed"
  else if code = -99 then "Not implemented"
  else "Unknown error (" + code + ")"

-- ============================================================================
-- Lifecycle Management
-- ============================================================================

-- Initialize the proven runtime. Publishes true on success, halts on failure.
-- Must be called before any safe operation.
def init() =
  val status = ffi_init()
  if status = 0 then true
  else stop

-- Shut down the proven runtime and release resources.
def deinit() = ffi_deinit()

-- Check whether the runtime has been initialized.
def is_initialized() = ffi_is_initialized()

-- ============================================================================
-- Result Extraction Helpers
-- ============================================================================

-- Extract integer value from IntResult, or halt on error.
def extract_int(result) =
  if result.getStatus() = 0 then result.getValue()
  else stop

-- Extract boolean value from BoolResult, or halt on error.
def extract_bool(result) =
  if result.getStatus() = 0 then result.getValue()
  else stop

-- Extract string value from StringResult, free native memory, or halt on error.
def extract_string(result) =
  if result.getStatus() = 0 then
    val s = result.getString()
    ffi_free_string(result.getPtr())
    >> s
  else stop

-- Extract float value from FloatResult, or halt on error.
def extract_float(result) =
  if result.getStatus() = 0 then result.getValue()
  else stop

-- ============================================================================
-- Concurrent Composition Examples
-- ============================================================================

-- Run two proven operations concurrently and collect both results.
-- Orc publishes both values; the caller can prune or collect as needed.
--
-- Usage: concurrent_pair(safe_add(1, 2), safe_mul(3, 4))
-- Publishes: 3 and 12 (in either order)
def concurrent_pair(a, b) = a | b

-- Run a proven operation with a timeout (in milliseconds).
-- Publishes the result if it completes in time, otherwise halts.
def with_timeout(computation, ms) =
  val timer = Rwait(ms) >> stop
  computation | timer
