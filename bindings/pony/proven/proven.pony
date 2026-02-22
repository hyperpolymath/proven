// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven - Main package module providing lifecycle management.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This file is a thin wrapper; it does NOT reimplement any logic.
//
// Usage:
//   use "proven"
//
//   actor Main
//     new create(env: Env) =>
//       Proven.init()
//       match SafeMath.add(2, 3)
//       | let v: I64 => env.out.print("Sum: " + v.string())
//       | None => env.out.print("Overflow!")
//       end
//       Proven.deinit()

primitive Proven
  """
  Top-level lifecycle management for the Proven safety library.

  Call `Proven.init()` before using any other Proven functions,
  and `Proven.deinit()` when done.
  """

  fun init(): I32 =>
    """Initialize the Proven runtime. Returns 0 on success."""
    _LibProven.init()

  fun deinit(): None =>
    """Shut down the Proven runtime."""
    _LibProven.deinit()

  fun is_initialized(): Bool =>
    """Check if the runtime is initialized."""
    _LibProven.is_initialized()

  fun abi_version(): U32 =>
    """Get FFI ABI version for compatibility checking."""
    _LibProven.ffi_abi_version()

  fun version_major(): U32 =>
    """Get major version number."""
    _LibProven.version_major()

  fun version_minor(): U32 =>
    """Get minor version number."""
    _LibProven.version_minor()

  fun version_patch(): U32 =>
    """Get patch version number."""
    _LibProven.version_patch()

  fun module_count(): U32 =>
    """Get total module count."""
    _LibProven.module_count()

  fun version_string(): String =>
    """Get version as "major.minor.patch" string."""
    version_major().string() + "." +
    version_minor().string() + "." +
    version_patch().string()
