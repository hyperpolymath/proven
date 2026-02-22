// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven - Main module with runtime lifecycle management.
// All computation is performed in verified Idris 2 code via the Zig FFI
// layer. This Wren class is a thin wrapper; it does NOT reimplement any logic.

// Proven runtime lifecycle management.
//
// Must call Proven.init() before using any other Proven classes.
// Call Proven.deinit() when done to clean up resources.
class Proven {
    // Initialize the Proven runtime (includes Idris 2 runtime).
    // Returns true on success. Safe to call multiple times.
    foreign static init()

    // Cleanup the Proven runtime.
    foreign static deinit()

    // Check if the runtime is initialized.
    foreign static isInitialized

    // Get the library version string ("major.minor.patch").
    foreign static version

    // Get the FFI ABI version for compatibility checking.
    foreign static abiVersion

    // Get the total number of modules in the library.
    foreign static moduleCount
}
