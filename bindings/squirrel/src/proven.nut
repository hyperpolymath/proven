// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven - Squirrel class definitions for the Proven safety library.
//
// This file provides idiomatic Squirrel class wrappers around the native
// "proven" table registered by proven_squirrel.cpp. All computation is
// delegated to libproven via FFI; no logic is reimplemented here.
//
// Usage:
//   // The native "proven" table is registered by the host application.
//   // These classes provide a more structured OOP interface.
//
//   local math = SafeMath();
//   local result = math.add(1000000000, 2000000000);
//   if (result != null) print("Sum: " + result);

/**
 * Proven - Main entry point and lifecycle management.
 *
 * Wraps proven.init(), proven.deinit(), proven.is_initialized(),
 * and proven.version() from the native FFI table.
 */
class Proven {
    _initialized = false;

    /**
     * Initialize the Proven runtime.
     * @return {integer} Status code (0 = success).
     */
    function init() {
        local status = proven.init();
        if (status == 0) _initialized = true;
        return status;
    }

    /**
     * Deinitialize the Proven runtime.
     * Call when done using Proven functions.
     */
    function deinit() {
        proven.deinit();
        _initialized = false;
    }

    /**
     * Check if the runtime is initialized.
     * @return {bool} true if initialized.
     */
    function is_initialized() {
        return proven.is_initialized();
    }

    /**
     * Get the library version string.
     * @return {string} Version in "major.minor.patch" format.
     */
    function version() {
        return proven.version();
    }
}
