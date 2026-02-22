// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven - Entry point for the Proven Groovy binding.
// Lifecycle management and version info via libproven JNA.
package com.hyperpolymath.proven

import groovy.transform.CompileStatic

/**
 * Entry point for the Proven library.
 *
 * Provides lifecycle management (init/deinit) and version information.
 * The native library must be initialized before calling any other Proven functions.
 *
 * All computation is performed in the Idris 2 verified core via the Zig FFI layer.
 * This Groovy binding is a thin JNA wrapper with no reimplemented logic.
 */
@CompileStatic
class Proven {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /** Initialize the Proven library. Must be called before any other operations. */
    static boolean init() {
        LIB.proven_init() == 0
    }

    /** Deinitialize the Proven library. */
    static void deinit() {
        LIB.proven_deinit()
    }

    /** Check if the library has been initialized. */
    static boolean isInitialized() {
        LIB.proven_is_initialized()
    }

    /** Get the FFI ABI version number. */
    static int abiVersion() {
        LIB.proven_ffi_abi_version()
    }

    /** Get the library major version. */
    static int versionMajor() {
        LIB.proven_version_major()
    }

    /** Get the library minor version. */
    static int versionMinor() {
        LIB.proven_version_minor()
    }

    /** Get the library patch version. */
    static int versionPatch() {
        LIB.proven_version_patch()
    }

    /** Get the library version as a string (e.g. "0.9.0"). */
    static String version() {
        "${versionMajor()}.${versionMinor()}.${versionPatch()}"
    }

    /** Get the number of modules registered in the library. */
    static int moduleCount() {
        LIB.proven_module_count()
    }
}
