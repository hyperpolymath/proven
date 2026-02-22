// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

/**
 * Entry point for the Proven library.
 *
 * Provides lifecycle management (init/deinit) and version information.
 * The native library must be initialized before calling any other Proven functions.
 *
 * Usage:
 * <pre>{@code
 *     Proven.init();
 *     try {
 *         Optional<Long> sum = SafeMath.add(1, 2);
 *         // ... use other Safe* classes
 *     } finally {
 *         Proven.deinit();
 *     }
 * }</pre>
 *
 * All computation is performed in the Idris 2 verified core via
 * the Zig FFI layer. This Java binding is a thin JNA wrapper with
 * no reimplemented logic.
 */
public final class Proven {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private Proven() {}

    /**
     * Initialize the Proven library. Must be called before any other operations.
     *
     * @return true if initialization succeeded
     */
    public static boolean init() {
        return LIB.proven_init() == 0;
    }

    /**
     * Deinitialize the Proven library. Call when done to free resources.
     */
    public static void deinit() {
        LIB.proven_deinit();
    }

    /**
     * Check if the library has been initialized.
     *
     * @return true if the library is initialized and ready
     */
    public static boolean isInitialized() {
        return LIB.proven_is_initialized();
    }

    /**
     * Get the FFI ABI version number.
     *
     * @return the ABI version
     */
    public static int abiVersion() {
        return LIB.proven_ffi_abi_version();
    }

    /**
     * Get the library major version.
     *
     * @return the major version number
     */
    public static int versionMajor() {
        return LIB.proven_version_major();
    }

    /**
     * Get the library minor version.
     *
     * @return the minor version number
     */
    public static int versionMinor() {
        return LIB.proven_version_minor();
    }

    /**
     * Get the library patch version.
     *
     * @return the patch version number
     */
    public static int versionPatch() {
        return LIB.proven_version_patch();
    }

    /**
     * Get the library version as a string (e.g. "0.9.0").
     *
     * @return the version string
     */
    public static String version() {
        return versionMajor() + "." + versionMinor() + "." + versionPatch();
    }

    /**
     * Get the number of modules registered in the library.
     *
     * @return the module count
     */
    public static int moduleCount() {
        return LIB.proven_module_count();
    }
}
