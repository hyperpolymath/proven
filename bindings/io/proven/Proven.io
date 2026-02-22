# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven.io - Main Proven prototype for the Io language.
#
# This file provides the top-level Proven prototype that acts as a namespace
# for all Proven submodules. All methods delegate to the native C addon
# (IoProven.c) which calls libproven via FFI.
#
# Usage:
#   Proven init
#   result := Proven safeAdd(100, 200)
#   result println
#   Proven deinit

Proven := Object clone do(
    //doc Proven category Safety
    //doc Proven description Formally verified safety library backed by Idris 2.

    # -----------------------------------------------------------------------
    # Lifecycle
    # -----------------------------------------------------------------------

    //doc Proven init Initialize the Proven runtime. Returns status code (0 = success).
    # init is provided by the native C addon (IoProven.c).

    //doc Proven deinit Shut down the Proven runtime.
    # deinit is provided by the native C addon (IoProven.c).

    //doc Proven isInitialized Check if the runtime is initialized.
    # isInitialized is provided by the native C addon (IoProven.c).

    //doc Proven version Get the library version string ("major.minor.patch").
    # version is provided by the native C addon (IoProven.c).

    # -----------------------------------------------------------------------
    # Convenience: Sub-modules as slots for organization
    # -----------------------------------------------------------------------

    //doc Proven SafeMath Returns the SafeMath prototype (safe integer arithmetic).
    //doc Proven SafeString Returns the SafeString prototype (encoding-safe text operations).
    //doc Proven SafeEmail Returns the SafeEmail prototype (email validation).
    //doc Proven SafeUrl Returns the SafeUrl prototype (URL parsing and encoding).
    //doc Proven SafeCrypto Returns the SafeCrypto prototype (cryptographic primitives).
    //doc Proven SafeJson Returns the SafeJson prototype (JSON validation).
)
