// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafePath - JNA wrapper for proven_path_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import com.sun.jna.Memory
import groovy.transform.CompileStatic

import java.nio.charset.StandardCharsets
import java.util.Optional

/**
 * Safe filesystem path operations via libproven JNA FFI.
 *
 * Every method delegates to the Idris 2 verified implementation.
 * No path logic is reimplemented in Groovy.
 */
@CompileStatic
class SafePath {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /**
     * Check if a path contains directory traversal sequences.
     */
    static Optional<Boolean> hasTraversal(String path) {
        Memory mem = NativeUtil.toNativeString(path)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = path.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractBool(
            LIB.proven_path_has_traversal(mem, bytes.length))
    }

    /**
     * Sanitize a filename by removing dangerous characters.
     */
    static Optional<String> sanitizeFilename(String filename) {
        Memory mem = NativeUtil.toNativeString(filename)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = filename.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractString(
            LIB.proven_path_sanitize_filename(mem, bytes.length))
    }
}
