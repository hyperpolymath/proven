// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

import com.sun.jna.Memory;

import java.nio.charset.StandardCharsets;
import java.util.Optional;

/**
 * Safe filesystem path operations via libproven FFI.
 *
 * Provides path traversal detection and filename sanitisation.
 * All logic delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No path-handling logic is reimplemented in Java.
 */
public final class SafePath {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private SafePath() {}

    /**
     * Check if a path contains directory traversal sequences (../, etc.).
     *
     * @param path the path to check
     * @return true if traversal is detected, empty on null pointer error
     */
    public static Optional<Boolean> hasTraversal(String path) {
        Memory mem = NativeUtil.toNativeString(path);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = path.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractBool(
            LIB.proven_path_has_traversal(mem, bytes.length));
    }

    /**
     * Sanitize a filename by removing dangerous characters.
     *
     * @param filename the filename to sanitize
     * @return the sanitized filename, or empty on error
     */
    public static Optional<String> sanitizeFilename(String filename) {
        Memory mem = NativeUtil.toNativeString(filename);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = filename.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractString(
            LIB.proven_path_sanitize_filename(mem, bytes.length));
    }
}
