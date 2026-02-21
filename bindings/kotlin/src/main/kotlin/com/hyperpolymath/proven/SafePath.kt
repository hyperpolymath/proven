// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

/**
 * Safe filesystem path operations with traversal attack prevention.
 */
object SafePath {
    /**
     * Check if a path contains directory traversal sequences.
     */
    fun hasTraversal(path: String): Boolean =
        path.contains("..") || path.contains("~")

    /**
     * Check if a path is safe (no traversal attacks).
     */
    fun isSafe(path: String): Boolean = !hasTraversal(path)

    /**
     * Sanitize a filename by removing dangerous characters.
     */
    fun sanitizeFilename(filename: String): String =
        filename
            .replace("..", "_")
            .replace("/", "_")
            .replace("\\", "_")
            .replace("<", "_")
            .replace(">", "_")
            .replace(":", "_")
            .replace("\"", "_")
            .replace("|", "_")
            .replace("?", "_")
            .replace("*", "_")
            .replace("\u0000", "_")

    /**
     * Safely join path components, rejecting traversal attempts.
     * Returns null if any part contains traversal sequences.
     */
    fun safeJoin(base: String, parts: List<String>): String? {
        if (parts.any { hasTraversal(it) }) return null

        val sanitized = parts.map { sanitizeFilename(it) }
        var path = base

        for (part in sanitized) {
            path = if (path.endsWith("/")) {
                path + part
            } else {
                "$path/$part"
            }
        }

        return path
    }

    /**
     * Safely join path components (vararg version).
     */
    fun safeJoin(base: String, vararg parts: String): String? =
        safeJoin(base, parts.toList())
}
