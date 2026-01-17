// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.hyperpolymath.proven

import groovy.transform.CompileStatic
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.InvalidPathException

/**
 * Safe path operations with traversal prevention and validation.
 * Guards against path traversal attacks and validates file system paths.
 */
@CompileStatic
class SafePath {

    /** Maximum allowed path length. */
    static final int MAX_PATH_LENGTH = 4096

    /** Maximum number of path components. */
    static final int MAX_COMPONENTS = 256

    /** Dangerous path patterns. */
    private static final List<String> DANGEROUS_PATTERNS = [
        '..', './', '/.', '//', '\\\\', '\\.',
        '\\.', '%2e', '%2E', '%2f', '%2F', '%5c', '%5C'
    ]

    /**
     * Check if path contains traversal attempts.
     */
    static boolean hasTraversal(String path) {
        if (path == null) return false
        def normalized = path.toLowerCase()
        DANGEROUS_PATTERNS.any { pattern -> normalized.contains(pattern) }
    }

    /**
     * Validate and normalize a path, rejecting traversal attempts.
     */
    static Result<String, String> validate(String path) {
        if (path == null || path.trim().isEmpty()) {
            return Result.err("Path is null or empty")
        }
        if (path.length() > MAX_PATH_LENGTH) {
            return Result.err("Path exceeds maximum length")
        }
        if (hasTraversal(path)) {
            return Result.err("Path contains traversal pattern")
        }

        try {
            Path normalized = Paths.get(path).normalize()
            String result = normalized.toString()

            // Check if normalization resulted in traversal
            if (result.contains("..")) {
                return Result.err("Normalized path contains traversal")
            }

            return Result.ok(result)
        } catch (InvalidPathException e) {
            return Result.err("Invalid path: ${e.reason}")
        }
    }

    /**
     * Safely join path components with validation.
     */
    static Result<String, String> join(String base, String... components) {
        if (base == null) {
            return Result.err("Base path is null")
        }

        try {
            Path result = Paths.get(base)

            for (String component : components) {
                if (component == null) {
                    return Result.err("Path component is null")
                }
                if (hasTraversal(component)) {
                    return Result.err("Path component contains traversal")
                }
                result = result.resolve(component)
            }

            String normalized = result.normalize().toString()

            // Verify the result stays within base
            if (!normalized.startsWith(Paths.get(base).normalize().toString())) {
                return Result.err("Joined path escapes base directory")
            }

            if (normalized.length() > MAX_PATH_LENGTH) {
                return Result.err("Resulting path exceeds maximum length")
            }

            return Result.ok(normalized)
        } catch (InvalidPathException e) {
            return Result.err("Invalid path component: ${e.reason}")
        }
    }

    /**
     * Extract file extension safely.
     */
    static Result<String, String> getExtension(String path) {
        if (path == null || path.isEmpty()) {
            return Result.err("Path is null or empty")
        }

        String filename = getFilename(path).orElse { "" }
        if (filename.isEmpty()) {
            return Result.err("No filename in path")
        }

        int lastDot = filename.lastIndexOf('.')
        if (lastDot < 0 || lastDot == filename.length() - 1) {
            return Result.err("No extension found")
        }

        return Result.ok(filename.substring(lastDot + 1).toLowerCase())
    }

    /**
     * Extract filename from path safely.
     */
    static Result<String, String> getFilename(String path) {
        if (path == null || path.isEmpty()) {
            return Result.err("Path is null or empty")
        }

        try {
            Path p = Paths.get(path)
            Path fileName = p.getFileName()
            if (fileName == null) {
                return Result.err("No filename in path")
            }
            return Result.ok(fileName.toString())
        } catch (InvalidPathException e) {
            return Result.err("Invalid path: ${e.reason}")
        }
    }

    /**
     * Get parent directory safely.
     */
    static Result<String, String> getParent(String path) {
        if (path == null || path.isEmpty()) {
            return Result.err("Path is null or empty")
        }

        try {
            Path p = Paths.get(path)
            Path parent = p.getParent()
            if (parent == null) {
                return Result.err("No parent directory")
            }
            return Result.ok(parent.toString())
        } catch (InvalidPathException e) {
            return Result.err("Invalid path: ${e.reason}")
        }
    }

    /**
     * Check if path is absolute.
     */
    static boolean isAbsolute(String path) {
        if (path == null) return false
        try {
            return Paths.get(path).isAbsolute()
        } catch (InvalidPathException e) {
            return false
        }
    }

    /**
     * Check if a path is contained within a base directory.
     */
    static Result<Boolean, String> isContainedIn(String path, String base) {
        if (path == null || base == null) {
            return Result.err("Path or base is null")
        }

        try {
            Path normalizedPath = Paths.get(path).toAbsolutePath().normalize()
            Path normalizedBase = Paths.get(base).toAbsolutePath().normalize()

            return Result.ok(normalizedPath.startsWith(normalizedBase))
        } catch (InvalidPathException e) {
            return Result.err("Invalid path: ${e.reason}")
        }
    }

    /**
     * Sanitize filename by removing unsafe characters.
     */
    static String sanitizeFilename(String filename) {
        if (filename == null) return ""

        // Remove null bytes and control characters
        String cleaned = filename.replaceAll(/[\x00-\x1F\x7F]/, '')

        // Remove filesystem-unsafe characters
        cleaned = cleaned.replaceAll(/[<>:"\\/|?*]/, '_')

        // Prevent reserved names on Windows
        def reserved = ['CON', 'PRN', 'AUX', 'NUL',
                        'COM1', 'COM2', 'COM3', 'COM4', 'COM5', 'COM6', 'COM7', 'COM8', 'COM9',
                        'LPT1', 'LPT2', 'LPT3', 'LPT4', 'LPT5', 'LPT6', 'LPT7', 'LPT8', 'LPT9']
        if (reserved.contains(cleaned.toUpperCase())) {
            cleaned = "_${cleaned}"
        }

        // Remove leading/trailing dots and spaces
        cleaned = cleaned.replaceAll(/^[.\s]+|[.\s]+$/, '')

        // Truncate if too long
        if (cleaned.length() > 255) {
            cleaned = cleaned.substring(0, 255)
        }

        cleaned.isEmpty() ? "unnamed" : cleaned
    }

    /**
     * Split path into components.
     */
    static Result<List<String>, String> components(String path) {
        if (path == null) {
            return Result.err("Path is null")
        }

        try {
            Path p = Paths.get(path).normalize()
            List<String> result = []
            p.each { Path component -> result << component.toString() }

            if (result.size() > MAX_COMPONENTS) {
                return Result.err("Too many path components")
            }

            return Result.ok(result)
        } catch (InvalidPathException e) {
            return Result.err("Invalid path: ${e.reason}")
        }
    }

    /**
     * Check if path has specific extension (case-insensitive).
     */
    static boolean hasExtension(String path, String extension) {
        getExtension(path).match(
            { String ext -> ext.equalsIgnoreCase(extension.replaceFirst(/^\./, '')) },
            { String err -> false }
        )
    }

    /**
     * Make path relative to base.
     */
    static Result<String, String> relativize(String base, String path) {
        if (base == null || path == null) {
            return Result.err("Base or path is null")
        }

        try {
            Path basePath = Paths.get(base).normalize()
            Path targetPath = Paths.get(path).normalize()

            Path relative = basePath.relativize(targetPath)
            String result = relative.toString()

            if (hasTraversal(result)) {
                return Result.err("Relative path escapes base")
            }

            return Result.ok(result)
        } catch (Exception e) {
            return Result.err("Cannot relativize: ${e.message}")
        }
    }
}
