<?php
// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * Safe filesystem path operations with traversal attack prevention.
 */
class SafePath
{
    /** @var array<string> Dangerous path sequences */
    private const TRAVERSAL_PATTERNS = ['..', '~'];

    /** @var array<string> Characters dangerous in filenames */
    private const DANGEROUS_CHARS = ['/', '\\', '<', '>', ':', '"', '|', '?', '*', "\0"];

    /**
     * Check if a path contains directory traversal sequences.
     *
     * @param string $path The path to check
     * @return bool True if traversal detected
     */
    public static function hasTraversal(string $path): bool
    {
        foreach (self::TRAVERSAL_PATTERNS as $pattern) {
            if (str_contains($path, $pattern)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Check if a path is safe (no traversal attacks).
     *
     * @param string $path The path to check
     * @return bool True if safe
     */
    public static function isSafe(string $path): bool
    {
        return !self::hasTraversal($path);
    }

    /**
     * Sanitize a filename by removing dangerous characters.
     *
     * @param string $filename The filename to sanitize
     * @return string Sanitized filename
     */
    public static function sanitizeFilename(string $filename): string
    {
        $result = str_replace('..', '_', $filename);
        foreach (self::DANGEROUS_CHARS as $char) {
            $result = str_replace($char, '_', $result);
        }
        return $result;
    }

    /**
     * Safely join path components, rejecting traversal attempts.
     *
     * @param string $base Base path
     * @param string ...$parts Path components to join
     * @return string|null Joined path, or null if traversal detected
     */
    public static function safeJoin(string $base, string ...$parts): ?string
    {
        foreach ($parts as $part) {
            if (self::hasTraversal($part)) {
                return null;
            }
        }

        $sanitizedParts = array_map([self::class, 'sanitizeFilename'], $parts);

        $path = rtrim($base, '/\\');
        foreach ($sanitizedParts as $part) {
            $path .= DIRECTORY_SEPARATOR . $part;
        }

        return $path;
    }

    /**
     * Resolve a path to its canonical form and verify it's within a base directory.
     *
     * @param string $basePath The allowed base directory
     * @param string $userPath The user-supplied path
     * @return string|null Resolved path if safe, null if outside base or invalid
     */
    public static function resolveWithin(string $basePath, string $userPath): ?string
    {
        $realBase = realpath($basePath);
        if ($realBase === false) {
            return null;
        }

        $fullPath = $realBase . DIRECTORY_SEPARATOR . $userPath;
        $realPath = realpath($fullPath);

        if ($realPath === false) {
            return null;
        }

        // Ensure resolved path is within base
        if (!str_starts_with($realPath, $realBase . DIRECTORY_SEPARATOR) && $realPath !== $realBase) {
            return null;
        }

        return $realPath;
    }

    /**
     * Get a safe basename, stripping any directory components.
     *
     * @param string $path The path
     * @return string Just the filename portion
     */
    public static function safeBasename(string $path): string
    {
        // Handle both Unix and Windows separators
        $filename = basename(str_replace('\\', '/', $path));
        return self::sanitizeFilename($filename);
    }

    /**
     * Check if a filename has a safe extension.
     *
     * @param string $filename The filename to check
     * @param array<string> $allowedExtensions List of allowed extensions (without dots)
     * @return bool True if extension is in allowed list
     */
    public static function hasAllowedExtension(string $filename, array $allowedExtensions): bool
    {
        $ext = strtolower(pathinfo($filename, PATHINFO_EXTENSION));
        return in_array($ext, array_map('strtolower', $allowedExtensions), true);
    }
}
