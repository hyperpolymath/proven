// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.proven;

import java.nio.charset.StandardCharsets;
import java.util.regex.Pattern;

/**
 * Safe filesystem path operations that prevent traversal attacks.
 * Provides path sanitization and normalization.
 * Calls native verified code via JNI when available.
 */
public final class SafePath {
    private SafePath() {}

    /** Pattern to detect path traversal sequences. */
    private static final Pattern TRAVERSAL_PATTERN = Pattern.compile(
        "(^|[/\\\\])\\.\\.([/\\\\]|$)|" +    // ../
        "(^|[/\\\\])\\.$|" +                  // ./ at end
        "^~[/\\\\]|" +                        // ~/
        "[\\x00-\\x1f]"                       // Control characters
    );

    /** Characters not allowed in filenames. */
    private static final String UNSAFE_FILENAME_CHARS = "<>:\"/\\|?*\0";

    /**
     * Check if path contains directory traversal sequences.
     */
    public static boolean hasTraversal(String path) {
        if (path == null || path.isEmpty()) return false;
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativePathHasTraversal(path.getBytes(StandardCharsets.UTF_8));
        }
        return hasTraversalPure(path);
    }

    /**
     * Sanitize a filename by removing dangerous characters.
     */
    public static ProvenResult<String> sanitizeFilename(String filename) {
        if (filename == null || filename.isEmpty()) {
            return ProvenResult.err(ProvenStatus.ERR_EMPTY_INPUT, "Empty filename");
        }
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativePathSanitizeFilename(
                filename.getBytes(StandardCharsets.UTF_8), status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            if (s.isOk() && result != null) {
                return ProvenResult.ok(new String(result, StandardCharsets.UTF_8));
            }
            return ProvenResult.err(s);
        }
        return sanitizeFilenamePure(filename);
    }

    /**
     * Normalize a path (resolve . and .. components safely).
     */
    public static ProvenResult<String> normalize(String path) {
        if (path == null || path.isEmpty()) {
            return ProvenResult.err(ProvenStatus.ERR_EMPTY_INPUT, "Empty path");
        }
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativePathNormalize(
                path.getBytes(StandardCharsets.UTF_8), status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            if (s.isOk() && result != null) {
                return ProvenResult.ok(new String(result, StandardCharsets.UTF_8));
            }
            return ProvenResult.err(s);
        }
        return normalizePure(path);
    }

    /**
     * Check if a path is within a base directory (no escape).
     */
    public static boolean isContainedIn(String path, String baseDir) {
        if (path == null || baseDir == null) return false;
        if (hasTraversal(path)) return false;

        var normalizedPath = normalize(path);
        var normalizedBase = normalize(baseDir);

        if (normalizedPath.isErr() || normalizedBase.isErr()) {
            return false;
        }

        String p = normalizedPath.unwrap();
        String b = normalizedBase.unwrap();

        // Ensure base ends with separator for proper prefix matching
        if (!b.endsWith("/") && !b.endsWith("\\")) {
            b = b + "/";
        }

        return p.startsWith(b) || p.equals(normalizedBase.unwrap());
    }

    /**
     * Join path components safely.
     */
    public static ProvenResult<String> join(String base, String... parts) {
        if (base == null) {
            return ProvenResult.err(ProvenStatus.ERR_NULL_POINTER, "Base path is null");
        }

        StringBuilder result = new StringBuilder(base);
        for (String part : parts) {
            if (part == null) continue;
            if (hasTraversal(part)) {
                return ProvenResult.err(ProvenStatus.ERR_VALIDATION_FAILED,
                    "Path component contains traversal");
            }

            // Add separator if needed
            if (!result.isEmpty() &&
                !result.toString().endsWith("/") &&
                !result.toString().endsWith("\\") &&
                !part.startsWith("/") &&
                !part.startsWith("\\")) {
                result.append("/");
            }
            result.append(part);
        }

        return normalize(result.toString());
    }

    /**
     * Get file extension safely.
     */
    public static String getExtension(String path) {
        if (path == null || path.isEmpty()) return "";
        int lastDot = path.lastIndexOf('.');
        int lastSep = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
        if (lastDot > lastSep && lastDot < path.length() - 1) {
            return path.substring(lastDot + 1).toLowerCase();
        }
        return "";
    }

    /**
     * Get filename from path.
     */
    public static String getFilename(String path) {
        if (path == null || path.isEmpty()) return "";
        int lastSep = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
        if (lastSep >= 0 && lastSep < path.length() - 1) {
            return path.substring(lastSep + 1);
        }
        return path;
    }

    // Pure Java fallback implementations

    private static boolean hasTraversalPure(String path) {
        if (path.contains("..")) {
            // More thorough check
            return TRAVERSAL_PATTERN.matcher(path).find();
        }
        // Check for control characters
        for (int i = 0; i < path.length(); i++) {
            char c = path.charAt(i);
            if (c < 0x20 && c != '\t') return true;
        }
        return false;
    }

    private static ProvenResult<String> sanitizeFilenamePure(String filename) {
        StringBuilder sb = new StringBuilder(filename.length());
        for (int i = 0; i < filename.length(); i++) {
            char c = filename.charAt(i);
            if (UNSAFE_FILENAME_CHARS.indexOf(c) < 0 && c >= 0x20) {
                sb.append(c);
            } else {
                sb.append('_');
            }
        }

        String result = sb.toString().trim();
        if (result.isEmpty()) {
            return ProvenResult.err(ProvenStatus.ERR_VALIDATION_FAILED,
                "Filename contains only invalid characters");
        }

        // Prevent reserved Windows filenames
        String upper = result.toUpperCase();
        if (upper.matches("^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])(\\..*)?$")) {
            result = "_" + result;
        }

        return ProvenResult.ok(result);
    }

    private static ProvenResult<String> normalizePure(String path) {
        String normalized = path.replace('\\', '/');
        String[] parts = normalized.split("/");
        java.util.ArrayDeque<String> stack = new java.util.ArrayDeque<>();

        boolean absolute = normalized.startsWith("/");

        for (String part : parts) {
            if (part.isEmpty() || part.equals(".")) {
                continue;
            }
            if (part.equals("..")) {
                if (!stack.isEmpty() && !stack.peekLast().equals("..")) {
                    stack.removeLast();
                } else if (!absolute) {
                    stack.addLast("..");
                }
            } else {
                stack.addLast(part);
            }
        }

        String result = String.join("/", stack);
        if (absolute) {
            result = "/" + result;
        }
        if (result.isEmpty()) {
            result = ".";
        }

        return ProvenResult.ok(result);
    }
}
