// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.hyperpolymath.proven

import groovy.transform.CompileStatic

/**
 * Safe string operations with validation and bounds checking.
 * Prevents common string manipulation vulnerabilities.
 */
@CompileStatic
class SafeString {

    /** Maximum allowed string length for safe operations. */
    static final int MAX_LENGTH = 1_000_000

    /** Default truncation length. */
    static final int DEFAULT_TRUNCATE_LENGTH = 256

    /**
     * Safely truncate a string to maximum length.
     */
    static Result<String, String> truncate(String input, int maxLength = DEFAULT_TRUNCATE_LENGTH) {
        if (input == null) {
            return Result.err("Input string is null")
        }
        if (maxLength < 0) {
            return Result.err("Max length must be non-negative")
        }
        if (input.length() <= maxLength) {
            return Result.ok(input)
        }
        return Result.ok(input.substring(0, maxLength))
    }

    /**
     * Safely get substring with bounds checking.
     */
    static Result<String, String> safeSubstring(String input, int start, int end) {
        if (input == null) {
            return Result.err("Input string is null")
        }
        if (start < 0 || end < 0) {
            return Result.err("Indices must be non-negative")
        }
        if (start > end) {
            return Result.err("Start index must not exceed end index")
        }
        if (end > input.length()) {
            return Result.err("End index exceeds string length")
        }
        return Result.ok(input.substring(start, end))
    }

    /**
     * Safely get character at index.
     */
    static Result<Character, String> charAt(String input, int index) {
        if (input == null) {
            return Result.err("Input string is null")
        }
        if (index < 0 || index >= input.length()) {
            return Result.err("Index out of bounds: ${index}")
        }
        return Result.ok(input.charAt(index))
    }

    /**
     * Check if string is null or empty.
     */
    static boolean isNullOrEmpty(String input) {
        input == null || input.isEmpty()
    }

    /**
     * Check if string is null, empty, or contains only whitespace.
     */
    static boolean isNullOrBlank(String input) {
        input == null || input.trim().isEmpty()
    }

    /**
     * Safely trim string with null handling.
     */
    static Result<String, String> safeTrim(String input) {
        if (input == null) {
            return Result.err("Input string is null")
        }
        return Result.ok(input.trim())
    }

    /**
     * Safely split string with limit.
     */
    static Result<List<String>, String> safeSplit(String input, String delimiter, int limit = -1) {
        if (input == null) {
            return Result.err("Input string is null")
        }
        if (delimiter == null) {
            return Result.err("Delimiter is null")
        }
        def parts = limit > 0 ? input.split(delimiter, limit) : input.split(delimiter)
        return Result.ok(parts.toList())
    }

    /**
     * Check if string contains only ASCII characters.
     */
    static boolean isAscii(String input) {
        if (input == null) return false
        input.chars().allMatch { int c -> c >= 0 && c < 128 }
    }

    /**
     * Check if string contains only printable ASCII.
     */
    static boolean isPrintableAscii(String input) {
        if (input == null) return false
        input.chars().allMatch { int c -> c >= 32 && c < 127 }
    }

    /**
     * Check if string contains only alphanumeric characters.
     */
    static boolean isAlphanumeric(String input) {
        if (input == null || input.isEmpty()) return false
        input.chars().allMatch { int c -> Character.isLetterOrDigit(c) }
    }

    /**
     * Check if string contains only digits.
     */
    static boolean isNumeric(String input) {
        if (input == null || input.isEmpty()) return false
        input.chars().allMatch { int c -> Character.isDigit(c) }
    }

    /**
     * Safely repeat a string with overflow protection.
     */
    static Result<String, String> repeat(String input, int count) {
        if (input == null) {
            return Result.err("Input string is null")
        }
        if (count < 0) {
            return Result.err("Count must be non-negative")
        }
        if (count == 0) {
            return Result.ok("")
        }
        long resultLength = (long) input.length() * count
        if (resultLength > MAX_LENGTH) {
            return Result.err("Result would exceed maximum length")
        }
        StringBuilder sb = new StringBuilder((int) resultLength)
        count.times { sb.append(input) }
        return Result.ok(sb.toString())
    }

    /**
     * Safely pad string on left.
     */
    static Result<String, String> padLeft(String input, int totalWidth, char padChar = ' ' as char) {
        if (input == null) {
            return Result.err("Input string is null")
        }
        if (totalWidth < 0 || totalWidth > MAX_LENGTH) {
            return Result.err("Invalid total width")
        }
        if (input.length() >= totalWidth) {
            return Result.ok(input)
        }
        return Result.ok(input.padLeft(totalWidth, String.valueOf(padChar)))
    }

    /**
     * Safely pad string on right.
     */
    static Result<String, String> padRight(String input, int totalWidth, char padChar = ' ' as char) {
        if (input == null) {
            return Result.err("Input string is null")
        }
        if (totalWidth < 0 || totalWidth > MAX_LENGTH) {
            return Result.err("Invalid total width")
        }
        if (input.length() >= totalWidth) {
            return Result.ok(input)
        }
        return Result.ok(input.padRight(totalWidth, String.valueOf(padChar)))
    }

    /**
     * Safely replace all occurrences with length check.
     */
    static Result<String, String> safeReplace(String input, String target, String replacement) {
        if (input == null || target == null || replacement == null) {
            return Result.err("Null argument provided")
        }
        if (target.isEmpty()) {
            return Result.err("Target string is empty")
        }
        String result = input.replace(target, replacement)
        if (result.length() > MAX_LENGTH) {
            return Result.err("Result exceeds maximum length")
        }
        return Result.ok(result)
    }

    /**
     * Escape HTML special characters.
     */
    static String escapeHtml(String input) {
        if (input == null) return ""
        input.replace("&", "&amp;")
             .replace("<", "&lt;")
             .replace(">", "&gt;")
             .replace("\"", "&quot;")
             .replace("'", "&#x27;")
    }

    /**
     * Escape for safe inclusion in JSON strings.
     */
    static String escapeJson(String input) {
        if (input == null) return ""
        StringBuilder sb = new StringBuilder()
        input.each { String c ->
            switch (c) {
                case '"': sb.append('\\"'); break
                case '\\': sb.append('\\\\'); break
                case '\b': sb.append('\\b'); break
                case '\f': sb.append('\\f'); break
                case '\n': sb.append('\\n'); break
                case '\r': sb.append('\\r'); break
                case '\t': sb.append('\\t'); break
                default:
                    int codePoint = c.codePointAt(0)
                    if (codePoint < 32) {
                        sb.append(String.format('\\u%04x', codePoint))
                    } else {
                        sb.append(c)
                    }
            }
        }
        sb.toString()
    }

    /**
     * Normalize whitespace (collapse multiple spaces to single).
     */
    static String normalizeWhitespace(String input) {
        if (input == null) return ""
        input.replaceAll(/\s+/, ' ').trim()
    }

    /**
     * Remove all non-printable characters.
     */
    static String removeNonPrintable(String input) {
        if (input == null) return ""
        input.replaceAll(/[\x00-\x1F\x7F]/, '')
    }

    /**
     * Levenshtein distance between two strings.
     */
    static Result<Integer, String> levenshteinDistance(String s1, String s2) {
        if (s1 == null || s2 == null) {
            return Result.err("Null string provided")
        }
        if (s1.length() > 10000 || s2.length() > 10000) {
            return Result.err("Strings too long for distance calculation")
        }

        int m = s1.length()
        int n = s2.length()

        if (m == 0) return Result.ok(n)
        if (n == 0) return Result.ok(m)

        int[] prev = new int[n + 1]
        int[] curr = new int[n + 1]

        for (int j = 0; j <= n; j++) {
            prev[j] = j
        }

        for (int i = 1; i <= m; i++) {
            curr[0] = i
            for (int j = 1; j <= n; j++) {
                int cost = s1.charAt(i - 1) == s2.charAt(j - 1) ? 0 : 1
                curr[j] = Math.min(Math.min(curr[j - 1] + 1, prev[j] + 1), prev[j - 1] + cost)
            }
            int[] temp = prev
            prev = curr
            curr = temp
        }

        return Result.ok(prev[n])
    }
}
