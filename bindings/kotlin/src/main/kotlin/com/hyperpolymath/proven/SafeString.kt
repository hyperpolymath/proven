// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

/**
 * Safe string operations for escaping and sanitization.
 */
object SafeString {
    /**
     * Escape a string for safe HTML insertion.
     */
    fun escapeHtml(value: String): String =
        value
            .replace("&", "&amp;")
            .replace("<", "&lt;")
            .replace(">", "&gt;")
            .replace("\"", "&quot;")
            .replace("'", "&#x27;")

    /**
     * Escape a string for safe SQL interpolation.
     * Note: Prefer parameterized queries over string interpolation.
     */
    fun escapeSql(value: String): String =
        value.replace("'", "''")

    /**
     * Escape a string for safe JavaScript string literal insertion.
     */
    fun escapeJs(value: String): String =
        value
            .replace("\\", "\\\\")
            .replace("\"", "\\\"")
            .replace("'", "\\'")
            .replace("\n", "\\n")
            .replace("\r", "\\r")
            .replace("\t", "\\t")

    /**
     * Percent-encode a string for safe URL inclusion.
     */
    fun escapeUrl(value: String): String =
        java.net.URLEncoder.encode(value, Charsets.UTF_8)
            .replace("+", "%20")

    /**
     * Safely truncate a string to a maximum length, respecting UTF-8 boundaries.
     */
    fun truncateSafe(value: String, maxLength: Int, suffix: String = "..."): String {
        if (maxLength < 0) return ""
        if (value.length <= maxLength) return value

        val suffixLen = suffix.length
        if (maxLength <= suffixLen) return value.take(maxLength)

        return value.take(maxLength - suffixLen) + suffix
    }
}
