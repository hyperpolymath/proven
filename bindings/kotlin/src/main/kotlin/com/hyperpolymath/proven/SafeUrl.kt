// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import java.net.URI

/**
 * Safe URL parsing and validation operations.
 */
object SafeUrl {
    /**
     * Represents the parsed components of a URL.
     */
    data class ParsedUrl(
        val scheme: String,
        val host: String,
        val port: Int?,
        val path: String,
        val query: String?,
        val fragment: String?
    )

    /**
     * Parse a URL into its components.
     */
    fun parse(urlString: String): ParsedUrl? = try {
        val uri = URI(urlString)
        val scheme = uri.scheme ?: return null
        val host = uri.host ?: return null

        ParsedUrl(
            scheme = scheme.lowercase(),
            host = host,
            port = if (uri.port == -1) null else uri.port,
            path = uri.path.ifEmpty { "/" },
            query = uri.query,
            fragment = uri.fragment
        )
    } catch (e: Exception) {
        null
    }

    /**
     * Check if a string is a valid URL.
     */
    fun isValid(urlString: String): Boolean = parse(urlString) != null

    /**
     * Extract the host from a URL.
     */
    fun getHost(urlString: String): String? = parse(urlString)?.host

    /**
     * Extract the path from a URL.
     */
    fun getPath(urlString: String): String? = parse(urlString)?.path

    /**
     * Check if a URL uses HTTPS.
     */
    fun isHttps(urlString: String): Boolean =
        parse(urlString)?.scheme == "https"

    /**
     * Check if a URL uses a secure scheme (https, wss).
     */
    fun isSecure(urlString: String): Boolean {
        val scheme = parse(urlString)?.scheme ?: return false
        return scheme == "https" || scheme == "wss"
    }
}
