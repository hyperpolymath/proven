// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

/**
 * HTTP header name with validation.
 */
@JvmInline
value class HeaderName private constructor(val value: String) {
    companion object {
        // Valid header name characters: token chars per RFC 7230
        private val VALID_TOKEN_CHARS = setOf(
            '!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '0', '1', '2', '3',
            '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
            'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
            'W', 'X', 'Y', 'Z', '^', '_', '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g',
            'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u',
            'v', 'w', 'x', 'y', 'z', '|', '~'
        )

        fun of(name: String): HeaderName? {
            if (name.isEmpty()) return null
            if (!name.all { it in VALID_TOKEN_CHARS }) return null
            return HeaderName(name)
        }

        // Standard headers
        val ACCEPT = HeaderName("Accept")
        val ACCEPT_CHARSET = HeaderName("Accept-Charset")
        val ACCEPT_ENCODING = HeaderName("Accept-Encoding")
        val ACCEPT_LANGUAGE = HeaderName("Accept-Language")
        val AUTHORIZATION = HeaderName("Authorization")
        val CACHE_CONTROL = HeaderName("Cache-Control")
        val CONNECTION = HeaderName("Connection")
        val CONTENT_DISPOSITION = HeaderName("Content-Disposition")
        val CONTENT_ENCODING = HeaderName("Content-Encoding")
        val CONTENT_LENGTH = HeaderName("Content-Length")
        val CONTENT_TYPE = HeaderName("Content-Type")
        val COOKIE = HeaderName("Cookie")
        val DATE = HeaderName("Date")
        val ETAG = HeaderName("ETag")
        val EXPIRES = HeaderName("Expires")
        val HOST = HeaderName("Host")
        val IF_MATCH = HeaderName("If-Match")
        val IF_MODIFIED_SINCE = HeaderName("If-Modified-Since")
        val IF_NONE_MATCH = HeaderName("If-None-Match")
        val LAST_MODIFIED = HeaderName("Last-Modified")
        val LOCATION = HeaderName("Location")
        val ORIGIN = HeaderName("Origin")
        val PRAGMA = HeaderName("Pragma")
        val RANGE = HeaderName("Range")
        val REFERER = HeaderName("Referer")
        val SET_COOKIE = HeaderName("Set-Cookie")
        val USER_AGENT = HeaderName("User-Agent")
        val VARY = HeaderName("Vary")
        val WWW_AUTHENTICATE = HeaderName("WWW-Authenticate")

        // Security headers
        val CONTENT_SECURITY_POLICY = HeaderName("Content-Security-Policy")
        val CONTENT_SECURITY_POLICY_REPORT_ONLY = HeaderName("Content-Security-Policy-Report-Only")
        val CROSS_ORIGIN_EMBEDDER_POLICY = HeaderName("Cross-Origin-Embedder-Policy")
        val CROSS_ORIGIN_OPENER_POLICY = HeaderName("Cross-Origin-Opener-Policy")
        val CROSS_ORIGIN_RESOURCE_POLICY = HeaderName("Cross-Origin-Resource-Policy")
        val PERMISSIONS_POLICY = HeaderName("Permissions-Policy")
        val REFERRER_POLICY = HeaderName("Referrer-Policy")
        val STRICT_TRANSPORT_SECURITY = HeaderName("Strict-Transport-Security")
        val X_CONTENT_TYPE_OPTIONS = HeaderName("X-Content-Type-Options")
        val X_FRAME_OPTIONS = HeaderName("X-Frame-Options")
        val X_XSS_PROTECTION = HeaderName("X-XSS-Protection")

        // CORS headers
        val ACCESS_CONTROL_ALLOW_CREDENTIALS = HeaderName("Access-Control-Allow-Credentials")
        val ACCESS_CONTROL_ALLOW_HEADERS = HeaderName("Access-Control-Allow-Headers")
        val ACCESS_CONTROL_ALLOW_METHODS = HeaderName("Access-Control-Allow-Methods")
        val ACCESS_CONTROL_ALLOW_ORIGIN = HeaderName("Access-Control-Allow-Origin")
        val ACCESS_CONTROL_EXPOSE_HEADERS = HeaderName("Access-Control-Expose-Headers")
        val ACCESS_CONTROL_MAX_AGE = HeaderName("Access-Control-Max-Age")
        val ACCESS_CONTROL_REQUEST_HEADERS = HeaderName("Access-Control-Request-Headers")
        val ACCESS_CONTROL_REQUEST_METHOD = HeaderName("Access-Control-Request-Method")
    }
}

/**
 * HTTP header value with validation.
 */
@JvmInline
value class HeaderValue private constructor(val value: String) {
    companion object {
        fun of(value: String): HeaderValue? {
            // Header values cannot contain null bytes or newlines
            if (value.contains('\u0000') || value.contains('\r') || value.contains('\n')) {
                return null
            }
            return HeaderValue(value.trim())
        }
    }
}

/**
 * HTTP header (name-value pair).
 */
data class Header(
    val name: HeaderName,
    val value: HeaderValue
) {
    override fun toString(): String = "${name.value}: ${value.value}"

    companion object {
        fun of(name: String, value: String): Header? {
            val headerName = HeaderName.of(name) ?: return null
            val headerValue = HeaderValue.of(value) ?: return null
            return Header(headerName, headerValue)
        }
    }
}

/**
 * HTTP headers collection.
 */
class Headers private constructor(
    private val headers: MutableList<Header>
) {
    val size: Int get() = headers.size

    operator fun get(name: HeaderName): String? {
        return headers.find { it.name.value.equals(name.value, ignoreCase = true) }?.value?.value
    }

    operator fun get(name: String): String? {
        return headers.find { it.name.value.equals(name, ignoreCase = true) }?.value?.value
    }

    fun getAll(name: HeaderName): List<String> {
        return headers.filter { it.name.value.equals(name.value, ignoreCase = true) }
            .map { it.value.value }
    }

    fun has(name: HeaderName): Boolean {
        return headers.any { it.name.value.equals(name.value, ignoreCase = true) }
    }

    fun has(name: String): Boolean {
        return headers.any { it.name.value.equals(name, ignoreCase = true) }
    }

    fun toList(): List<Header> = headers.toList()

    fun toMap(): Map<String, String> {
        return headers.associate { it.name.value to it.value.value }
    }

    fun toMultiMap(): Map<String, List<String>> {
        return headers.groupBy({ it.name.value }, { it.value.value })
    }

    companion object {
        fun empty(): Headers = Headers(mutableListOf())

        fun of(vararg pairs: Pair<String, String>): Headers? {
            val headers = mutableListOf<Header>()
            for ((name, value) in pairs) {
                val header = Header.of(name, value) ?: return null
                headers.add(header)
            }
            return Headers(headers)
        }

        fun parse(raw: String): Headers? {
            val headers = mutableListOf<Header>()
            val lines = raw.split("\r\n", "\n")

            for (line in lines) {
                if (line.isBlank()) continue
                val colonIndex = line.indexOf(':')
                if (colonIndex == -1) continue

                val name = line.substring(0, colonIndex)
                val value = line.substring(colonIndex + 1)
                val header = Header.of(name, value) ?: continue
                headers.add(header)
            }

            return Headers(headers)
        }
    }
}

/**
 * Header utilities.
 */
object SafeHeader {
    /**
     * Parse Content-Type header.
     */
    fun parseContentType(value: String): Pair<String, Map<String, String>>? {
        val parts = value.split(';').map { it.trim() }
        if (parts.isEmpty()) return null

        val mediaType = parts[0]
        val parameters = mutableMapOf<String, String>()

        for (param in parts.drop(1)) {
            val eqIndex = param.indexOf('=')
            if (eqIndex != -1) {
                val key = param.substring(0, eqIndex).trim()
                var paramValue = param.substring(eqIndex + 1).trim()
                // Remove quotes if present
                if (paramValue.startsWith('"') && paramValue.endsWith('"')) {
                    paramValue = paramValue.drop(1).dropLast(1)
                }
                parameters[key] = paramValue
            }
        }

        return mediaType to parameters
    }

    /**
     * Parse Accept header with quality values.
     */
    fun parseAccept(value: String): List<Pair<String, Double>> {
        return value.split(',')
            .map { it.trim() }
            .filter { it.isNotEmpty() }
            .map { part ->
                val semicolon = part.indexOf(';')
                if (semicolon == -1) {
                    part to 1.0
                } else {
                    val mediaType = part.substring(0, semicolon).trim()
                    val params = part.substring(semicolon + 1)
                    val qMatch = "q=([0-9.]+)".toRegex().find(params)
                    val quality = qMatch?.groupValues?.get(1)?.toDoubleOrNull() ?: 1.0
                    mediaType to quality.coerceIn(0.0, 1.0)
                }
            }
            .sortedByDescending { it.second }
    }

    /**
     * Parse Cache-Control header.
     */
    fun parseCacheControl(value: String): Map<String, String?> {
        val directives = mutableMapOf<String, String?>()

        for (part in value.split(',').map { it.trim() }) {
            val eqIndex = part.indexOf('=')
            if (eqIndex == -1) {
                directives[part.lowercase()] = null
            } else {
                val key = part.substring(0, eqIndex).trim().lowercase()
                var directiveValue = part.substring(eqIndex + 1).trim()
                if (directiveValue.startsWith('"') && directiveValue.endsWith('"')) {
                    directiveValue = directiveValue.drop(1).dropLast(1)
                }
                directives[key] = directiveValue
            }
        }

        return directives
    }

    /**
     * Build Strict-Transport-Security header.
     */
    fun buildHSTS(maxAge: Long, includeSubDomains: Boolean = true, preload: Boolean = false): String {
        val parts = mutableListOf("max-age=$maxAge")
        if (includeSubDomains) parts.add("includeSubDomains")
        if (preload) parts.add("preload")
        return parts.joinToString("; ")
    }

    /**
     * Build Content-Security-Policy header.
     */
    fun buildCSP(directives: Map<String, List<String>>): String {
        return directives.entries.joinToString("; ") { (key, values) ->
            "$key ${values.joinToString(" ")}"
        }
    }

    /**
     * Build Content-Disposition header.
     */
    fun buildContentDisposition(type: String, filename: String? = null): String {
        return if (filename != null) {
            "$type; filename=\"${escapeFilename(filename)}\""
        } else {
            type
        }
    }

    /**
     * Escape filename for Content-Disposition.
     */
    private fun escapeFilename(filename: String): String {
        return filename.replace("\"", "\\\"").replace("\r", "").replace("\n", "")
    }

    /**
     * Build Authorization header with Bearer token.
     */
    fun buildBearerAuth(token: String): String = "Bearer $token"

    /**
     * Build Authorization header with Basic auth.
     */
    fun buildBasicAuth(username: String, password: String): String {
        val credentials = "$username:$password"
        val encoded = java.util.Base64.getEncoder().encodeToString(credentials.toByteArray())
        return "Basic $encoded"
    }

    /**
     * Parse Authorization header.
     */
    fun parseAuthorization(value: String): Pair<String, String>? {
        val spaceIndex = value.indexOf(' ')
        if (spaceIndex == -1) return null
        val scheme = value.substring(0, spaceIndex)
        val credentials = value.substring(spaceIndex + 1).trim()
        return scheme to credentials
    }

    /**
     * Build Link header for pagination.
     */
    fun buildLinkHeader(links: Map<String, String>): String {
        return links.entries.joinToString(", ") { (rel, url) ->
            "<$url>; rel=\"$rel\""
        }
    }

    /**
     * Common security headers.
     */
    fun securityHeaders(): Headers {
        return Headers.of(
            "X-Content-Type-Options" to "nosniff",
            "X-Frame-Options" to "DENY",
            "X-XSS-Protection" to "1; mode=block",
            "Referrer-Policy" to "strict-origin-when-cross-origin",
            "Content-Security-Policy" to "default-src 'self'"
        ) ?: Headers.empty()
    }

    /**
     * CORS headers for all origins.
     */
    fun corsAllOrigins(): Headers {
        return Headers.of(
            "Access-Control-Allow-Origin" to "*",
            "Access-Control-Allow-Methods" to "GET, POST, PUT, DELETE, OPTIONS",
            "Access-Control-Allow-Headers" to "Content-Type, Authorization",
            "Access-Control-Max-Age" to "86400"
        ) ?: Headers.empty()
    }

    /**
     * CORS headers for specific origin.
     */
    fun corsForOrigin(origin: String, allowCredentials: Boolean = false): Headers {
        val headers = mutableListOf(
            "Access-Control-Allow-Origin" to origin,
            "Access-Control-Allow-Methods" to "GET, POST, PUT, DELETE, OPTIONS",
            "Access-Control-Allow-Headers" to "Content-Type, Authorization",
            "Access-Control-Max-Age" to "86400"
        )
        if (allowCredentials) {
            headers.add("Access-Control-Allow-Credentials" to "true")
        }
        return Headers.of(*headers.toTypedArray()) ?: Headers.empty()
    }
}
