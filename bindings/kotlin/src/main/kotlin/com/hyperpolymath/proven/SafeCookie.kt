// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import java.time.Instant
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import java.util.Locale

/**
 * SameSite attribute values.
 */
enum class SameSite(val value: String) {
    STRICT("Strict"),
    LAX("Lax"),
    NONE("None")
}

/**
 * HTTP Cookie with validation.
 */
data class Cookie(
    val name: String,
    val value: String,
    val domain: String? = null,
    val path: String? = null,
    val expires: Instant? = null,
    val maxAge: Long? = null,
    val secure: Boolean = false,
    val httpOnly: Boolean = false,
    val sameSite: SameSite? = null,
    val partitioned: Boolean = false
) {
    init {
        require(isValidName(name)) { "Invalid cookie name" }
        require(isValidValue(value)) { "Invalid cookie value" }
    }

    /**
     * Convert to Set-Cookie header value.
     */
    fun toSetCookieString(): String {
        val parts = mutableListOf("$name=$value")

        domain?.let { parts.add("Domain=$it") }
        path?.let { parts.add("Path=$it") }
        expires?.let { parts.add("Expires=${formatExpires(it)}") }
        maxAge?.let { parts.add("Max-Age=$it") }
        if (secure) parts.add("Secure")
        if (httpOnly) parts.add("HttpOnly")
        sameSite?.let { parts.add("SameSite=${it.value}") }
        if (partitioned) parts.add("Partitioned")

        return parts.joinToString("; ")
    }

    /**
     * Check if cookie is expired.
     */
    fun isExpired(now: Instant = Instant.now()): Boolean {
        expires?.let { return now.isAfter(it) }
        return false
    }

    /**
     * Check if cookie is session cookie (no expiration).
     */
    fun isSession(): Boolean = expires == null && maxAge == null

    companion object {
        private val RFC_1123_DATE_TIME = DateTimeFormatter
            .ofPattern("EEE, dd MMM yyyy HH:mm:ss z", Locale.ENGLISH)
            .withZone(ZoneOffset.UTC)

        // Cookie name must be a token (no separators, control chars)
        private val NAME_PATTERN = "^[!#$%&'*+\\-.0-9A-Z^_`a-z|~]+$".toRegex()

        // Cookie value restrictions
        private val INVALID_VALUE_CHARS = setOf('"', ',', ';', '\\', ' ')

        fun isValidName(name: String): Boolean {
            return name.isNotEmpty() && NAME_PATTERN.matches(name)
        }

        fun isValidValue(value: String): Boolean {
            return value.none { it in INVALID_VALUE_CHARS || it.code < 0x20 || it.code > 0x7E }
        }

        fun formatExpires(instant: Instant): String {
            return RFC_1123_DATE_TIME.format(instant)
        }

        /**
         * Parse a Set-Cookie header value.
         */
        fun parseSetCookie(header: String): Cookie? {
            val parts = header.split(";").map { it.trim() }
            if (parts.isEmpty()) return null

            // First part is name=value
            val nameValue = parts[0]
            val eqIndex = nameValue.indexOf('=')
            if (eqIndex == -1) return null

            val name = nameValue.substring(0, eqIndex)
            val value = nameValue.substring(eqIndex + 1)

            if (!isValidName(name)) return null

            var domain: String? = null
            var path: String? = null
            var expires: Instant? = null
            var maxAge: Long? = null
            var secure = false
            var httpOnly = false
            var sameSite: SameSite? = null
            var partitioned = false

            for (part in parts.drop(1)) {
                val attrEqIndex = part.indexOf('=')
                val attrName = if (attrEqIndex != -1) part.substring(0, attrEqIndex).trim().lowercase() else part.lowercase()
                val attrValue = if (attrEqIndex != -1) part.substring(attrEqIndex + 1).trim() else null

                when (attrName) {
                    "domain" -> domain = attrValue
                    "path" -> path = attrValue
                    "expires" -> {
                        attrValue?.let {
                            expires = try {
                                Instant.from(RFC_1123_DATE_TIME.parse(it))
                            } catch (e: Exception) {
                                null
                            }
                        }
                    }
                    "max-age" -> maxAge = attrValue?.toLongOrNull()
                    "secure" -> secure = true
                    "httponly" -> httpOnly = true
                    "samesite" -> {
                        sameSite = when (attrValue?.lowercase()) {
                            "strict" -> SameSite.STRICT
                            "lax" -> SameSite.LAX
                            "none" -> SameSite.NONE
                            else -> null
                        }
                    }
                    "partitioned" -> partitioned = true
                }
            }

            return try {
                Cookie(
                    name = name,
                    value = value,
                    domain = domain,
                    path = path,
                    expires = expires,
                    maxAge = maxAge,
                    secure = secure,
                    httpOnly = httpOnly,
                    sameSite = sameSite,
                    partitioned = partitioned
                )
            } catch (e: Exception) {
                null
            }
        }

        /**
         * Parse Cookie header (client-side cookies).
         */
        fun parseCookieHeader(header: String): Map<String, String> {
            val cookies = mutableMapOf<String, String>()

            for (part in header.split(";").map { it.trim() }) {
                val eqIndex = part.indexOf('=')
                if (eqIndex != -1) {
                    val name = part.substring(0, eqIndex).trim()
                    val value = part.substring(eqIndex + 1).trim()
                    if (isValidName(name)) {
                        cookies[name] = value
                    }
                }
            }

            return cookies
        }
    }
}

/**
 * Cookie jar for managing cookies.
 */
class CookieJar {
    private val cookies = mutableMapOf<String, Cookie>()

    /**
     * Get key for cookie (name + domain + path).
     */
    private fun cookieKey(cookie: Cookie): String {
        return "${cookie.name}|${cookie.domain ?: ""}|${cookie.path ?: "/"}"
    }

    /**
     * Add or update a cookie.
     */
    fun set(cookie: Cookie) {
        val key = cookieKey(cookie)
        // Remove expired cookies first
        if (cookie.isExpired()) {
            cookies.remove(key)
        } else {
            cookies[key] = cookie
        }
    }

    /**
     * Get a cookie by name.
     */
    fun get(name: String): Cookie? {
        return cookies.values.find { it.name == name && !it.isExpired() }
    }

    /**
     * Get all cookies for a domain and path.
     */
    fun getCookiesFor(domain: String, path: String = "/"): List<Cookie> {
        return cookies.values.filter { cookie ->
            !cookie.isExpired() &&
            (cookie.domain == null || domain.endsWith(cookie.domain!!)) &&
            (cookie.path == null || path.startsWith(cookie.path!!))
        }
    }

    /**
     * Get all non-expired cookies.
     */
    fun getAll(): List<Cookie> {
        return cookies.values.filter { !it.isExpired() }
    }

    /**
     * Remove a cookie.
     */
    fun remove(name: String, domain: String? = null, path: String? = null) {
        val keysToRemove = cookies.keys.filter { key ->
            val parts = key.split("|")
            parts[0] == name &&
            (domain == null || parts.getOrNull(1) == domain) &&
            (path == null || parts.getOrNull(2) == path)
        }
        keysToRemove.forEach { cookies.remove(it) }
    }

    /**
     * Remove all expired cookies.
     */
    fun removeExpired() {
        val now = Instant.now()
        cookies.entries.removeIf { it.value.isExpired(now) }
    }

    /**
     * Clear all cookies.
     */
    fun clear() {
        cookies.clear()
    }

    /**
     * Get count of non-expired cookies.
     */
    fun count(): Int = cookies.values.count { !it.isExpired() }

    /**
     * Build Cookie header for request.
     */
    fun toCookieHeader(domain: String, path: String = "/"): String {
        return getCookiesFor(domain, path).joinToString("; ") { "${it.name}=${it.value}" }
    }
}

/**
 * Cookie utilities.
 */
object SafeCookie {
    /**
     * Create a session cookie.
     */
    fun session(name: String, value: String, httpOnly: Boolean = true, secure: Boolean = true): Cookie? {
        return try {
            Cookie(
                name = name,
                value = value,
                httpOnly = httpOnly,
                secure = secure,
                sameSite = SameSite.LAX
            )
        } catch (e: Exception) {
            null
        }
    }

    /**
     * Create a persistent cookie.
     */
    fun persistent(
        name: String,
        value: String,
        maxAgeSeconds: Long,
        httpOnly: Boolean = true,
        secure: Boolean = true
    ): Cookie? {
        return try {
            Cookie(
                name = name,
                value = value,
                maxAge = maxAgeSeconds,
                httpOnly = httpOnly,
                secure = secure,
                sameSite = SameSite.LAX
            )
        } catch (e: Exception) {
            null
        }
    }

    /**
     * Create a secure cookie for authentication.
     */
    fun authCookie(name: String, value: String, maxAgeSeconds: Long): Cookie? {
        return try {
            Cookie(
                name = name,
                value = value,
                maxAge = maxAgeSeconds,
                httpOnly = true,
                secure = true,
                sameSite = SameSite.STRICT,
                path = "/"
            )
        } catch (e: Exception) {
            null
        }
    }

    /**
     * Create a cookie to delete an existing cookie.
     */
    fun deletionCookie(name: String, domain: String? = null, path: String? = "/"): Cookie {
        return Cookie(
            name = name,
            value = "",
            domain = domain,
            path = path,
            maxAge = 0,
            expires = Instant.EPOCH
        )
    }

    /**
     * Check if cookie name is valid.
     */
    fun isValidName(name: String): Boolean = Cookie.isValidName(name)

    /**
     * Check if cookie value is valid.
     */
    fun isValidValue(value: String): Boolean = Cookie.isValidValue(value)

    /**
     * URL-encode a cookie value.
     */
    fun encodeValue(value: String): String {
        return java.net.URLEncoder.encode(value, "UTF-8")
    }

    /**
     * URL-decode a cookie value.
     */
    fun decodeValue(value: String): String {
        return java.net.URLDecoder.decode(value, "UTF-8")
    }
}
