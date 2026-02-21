// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

/**
 * Safe email validation and parsing operations.
 */
object SafeEmail {
    /**
     * Represents the parts of an email address.
     */
    data class EmailParts(
        val localPart: String,
        val domain: String
    )

    /**
     * Check if an email address is valid (basic check).
     */
    fun isValid(email: String): Boolean {
        val parts = email.split("@")
        if (parts.size != 2) return false

        val (localPart, domain) = parts
        if (localPart.isEmpty()) return false
        if (domain.length < 3) return false
        if (!domain.contains(".")) return false
        if (domain.startsWith(".")) return false
        if (domain.endsWith(".")) return false

        return true
    }

    /**
     * Split an email into local part and domain.
     */
    fun split(email: String): EmailParts? {
        if (!isValid(email)) return null

        val parts = email.split("@")
        return EmailParts(parts[0], parts[1])
    }

    /**
     * Extract the domain from an email address.
     */
    fun getDomain(email: String): String? = split(email)?.domain

    /**
     * Extract the local part from an email address.
     */
    fun getLocalPart(email: String): String? = split(email)?.localPart

    /**
     * Normalize an email address (lowercase domain).
     */
    fun normalize(email: String): String? {
        val parts = split(email) ?: return null
        return "${parts.localPart}@${parts.domain.lowercase()}"
    }
}
