// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.hyperpolymath.proven

import groovy.transform.CompileStatic
import java.util.regex.Pattern

/**
 * Safe email address validation and manipulation.
 * Validates email addresses according to RFC 5321/5322.
 */
@CompileStatic
class SafeEmail {

    /** Maximum local part length per RFC 5321. */
    static final int MAX_LOCAL_LENGTH = 64

    /** Maximum domain length per RFC 5321. */
    static final int MAX_DOMAIN_LENGTH = 255

    /** Maximum total email length. */
    static final int MAX_EMAIL_LENGTH = 254

    /** Basic email pattern for validation. */
    private static final Pattern EMAIL_PATTERN = Pattern.compile(
        /^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/
    )

    /** Common disposable email domains. */
    private static final Set<String> DISPOSABLE_DOMAINS = [
        'mailinator.com', 'guerrillamail.com', 'tempmail.com', 'throwaway.email',
        '10minutemail.com', 'trashmail.com', 'fakeinbox.com', 'yopmail.com'
    ] as Set

    /**
     * Validate an email address.
     */
    static Result<Boolean, String> validate(String email) {
        if (email == null || email.isEmpty()) {
            return Result.err("Email is null or empty")
        }
        if (email.length() > MAX_EMAIL_LENGTH) {
            return Result.err("Email exceeds maximum length of ${MAX_EMAIL_LENGTH}")
        }

        def parts = split(email)
        if (parts.err) {
            return Result.err(parts.error)
        }

        def (localPart, domain) = parts.value

        if (localPart.length() > MAX_LOCAL_LENGTH) {
            return Result.err("Local part exceeds maximum length of ${MAX_LOCAL_LENGTH}")
        }
        if (domain.length() > MAX_DOMAIN_LENGTH) {
            return Result.err("Domain exceeds maximum length of ${MAX_DOMAIN_LENGTH}")
        }

        if (!EMAIL_PATTERN.matcher(email).matches()) {
            return Result.err("Invalid email format")
        }

        return Result.ok(true)
    }

    /**
     * Check if email is valid (simple boolean).
     */
    static boolean isValid(String email) {
        validate(email).match({ Boolean v -> v }, { String e -> false })
    }

    /**
     * Split email into local part and domain.
     */
    static Result<List<String>, String> split(String email) {
        if (email == null || email.isEmpty()) {
            return Result.err("Email is null or empty")
        }

        int atIndex = email.lastIndexOf('@')
        if (atIndex < 1) {
            return Result.err("Missing or misplaced @ symbol")
        }
        if (atIndex == email.length() - 1) {
            return Result.err("No domain after @ symbol")
        }

        String localPart = email.substring(0, atIndex)
        String domain = email.substring(atIndex + 1)

        return Result.ok([localPart, domain])
    }

    /**
     * Get the local part of an email address.
     */
    static Result<String, String> getLocalPart(String email) {
        split(email).map { List<String> parts -> parts[0] }
    }

    /**
     * Get the domain of an email address.
     */
    static Result<String, String> getDomain(String email) {
        split(email).map { List<String> parts -> parts[1] }
    }

    /**
     * Normalize an email address (lowercase domain, preserve local case).
     */
    static Result<String, String> normalize(String email) {
        def parts = split(email)
        if (parts.err) {
            return Result.err(parts.error)
        }

        def (localPart, domain) = parts.value
        return Result.ok("${localPart}@${domain.toLowerCase()}")
    }

    /**
     * Check if email uses a disposable domain.
     */
    static boolean isDisposable(String email) {
        getDomain(email).match(
            { String domain -> DISPOSABLE_DOMAINS.contains(domain.toLowerCase()) },
            { String err -> false }
        )
    }

    /**
     * Mask email for display (e.g., "j***@example.com").
     */
    static Result<String, String> mask(String email) {
        def parts = split(email)
        if (parts.err) {
            return Result.err(parts.error)
        }

        def (localPart, domain) = parts.value

        String masked
        if (localPart.length() <= 2) {
            masked = '*' * localPart.length()
        } else {
            masked = localPart[0] + '*' * (localPart.length() - 2) + localPart[-1]
        }

        return Result.ok("${masked}@${domain}")
    }

    /**
     * Create a mailto: URI.
     */
    static Result<String, String> toMailtoUri(String email, String subject = null, String body = null) {
        if (!isValid(email)) {
            return Result.err("Invalid email address")
        }

        StringBuilder uri = new StringBuilder("mailto:")
        uri.append(URLEncoder.encode(email, 'UTF-8').replace('+', '%20'))

        List<String> params = []
        if (subject != null) {
            params << "subject=${URLEncoder.encode(subject, 'UTF-8')}"
        }
        if (body != null) {
            params << "body=${URLEncoder.encode(body, 'UTF-8')}"
        }

        if (!params.isEmpty()) {
            uri.append('?')
            uri.append(params.join('&'))
        }

        return Result.ok(uri.toString())
    }

    /**
     * Check if domain has valid MX record format (basic check).
     */
    static boolean hasMxFormat(String domain) {
        if (domain == null || domain.isEmpty()) return false
        // Basic format check - actual MX lookup requires DNS
        domain.contains('.') && !domain.startsWith('.') && !domain.endsWith('.')
    }

    /**
     * Generate a simple hash of email for use as identifier.
     */
    static String toHash(String email) {
        normalize(email).match(
            { String normalized ->
                Integer.toHexString(normalized.hashCode() & 0x7FFFFFFF).padLeft(8, '0')
            },
            { String err -> "" }
        )
    }

    /**
     * Check if two emails are equivalent (after normalization).
     */
    static boolean equivalent(String email1, String email2) {
        def n1 = normalize(email1)
        def n2 = normalize(email2)
        n1.ok && n2.ok && n1.value == n2.value
    }
}
