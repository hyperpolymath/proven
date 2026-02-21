// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.proven;

import java.nio.charset.StandardCharsets;
import java.util.regex.Pattern;

/**
 * Safe email validation following RFC 5321.
 * Provides email parsing and normalization.
 * Calls native verified code via JNI when available.
 */
public final class SafeEmail {
    private SafeEmail() {}

    /** Maximum email length per RFC 5321. */
    public static final int MAX_EMAIL_LENGTH = 254;

    /** Maximum local part length. */
    public static final int MAX_LOCAL_PART_LENGTH = 64;

    /** Maximum domain length. */
    public static final int MAX_DOMAIN_LENGTH = 253;

    /** Basic email validation pattern. */
    private static final Pattern EMAIL_PATTERN = Pattern.compile(
        "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?" +
        "(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
    );

    /**
     * Parsed email address components.
     */
    public record EmailComponents(String localPart, String domain) {
        public String toAddress() {
            return localPart + "@" + domain;
        }
    }

    /**
     * Check if email address is valid (RFC 5321 simplified).
     */
    public static boolean isValid(String email) {
        if (email == null || email.isEmpty()) return false;
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativeEmailIsValid(email.getBytes(StandardCharsets.UTF_8));
        }
        return isValidPure(email);
    }

    /**
     * Parse email address into components.
     */
    public static ProvenResult<EmailComponents> parse(String email) {
        if (email == null || email.isEmpty()) {
            return ProvenResult.err(ProvenStatus.ERR_EMPTY_INPUT, "Empty email");
        }
        if (!isValid(email)) {
            return ProvenResult.err(ProvenStatus.ERR_VALIDATION_FAILED, "Invalid email format");
        }

        int atIndex = email.lastIndexOf('@');
        if (atIndex < 0) {
            return ProvenResult.err(ProvenStatus.ERR_VALIDATION_FAILED, "Missing @ symbol");
        }

        String localPart = email.substring(0, atIndex);
        String domain = email.substring(atIndex + 1);

        return ProvenResult.ok(new EmailComponents(localPart, domain));
    }

    /**
     * Normalize email address (lowercase domain, trim).
     */
    public static ProvenResult<String> normalize(String email) {
        if (email == null || email.isEmpty()) {
            return ProvenResult.err(ProvenStatus.ERR_EMPTY_INPUT, "Empty email");
        }

        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeEmailNormalize(
                email.getBytes(StandardCharsets.UTF_8), status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            if (s.isOk() && result != null) {
                return ProvenResult.ok(new String(result, StandardCharsets.UTF_8));
            }
            return ProvenResult.err(s);
        }

        return parse(email.trim())
            .map(components ->
                components.localPart() + "@" + components.domain().toLowerCase());
    }

    /**
     * Extract domain from email address.
     */
    public static ProvenResult<String> getDomain(String email) {
        return parse(email).map(EmailComponents::domain);
    }

    /**
     * Extract local part from email address.
     */
    public static ProvenResult<String> getLocalPart(String email) {
        return parse(email).map(EmailComponents::localPart);
    }

    /**
     * Check if two email addresses are equivalent (case-insensitive domain).
     */
    public static boolean areEquivalent(String email1, String email2) {
        var norm1 = normalize(email1);
        var norm2 = normalize(email2);

        if (norm1.isErr() || norm2.isErr()) {
            return false;
        }

        return norm1.unwrap().equalsIgnoreCase(norm2.unwrap());
    }

    /**
     * Check if email appears to be a role-based address.
     * Role-based addresses (admin@, support@, etc.) often have lower deliverability.
     */
    public static boolean isRoleBased(String email) {
        var localPart = getLocalPart(email);
        if (localPart.isErr()) return false;

        String local = localPart.unwrap().toLowerCase();
        return local.matches("^(admin|info|support|sales|contact|help|" +
            "noreply|no-reply|webmaster|postmaster|hostmaster|abuse|" +
            "security|billing|team|staff|office|hr|marketing|press|" +
            "media|legal|feedback|newsletter)$");
    }

    /**
     * Check if email appears to be a disposable/temporary address.
     * Note: This is a basic heuristic, not comprehensive.
     */
    public static boolean likelyDisposable(String email) {
        var domain = getDomain(email);
        if (domain.isErr()) return false;

        String d = domain.unwrap().toLowerCase();
        return d.matches(".*(tempmail|mailinator|guerrillamail|throwaway|" +
            "10minutemail|trashmail|fakeinbox|temp-mail|disposable).*");
    }

    // Pure Java fallback implementation

    private static boolean isValidPure(String email) {
        if (email.length() > MAX_EMAIL_LENGTH) return false;

        int atIndex = email.lastIndexOf('@');
        if (atIndex < 1 || atIndex > MAX_LOCAL_PART_LENGTH) return false;
        if (atIndex >= email.length() - 1) return false;

        String localPart = email.substring(0, atIndex);
        String domain = email.substring(atIndex + 1);

        if (localPart.isEmpty() || localPart.length() > MAX_LOCAL_PART_LENGTH) return false;
        if (domain.isEmpty() || domain.length() > MAX_DOMAIN_LENGTH) return false;

        // Check for consecutive dots
        if (localPart.contains("..") || domain.contains("..")) return false;

        // Check start/end with dot
        if (localPart.startsWith(".") || localPart.endsWith(".")) return false;
        if (domain.startsWith(".") || domain.endsWith(".")) return false;

        // Domain must have at least one dot (TLD)
        if (!domain.contains(".")) return false;

        // Use regex for detailed validation
        return EMAIL_PATTERN.matcher(email).matches();
    }
}
