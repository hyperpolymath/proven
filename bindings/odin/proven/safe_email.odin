// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:strings"

// Result type for email parsing.
EmailResult :: struct {
    local_part: string,
    domain:     string,
    error:      string,
    ok:         bool,
}

// Create a successful EmailResult.
email_ok :: proc(local_part, domain: string) -> EmailResult {
    return EmailResult{local_part = local_part, domain = domain, error = "", ok = true}
}

// Create an error EmailResult.
email_error :: proc(message: string) -> EmailResult {
    return EmailResult{local_part = "", domain = "", error = message, ok = false}
}

// Common disposable email domains.
DISPOSABLE_DOMAINS :: []string{
    "mailinator.com", "guerrillamail.com", "tempmail.com", "throwaway.email",
    "10minutemail.com", "fakeinbox.com", "trashmail.com", "maildrop.cc",
    "yopmail.com", "sharklasers.com",
}

// Check if email format is valid.
is_valid_email :: proc(email: string) -> bool {
    if len(email) == 0 || len(email) > 254 {
        return false
    }

    at_index := strings.index(email, "@")
    if at_index < 0 {
        return false
    }

    local_part := email[:at_index]
    domain := email[at_index + 1:]

    if len(local_part) == 0 || len(local_part) > 64 {
        return false
    }
    if len(domain) == 0 || len(domain) > 253 {
        return false
    }

    if !is_valid_local_part(local_part) {
        return false
    }
    if !is_valid_domain(domain) {
        return false
    }

    return true
}

// Validate local part of email.
is_valid_local_part :: proc(local_part: string) -> bool {
    if len(local_part) == 0 {
        return false
    }

    allowed := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.!#$%&'*+/=?^_`{|}~-"
    for c in local_part {
        if !strings.contains_rune(allowed, c) {
            return false
        }
    }
    return true
}

// Validate domain part of email.
is_valid_domain :: proc(domain: string) -> bool {
    if len(domain) == 0 || !strings.contains(domain, ".") {
        return false
    }

    labels := strings.split(domain, ".")
    for label in labels {
        if len(label) == 0 || len(label) > 63 {
            return false
        }
        // Must start and end with alphanumeric
        if !is_alpha_num(label[0]) {
            return false
        }
        if !is_alpha_num(label[len(label) - 1]) {
            return false
        }
        // Middle can include hyphens
        for i := 1; i < len(label) - 1; i += 1 {
            c := label[i]
            if !is_alpha_num(c) && c != '-' {
                return false
            }
        }
    }
    return true
}

// Check if character is alphanumeric.
is_alpha_num :: proc(c: u8) -> bool {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
}

// Parse and validate email address.
parse_email :: proc(email: string) -> EmailResult {
    if len(email) == 0 {
        return email_error("Email is empty")
    }
    if len(email) > 254 {
        return email_error("Email too long")
    }

    at_index := strings.index(email, "@")
    if at_index < 0 {
        return email_error("Missing @ symbol")
    }

    local_part := email[:at_index]
    domain := email[at_index + 1:]

    if len(local_part) == 0 {
        return email_error("Local part is empty")
    }
    if len(local_part) > 64 {
        return email_error("Local part too long")
    }
    if len(domain) == 0 {
        return email_error("Domain is empty")
    }
    if len(domain) > 253 {
        return email_error("Domain too long")
    }

    if !is_valid_email(email) {
        return email_error("Invalid email format")
    }

    return email_ok(local_part, domain)
}

// Check if domain is a disposable email provider.
is_disposable_email :: proc(email: string) -> bool {
    at_index := strings.index(email, "@")
    if at_index < 0 {
        return false
    }

    domain := strings.to_lower(email[at_index + 1:])
    for d in DISPOSABLE_DOMAINS {
        if domain == d {
            return true
        }
    }
    return false
}

// Normalize email address.
normalize_email :: proc(email: string, allocator := context.allocator) -> (result: string, ok: bool) {
    parsed := parse_email(email)
    if !parsed.ok {
        return "", false
    }
    return strings.concatenate({parsed.local_part, "@", strings.to_lower(parsed.domain)}, allocator), true
}

// Get domain from email.
get_email_domain :: proc(email: string) -> (domain: string, ok: bool) {
    at_index := strings.index(email, "@")
    if at_index < 0 {
        return "", false
    }
    return email[at_index + 1:], true
}

// Get local part from email.
get_local_part :: proc(email: string) -> (local_part: string, ok: bool) {
    at_index := strings.index(email, "@")
    if at_index < 0 {
        return "", false
    }
    return email[:at_index], true
}

// Mask email for display.
mask_email :: proc(email: string, allocator := context.allocator) -> (result: string, ok: bool) {
    parsed := parse_email(email)
    if !parsed.ok {
        return "", false
    }

    local := parsed.local_part
    domain := parsed.domain

    if len(local) <= 1 {
        return strings.concatenate({local, "***@", domain}, allocator), true
    }
    return strings.concatenate({local[:1], "***@", domain}, allocator), true
}

// Check if two emails are equal.
emails_equal :: proc(email1, email2: string) -> bool {
    norm1, ok1 := normalize_email(email1)
    norm2, ok2 := normalize_email(email2)
    if !ok1 || !ok2 {
        return false
    }
    return norm1 == norm2
}
