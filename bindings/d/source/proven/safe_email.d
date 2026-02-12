// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe email validation and parsing.
 */
module proven.safe_email;

import std.string : toLower, indexOf;
import std.array : split;
import std.algorithm : canFind, all;
import std.typecons : Nullable, nullable;

/// Result type for email parsing.
struct EmailResult
{
    string localPart;
    string domain;
    string error;
    bool ok;

    static EmailResult success(string localPart, string domain)
    {
        return EmailResult(localPart, domain, "", true);
    }

    static EmailResult failure(string error)
    {
        return EmailResult("", "", error, false);
    }
}

/// Common disposable email domains.
private immutable string[] disposableDomains = [
    "mailinator.com", "guerrillamail.com", "tempmail.com", "throwaway.email",
    "10minutemail.com", "fakeinbox.com", "trashmail.com", "maildrop.cc",
    "yopmail.com", "sharklasers.com"
];

/// Check if email format is valid.
bool isValidEmail(string email) pure @safe
{
    if (email.length == 0 || email.length > 254)
        return false;

    auto atIndex = email.indexOf('@');
    if (atIndex < 0)
        return false;

    auto localPart = email[0 .. atIndex];
    auto domain = email[atIndex + 1 .. $];

    if (localPart.length == 0 || localPart.length > 64)
        return false;
    if (domain.length == 0 || domain.length > 253)
        return false;

    if (!isValidLocalPart(localPart))
        return false;
    if (!isValidDomain(domain))
        return false;

    return true;
}

/// Validate local part of email.
private bool isValidLocalPart(string localPart) pure @safe
{
    if (localPart.length == 0)
        return false;

    immutable allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.!#$%&'*+/=?^_`{|}~-";
    foreach (c; localPart)
    {
        if (!allowed.canFind(c))
            return false;
    }
    return true;
}

/// Validate domain part of email.
private bool isValidDomain(string domain) pure @safe
{
    if (domain.length == 0 || !domain.canFind('.'))
        return false;

    auto labels = domain.split('.');
    foreach (label; labels)
    {
        if (label.length == 0 || label.length > 63)
            return false;
        // Must start and end with alphanumeric
        if (!isAlphaNum(label[0]))
            return false;
        if (!isAlphaNum(label[$ - 1]))
            return false;
        // Middle can include hyphens
        foreach (c; label)
        {
            if (!isAlphaNum(c) && c != '-')
                return false;
        }
    }
    return true;
}

/// Check if character is alphanumeric.
private bool isAlphaNum(char c) pure nothrow @safe @nogc
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9');
}

/// Parse and validate email address.
EmailResult parseEmail(string email) pure @safe
{
    if (email.length == 0)
        return EmailResult.failure("Email is empty");
    if (email.length > 254)
        return EmailResult.failure("Email too long");

    auto atIndex = email.indexOf('@');
    if (atIndex < 0)
        return EmailResult.failure("Missing @ symbol");

    auto localPart = email[0 .. atIndex];
    auto domain = email[atIndex + 1 .. $];

    if (localPart.length == 0)
        return EmailResult.failure("Local part is empty");
    if (localPart.length > 64)
        return EmailResult.failure("Local part too long");
    if (domain.length == 0)
        return EmailResult.failure("Domain is empty");
    if (domain.length > 253)
        return EmailResult.failure("Domain too long");

    if (!isValidEmail(email))
        return EmailResult.failure("Invalid email format");

    return EmailResult.success(localPart, domain);
}

/// Check if domain is a disposable email provider.
bool isDisposableEmail(string email) pure @safe
{
    auto atIndex = email.indexOf('@');
    if (atIndex < 0)
        return false;

    auto domain = email[atIndex + 1 .. $].toLower();
    return disposableDomains.canFind(domain);
}

/// Normalize email address.
Nullable!string normalizeEmail(string email) pure @safe
{
    auto result = parseEmail(email);
    if (!result.ok)
        return Nullable!string.init;
    return nullable(result.localPart ~ "@" ~ result.domain.toLower());
}

/// Get domain from email.
Nullable!string getEmailDomain(string email) pure @safe
{
    auto atIndex = email.indexOf('@');
    if (atIndex < 0)
        return Nullable!string.init;
    return nullable(email[atIndex + 1 .. $]);
}

/// Get local part from email.
Nullable!string getLocalPart(string email) pure @safe
{
    auto atIndex = email.indexOf('@');
    if (atIndex < 0)
        return Nullable!string.init;
    return nullable(email[0 .. atIndex]);
}

/// Mask email for display.
Nullable!string maskEmail(string email) pure @safe
{
    auto result = parseEmail(email);
    if (!result.ok)
        return Nullable!string.init;

    auto local = result.localPart;
    auto domain = result.domain;

    if (local.length <= 1)
        return nullable(local ~ "***@" ~ domain);
    return nullable(local[0 .. 1] ~ "***@" ~ domain);
}

/// Check if two emails are equal (case-insensitive domain).
bool emailsEqual(string email1, string email2) pure @safe
{
    auto norm1 = normalizeEmail(email1);
    auto norm2 = normalizeEmail(email2);
    if (norm1.isNull || norm2.isNull)
        return false;
    return norm1.get == norm2.get;
}

// Unit tests
unittest
{
    assert(isValidEmail("user@example.com"));
    assert(!isValidEmail("invalid"));
    assert(parseEmail("user@example.com").ok);
    assert(isDisposableEmail("test@mailinator.com"));
}
