<?php
// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * Safe email validation and parsing operations.
 */
class SafeEmail
{
    /**
     * Check if an email address is valid.
     *
     * @param string $email The email address to validate
     * @return bool True if valid
     */
    public static function isValid(string $email): bool
    {
        // Use PHP's built-in filter which handles edge cases well
        return filter_var($email, FILTER_VALIDATE_EMAIL) !== false;
    }

    /**
     * Split an email into local part and domain.
     *
     * @param string $email The email address
     * @return array{local_part: string, domain: string}|null Parts or null if invalid
     */
    public static function split(string $email): ?array
    {
        if (!self::isValid($email)) {
            return null;
        }

        $atPos = strrpos($email, '@');
        if ($atPos === false) {
            return null;
        }

        return [
            'local_part' => substr($email, 0, $atPos),
            'domain' => substr($email, $atPos + 1),
        ];
    }

    /**
     * Extract the domain from an email address.
     *
     * @param string $email The email address
     * @return string|null Domain or null if invalid
     */
    public static function getDomain(string $email): ?string
    {
        $parts = self::split($email);
        return $parts['domain'] ?? null;
    }

    /**
     * Extract the local part from an email address.
     *
     * @param string $email The email address
     * @return string|null Local part or null if invalid
     */
    public static function getLocalPart(string $email): ?string
    {
        $parts = self::split($email);
        return $parts['local_part'] ?? null;
    }

    /**
     * Normalize an email address (lowercase domain).
     *
     * @param string $email The email address
     * @return string|null Normalized email or null if invalid
     */
    public static function normalize(string $email): ?string
    {
        $parts = self::split($email);
        if ($parts === null) {
            return null;
        }
        return $parts['local_part'] . '@' . strtolower($parts['domain']);
    }

    /**
     * Check if an email domain has MX records.
     *
     * Note: This performs a DNS lookup and may be slow.
     *
     * @param string $email The email address
     * @return bool True if domain has MX records
     */
    public static function hasMxRecord(string $email): bool
    {
        $domain = self::getDomain($email);
        if ($domain === null) {
            return false;
        }
        return checkdnsrr($domain, 'MX');
    }

    /**
     * Check if an email is from a disposable email service.
     *
     * Note: This is a basic check against common disposable domains.
     * For production use, consider a dedicated service.
     *
     * @param string $email The email address
     * @return bool True if likely disposable
     */
    public static function isDisposable(string $email): bool
    {
        $disposableDomains = [
            'tempmail.com', 'throwaway.email', 'guerrillamail.com',
            'mailinator.com', '10minutemail.com', 'temp-mail.org',
            'fakeinbox.com', 'trashmail.com', 'yopmail.com',
        ];

        $domain = self::getDomain($email);
        if ($domain === null) {
            return false;
        }

        return in_array(strtolower($domain), $disposableDomains, true);
    }
}
