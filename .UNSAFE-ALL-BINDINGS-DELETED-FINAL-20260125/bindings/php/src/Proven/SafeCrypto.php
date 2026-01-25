<?php
// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * Cryptographic safety operations with constant-time guarantees.
 */
class SafeCrypto
{
    /**
     * Compare two strings in constant time to prevent timing attacks.
     *
     * @param string $a First string
     * @param string $b Second string
     * @return bool True if equal
     */
    public static function constantTimeCompare(string $a, string $b): bool
    {
        return hash_equals($a, $b);
    }

    /**
     * Generate cryptographically secure random bytes.
     *
     * @param int $length Number of bytes to generate
     * @return string Random bytes
     * @throws \Exception If secure random generation fails
     */
    public static function randomBytes(int $length): string
    {
        if ($length < 0) {
            throw new \InvalidArgumentException('Length must be non-negative');
        }
        if ($length === 0) {
            return '';
        }
        return random_bytes($length);
    }

    /**
     * Generate a cryptographically secure random integer.
     *
     * @param int $min Minimum value (inclusive)
     * @param int $max Maximum value (inclusive)
     * @return int Random integer in range
     * @throws \Exception If secure random generation fails
     */
    public static function randomInt(int $min, int $max): int
    {
        return random_int($min, $max);
    }

    /**
     * Generate a secure random hex string.
     *
     * @param int $byteLength Number of random bytes (output will be 2x this length)
     * @return string Hex-encoded random string
     */
    public static function randomHex(int $byteLength): string
    {
        return bin2hex(self::randomBytes($byteLength));
    }

    /**
     * Generate a secure random base64 string.
     *
     * @param int $byteLength Number of random bytes
     * @return string Base64-encoded random string
     */
    public static function randomBase64(int $byteLength): string
    {
        return base64_encode(self::randomBytes($byteLength));
    }

    /**
     * Generate a URL-safe random string.
     *
     * @param int $byteLength Number of random bytes
     * @return string URL-safe base64-encoded random string
     */
    public static function randomUrlSafe(int $byteLength): string
    {
        return rtrim(strtr(base64_encode(self::randomBytes($byteLength)), '+/', '-_'), '=');
    }

    /**
     * Securely zero a string in memory (best effort).
     *
     * Note: PHP strings are copy-on-write, so this creates a new zeroed string.
     * The original string may still exist in memory until garbage collected.
     *
     * @param int $length Length of zero-filled string to create
     * @return string String of null bytes
     */
    public static function secureZero(int $length): string
    {
        return str_repeat("\0", $length);
    }

    /**
     * Hash a password securely using bcrypt.
     *
     * @param string $password The password to hash
     * @param int $cost Bcrypt cost factor (default 12)
     * @return string The hashed password
     */
    public static function hashPassword(string $password, int $cost = 12): string
    {
        return password_hash($password, PASSWORD_BCRYPT, ['cost' => $cost]);
    }

    /**
     * Verify a password against a hash using constant-time comparison.
     *
     * @param string $password The password to verify
     * @param string $hash The hash to verify against
     * @return bool True if password matches
     */
    public static function verifyPassword(string $password, string $hash): bool
    {
        return password_verify($password, $hash);
    }

    /**
     * Check if a password hash needs rehashing (algorithm or cost changed).
     *
     * @param string $hash The hash to check
     * @param int $cost Current cost factor
     * @return bool True if rehashing is needed
     */
    public static function needsRehash(string $hash, int $cost = 12): bool
    {
        return password_needs_rehash($hash, PASSWORD_BCRYPT, ['cost' => $cost]);
    }

    /**
     * Generate a HMAC using SHA-256.
     *
     * @param string $data The data to authenticate
     * @param string $key The secret key
     * @return string The HMAC as raw bytes
     */
    public static function hmac(string $data, string $key): string
    {
        return hash_hmac('sha256', $data, $key, true);
    }

    /**
     * Verify a HMAC using constant-time comparison.
     *
     * @param string $data The data that was authenticated
     * @param string $key The secret key
     * @param string $expectedHmac The HMAC to verify
     * @return bool True if HMAC is valid
     */
    public static function verifyHmac(string $data, string $key, string $expectedHmac): bool
    {
        $actualHmac = self::hmac($data, $key);
        return self::constantTimeCompare($actualHmac, $expectedHmac);
    }
}
