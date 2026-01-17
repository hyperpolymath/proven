// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.hyperpolymath.proven

import groovy.transform.CompileStatic
import java.security.MessageDigest
import java.security.SecureRandom
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.util.Base64

/**
 * Safe cryptographic operations with proper error handling.
 * Provides hashing, HMAC, and secure random generation.
 */
@CompileStatic
class SafeCrypto {

    /** Supported hash algorithms. */
    static final List<String> SUPPORTED_HASHES = ['MD5', 'SHA-1', 'SHA-256', 'SHA-384', 'SHA-512']

    /** Default hash algorithm. */
    static final String DEFAULT_HASH = 'SHA-256'

    /** Thread-local secure random for performance. */
    private static final ThreadLocal<SecureRandom> SECURE_RANDOM = ThreadLocal.withInitial {
        new SecureRandom()
    }

    /**
     * Hash data with specified algorithm.
     */
    static Result<byte[], String> hash(byte[] data, String algorithm = DEFAULT_HASH) {
        if (data == null) {
            return Result.err("Data is null")
        }
        if (!SUPPORTED_HASHES.contains(algorithm)) {
            return Result.err("Unsupported hash algorithm: ${algorithm}")
        }

        try {
            MessageDigest digest = MessageDigest.getInstance(algorithm)
            return Result.ok(digest.digest(data))
        } catch (Exception e) {
            return Result.err("Hash failed: ${e.message}")
        }
    }

    /**
     * Hash string with specified algorithm.
     */
    static Result<byte[], String> hashString(String data, String algorithm = DEFAULT_HASH) {
        if (data == null) {
            return Result.err("Data is null")
        }
        return hash(data.getBytes('UTF-8'), algorithm)
    }

    /**
     * Hash data and return as hex string.
     */
    static Result<String, String> hashHex(byte[] data, String algorithm = DEFAULT_HASH) {
        hash(data, algorithm).map { byte[] bytes ->
            bytes.collect { String.format('%02x', it) }.join('')
        }
    }

    /**
     * Hash string and return as hex string.
     */
    static Result<String, String> hashStringHex(String data, String algorithm = DEFAULT_HASH) {
        hashString(data, algorithm).map { byte[] bytes ->
            bytes.collect { String.format('%02x', it) }.join('')
        }
    }

    /**
     * SHA-256 hash shortcut.
     */
    static Result<String, String> sha256(String data) {
        hashStringHex(data, 'SHA-256')
    }

    /**
     * SHA-512 hash shortcut.
     */
    static Result<String, String> sha512(String data) {
        hashStringHex(data, 'SHA-512')
    }

    /**
     * MD5 hash shortcut (not for security, only for checksums).
     */
    static Result<String, String> md5(String data) {
        hashStringHex(data, 'MD5')
    }

    /**
     * Compute HMAC.
     */
    static Result<byte[], String> hmac(byte[] data, byte[] key, String algorithm = 'HmacSHA256') {
        if (data == null) {
            return Result.err("Data is null")
        }
        if (key == null || key.length == 0) {
            return Result.err("Key is null or empty")
        }

        try {
            Mac mac = Mac.getInstance(algorithm)
            mac.init(new SecretKeySpec(key, algorithm))
            return Result.ok(mac.doFinal(data))
        } catch (Exception e) {
            return Result.err("HMAC failed: ${e.message}")
        }
    }

    /**
     * Compute HMAC and return as hex.
     */
    static Result<String, String> hmacHex(byte[] data, byte[] key, String algorithm = 'HmacSHA256') {
        hmac(data, key, algorithm).map { byte[] bytes ->
            bytes.collect { String.format('%02x', it) }.join('')
        }
    }

    /**
     * Compute HMAC for strings.
     */
    static Result<String, String> hmacString(String data, String key, String algorithm = 'HmacSHA256') {
        if (data == null || key == null) {
            return Result.err("Data or key is null")
        }
        hmacHex(data.getBytes('UTF-8'), key.getBytes('UTF-8'), algorithm)
    }

    /**
     * Generate secure random bytes.
     */
    static Result<byte[], String> randomBytes(int length) {
        if (length <= 0) {
            return Result.err("Length must be positive")
        }
        if (length > 1024 * 1024) {
            return Result.err("Length exceeds maximum (1MB)")
        }

        try {
            byte[] bytes = new byte[length]
            SECURE_RANDOM.get().nextBytes(bytes)
            return Result.ok(bytes)
        } catch (Exception e) {
            return Result.err("Random generation failed: ${e.message}")
        }
    }

    /**
     * Generate secure random bytes as hex string.
     */
    static Result<String, String> randomHex(int byteLength) {
        randomBytes(byteLength).map { byte[] bytes ->
            bytes.collect { String.format('%02x', it) }.join('')
        }
    }

    /**
     * Generate secure random bytes as Base64 string.
     */
    static Result<String, String> randomBase64(int byteLength) {
        randomBytes(byteLength).map { byte[] bytes ->
            Base64.getEncoder().encodeToString(bytes)
        }
    }

    /**
     * Generate secure random bytes as URL-safe Base64 string.
     */
    static Result<String, String> randomBase64Url(int byteLength) {
        randomBytes(byteLength).map { byte[] bytes ->
            Base64.getUrlEncoder().withoutPadding().encodeToString(bytes)
        }
    }

    /**
     * Generate a secure random integer in range [0, bound).
     */
    static Result<Integer, String> randomInt(int bound) {
        if (bound <= 0) {
            return Result.err("Bound must be positive")
        }
        return Result.ok(SECURE_RANDOM.get().nextInt(bound))
    }

    /**
     * Generate a secure random long.
     */
    static Result<Long, String> randomLong() {
        return Result.ok(SECURE_RANDOM.get().nextLong())
    }

    /**
     * Constant-time comparison to prevent timing attacks.
     */
    static boolean constantTimeEquals(byte[] a, byte[] b) {
        if (a == null || b == null) return false
        if (a.length != b.length) return false

        int result = 0
        for (int i = 0; i < a.length; i++) {
            result |= a[i] ^ b[i]
        }
        return result == 0
    }

    /**
     * Constant-time comparison for strings.
     */
    static boolean constantTimeEquals(String a, String b) {
        if (a == null || b == null) return false
        constantTimeEquals(a.getBytes('UTF-8'), b.getBytes('UTF-8'))
    }

    /**
     * Encode bytes to Base64.
     */
    static String toBase64(byte[] data) {
        if (data == null) return ""
        Base64.getEncoder().encodeToString(data)
    }

    /**
     * Decode Base64 to bytes.
     */
    static Result<byte[], String> fromBase64(String encoded) {
        if (encoded == null || encoded.isEmpty()) {
            return Result.err("Input is null or empty")
        }
        try {
            return Result.ok(Base64.getDecoder().decode(encoded))
        } catch (Exception e) {
            return Result.err("Invalid Base64: ${e.message}")
        }
    }

    /**
     * Encode bytes to URL-safe Base64.
     */
    static String toBase64Url(byte[] data) {
        if (data == null) return ""
        Base64.getUrlEncoder().withoutPadding().encodeToString(data)
    }

    /**
     * Decode URL-safe Base64 to bytes.
     */
    static Result<byte[], String> fromBase64Url(String encoded) {
        if (encoded == null || encoded.isEmpty()) {
            return Result.err("Input is null or empty")
        }
        try {
            return Result.ok(Base64.getUrlDecoder().decode(encoded))
        } catch (Exception e) {
            return Result.err("Invalid Base64Url: ${e.message}")
        }
    }
}
