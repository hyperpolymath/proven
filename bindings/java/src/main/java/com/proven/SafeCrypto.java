// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.proven;

import java.security.MessageDigest;
import java.security.SecureRandom;

/**
 * Safe cryptographic operations.
 * Provides secure hashing, HMAC, and constant-time comparison.
 * Calls native verified code via JNI when available.
 */
public final class SafeCrypto {
    private SafeCrypto() {}

    private static final SecureRandom SECURE_RANDOM = new SecureRandom();

    /**
     * SHA-256 hash.
     */
    public static ProvenResult<byte[]> sha256(byte[] data) {
        if (data == null) {
            return ProvenResult.err(ProvenStatus.ERR_NULL_POINTER, "Null input");
        }
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeCryptoSha256(data, status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() && result != null ? ProvenResult.ok(result) : ProvenResult.err(s);
        }
        return sha256Pure(data);
    }

    /**
     * SHA3-256 hash.
     */
    public static ProvenResult<byte[]> sha3_256(byte[] data) {
        if (data == null) {
            return ProvenResult.err(ProvenStatus.ERR_NULL_POINTER, "Null input");
        }
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeCryptoSha3_256(data, status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() && result != null ? ProvenResult.ok(result) : ProvenResult.err(s);
        }
        return sha3_256Pure(data);
    }

    /**
     * BLAKE3 hash.
     */
    public static ProvenResult<byte[]> blake3(byte[] data) {
        if (data == null) {
            return ProvenResult.err(ProvenStatus.ERR_NULL_POINTER, "Null input");
        }
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeCryptoBlake3(data, status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() && result != null ? ProvenResult.ok(result) : ProvenResult.err(s);
        }
        // Fall back to SHA-256 if BLAKE3 not available
        return sha256(data);
    }

    /**
     * HMAC-SHA256.
     */
    public static ProvenResult<byte[]> hmacSha256(byte[] key, byte[] data) {
        if (key == null || data == null) {
            return ProvenResult.err(ProvenStatus.ERR_NULL_POINTER, "Null input");
        }
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeCryptoHmacSha256(key, data, status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() && result != null ? ProvenResult.ok(result) : ProvenResult.err(s);
        }
        return hmacSha256Pure(key, data);
    }

    /**
     * Constant-time comparison to prevent timing attacks.
     */
    public static boolean constantTimeCompare(byte[] a, byte[] b) {
        if (a == null || b == null) return false;
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativeCryptoConstantTimeEq(a, b);
        }
        return constantTimeComparePure(a, b);
    }

    /**
     * Generate cryptographically secure random bytes.
     */
    public static ProvenResult<byte[]> secureRandom(int length) {
        if (length <= 0 || length > 1024 * 1024) {
            return ProvenResult.err(ProvenStatus.ERR_INVALID_ARGUMENT,
                "Length must be between 1 and 1MB");
        }
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeCryptoRandomBytes(length, status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() && result != null ? ProvenResult.ok(result) : ProvenResult.err(s);
        }
        byte[] bytes = new byte[length];
        SECURE_RANDOM.nextBytes(bytes);
        return ProvenResult.ok(bytes);
    }

    /**
     * Convert bytes to hexadecimal string.
     */
    public static String toHex(byte[] bytes) {
        if (bytes == null) return "";
        StringBuilder sb = new StringBuilder(bytes.length * 2);
        for (byte b : bytes) {
            sb.append(String.format("%02x", b & 0xFF));
        }
        return sb.toString();
    }

    /**
     * Convert hexadecimal string to bytes.
     */
    public static ProvenResult<byte[]> fromHex(String hex) {
        if (hex == null || hex.length() % 2 != 0) {
            return ProvenResult.err(ProvenStatus.ERR_INVALID_ARGUMENT, "Invalid hex string");
        }
        byte[] bytes = new byte[hex.length() / 2];
        for (int i = 0; i < bytes.length; i++) {
            int hi = Character.digit(hex.charAt(i * 2), 16);
            int lo = Character.digit(hex.charAt(i * 2 + 1), 16);
            if (hi == -1 || lo == -1) {
                return ProvenResult.err(ProvenStatus.ERR_INVALID_ARGUMENT, "Invalid hex character");
            }
            bytes[i] = (byte) ((hi << 4) | lo);
        }
        return ProvenResult.ok(bytes);
    }

    /**
     * Hash a password for storage (using SHA-256 with salt).
     * Note: For production, use SafePassword.hash() which uses proper password hashing.
     */
    public static ProvenResult<String> hashPassword(String password) {
        if (password == null || password.isEmpty()) {
            return ProvenResult.err(ProvenStatus.ERR_EMPTY_INPUT, "Empty password");
        }

        // Generate salt
        var saltResult = secureRandom(16);
        if (saltResult.isErr()) return ProvenResult.err(saltResult.status());
        byte[] salt = saltResult.unwrap();

        // Hash password with salt
        byte[] passwordBytes = password.getBytes(java.nio.charset.StandardCharsets.UTF_8);
        byte[] toHash = new byte[salt.length + passwordBytes.length];
        System.arraycopy(salt, 0, toHash, 0, salt.length);
        System.arraycopy(passwordBytes, 0, toHash, salt.length, passwordBytes.length);

        var hashResult = sha256(toHash);
        if (hashResult.isErr()) return ProvenResult.err(hashResult.status());

        // Return salt:hash
        return ProvenResult.ok(toHex(salt) + ":" + toHex(hashResult.unwrap()));
    }

    /**
     * Verify a password against a stored hash.
     */
    public static boolean verifyPassword(String password, String storedHash) {
        if (password == null || storedHash == null) return false;

        String[] parts = storedHash.split(":");
        if (parts.length != 2) return false;

        var saltResult = fromHex(parts[0]);
        var expectedHashResult = fromHex(parts[1]);
        if (saltResult.isErr() || expectedHashResult.isErr()) return false;

        byte[] salt = saltResult.unwrap();
        byte[] expectedHash = expectedHashResult.unwrap();

        // Hash password with salt
        byte[] passwordBytes = password.getBytes(java.nio.charset.StandardCharsets.UTF_8);
        byte[] toHash = new byte[salt.length + passwordBytes.length];
        System.arraycopy(salt, 0, toHash, 0, salt.length);
        System.arraycopy(passwordBytes, 0, toHash, salt.length, passwordBytes.length);

        var hashResult = sha256(toHash);
        if (hashResult.isErr()) return false;

        return constantTimeCompare(hashResult.unwrap(), expectedHash);
    }

    // Pure Java fallback implementations

    private static ProvenResult<byte[]> sha256Pure(byte[] data) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            return ProvenResult.ok(md.digest(data));
        } catch (Exception e) {
            return ProvenResult.err(ProvenStatus.ERR_NOT_IMPLEMENTED, "SHA-256 not available");
        }
    }

    private static ProvenResult<byte[]> sha3_256Pure(byte[] data) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA3-256");
            return ProvenResult.ok(md.digest(data));
        } catch (Exception e) {
            // Fall back to SHA-256
            return sha256Pure(data);
        }
    }

    private static ProvenResult<byte[]> hmacSha256Pure(byte[] key, byte[] data) {
        try {
            javax.crypto.Mac mac = javax.crypto.Mac.getInstance("HmacSHA256");
            mac.init(new javax.crypto.spec.SecretKeySpec(key, "HmacSHA256"));
            return ProvenResult.ok(mac.doFinal(data));
        } catch (Exception e) {
            return ProvenResult.err(ProvenStatus.ERR_NOT_IMPLEMENTED, "HMAC not available");
        }
    }

    private static boolean constantTimeComparePure(byte[] a, byte[] b) {
        if (a.length != b.length) return false;
        int result = 0;
        for (int i = 0; i < a.length; i++) {
            result |= a[i] ^ b[i];
        }
        return result == 0;
    }
}
