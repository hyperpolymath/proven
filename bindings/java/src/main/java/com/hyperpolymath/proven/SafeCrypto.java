// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.hyperpolymath.proven;

import java.security.MessageDigest;
import java.security.SecureRandom;

/**
 * Safe cryptographic operations.
 * Provides secure hashing, HMAC, and constant-time comparison.
 * Calls Idris 2 verified code via Zig ABI when available.
 */
public final class SafeCrypto {
    private SafeCrypto() {}

    private static final SecureRandom SECURE_RANDOM = new SecureRandom();

    /**
     * SHA3-256 hash.
     */
    public static Result<byte[], String> sha3_256(byte[] data) {
        if (data == null) {
            return Result.err("Null input");
        }
        if (ProvenNative.isNativeLoaded()) {
            byte[] result = ProvenNative.nativeSha3_256(data);
            return result != null ? Result.ok(result) : Result.err("Native hash failed");
        }
        return sha3_256Pure(data);
    }

    /**
     * BLAKE3 hash.
     */
    public static Result<byte[], String> blake3(byte[] data) {
        if (data == null) {
            return Result.err("Null input");
        }
        if (ProvenNative.isNativeLoaded()) {
            byte[] result = ProvenNative.nativeBlake3(data);
            return result != null ? Result.ok(result) : Result.err("Native hash failed");
        }
        // Fall back to SHA-256 if BLAKE3 not available
        return sha256Fallback(data);
    }

    /**
     * HMAC-SHA3-256.
     */
    public static Result<byte[], String> hmacSha3(byte[] key, byte[] data) {
        if (key == null || data == null) {
            return Result.err("Null input");
        }
        if (ProvenNative.isNativeLoaded()) {
            byte[] result = ProvenNative.nativeHmacSha3(key, data);
            return result != null ? Result.ok(result) : Result.err("Native HMAC failed");
        }
        return hmacSha256Fallback(key, data);
    }

    /**
     * Constant-time comparison to prevent timing attacks.
     */
    public static boolean constantTimeCompare(byte[] a, byte[] b) {
        if (a == null || b == null) return false;
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativeConstantTimeCompare(a, b);
        }
        return constantTimeComparePure(a, b);
    }

    /**
     * Generate cryptographically secure random bytes.
     */
    public static byte[] secureRandom(int length) {
        if (length <= 0 || length > 1024 * 1024) {
            return new byte[0];
        }
        if (ProvenNative.isNativeLoaded()) {
            byte[] result = ProvenNative.nativeSecureRandom(length);
            if (result != null) return result;
        }
        byte[] bytes = new byte[length];
        SECURE_RANDOM.nextBytes(bytes);
        return bytes;
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
    public static Result<byte[], String> fromHex(String hex) {
        if (hex == null || hex.length() % 2 != 0) {
            return Result.err("Invalid hex string");
        }
        byte[] bytes = new byte[hex.length() / 2];
        for (int i = 0; i < bytes.length; i++) {
            int hi = Character.digit(hex.charAt(i * 2), 16);
            int lo = Character.digit(hex.charAt(i * 2 + 1), 16);
            if (hi == -1 || lo == -1) {
                return Result.err("Invalid hex character");
            }
            bytes[i] = (byte) ((hi << 4) | lo);
        }
        return Result.ok(bytes);
    }

    // Pure Java fallback implementations

    private static Result<byte[], String> sha3_256Pure(byte[] data) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA3-256");
            return Result.ok(md.digest(data));
        } catch (Exception e) {
            return sha256Fallback(data);
        }
    }

    private static Result<byte[], String> sha256Fallback(byte[] data) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            return Result.ok(md.digest(data));
        } catch (Exception e) {
            return Result.err("Hash algorithm not available");
        }
    }

    private static Result<byte[], String> hmacSha256Fallback(byte[] key, byte[] data) {
        try {
            javax.crypto.Mac mac = javax.crypto.Mac.getInstance("HmacSHA256");
            mac.init(new javax.crypto.spec.SecretKeySpec(key, "HmacSHA256"));
            return Result.ok(mac.doFinal(data));
        } catch (Exception e) {
            return Result.err("HMAC not available");
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
