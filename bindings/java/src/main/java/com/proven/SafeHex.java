// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.proven;

import java.nio.charset.StandardCharsets;

/**
 * Safe hexadecimal encoding and decoding.
 * Provides constant-time comparison for security-sensitive operations.
 * Calls native verified code via JNI when available.
 */
public final class SafeHex {
    private SafeHex() {}

    private static final char[] HEX_LOWER = "0123456789abcdef".toCharArray();
    private static final char[] HEX_UPPER = "0123456789ABCDEF".toCharArray();

    /**
     * Encode bytes to lowercase hex string.
     */
    public static String encode(byte[] bytes) {
        if (bytes == null) return "";
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeHexEncode(bytes, false, status);
            if (ProvenStatus.fromCode(status[0]).isOk() && result != null) {
                return new String(result, StandardCharsets.UTF_8);
            }
        }
        return encodePure(bytes, false);
    }

    /**
     * Encode bytes to uppercase hex string.
     */
    public static String encodeUpper(byte[] bytes) {
        if (bytes == null) return "";
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeHexEncode(bytes, true, status);
            if (ProvenStatus.fromCode(status[0]).isOk() && result != null) {
                return new String(result, StandardCharsets.UTF_8);
            }
        }
        return encodePure(bytes, true);
    }

    /**
     * Decode hex string to bytes.
     */
    public static ProvenResult<byte[]> decode(String hex) {
        if (hex == null || hex.isEmpty()) {
            return ProvenResult.ok(new byte[0]);
        }
        if (hex.length() % 2 != 0) {
            return ProvenResult.err(ProvenStatus.ERR_INVALID_ARGUMENT, "Hex string has odd length");
        }
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeHexDecode(
                hex.getBytes(StandardCharsets.UTF_8), status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() && result != null ? ProvenResult.ok(result) : ProvenResult.err(s);
        }
        return decodePure(hex);
    }

    /**
     * Check if character is valid hex digit.
     */
    public static boolean isValidChar(char c) {
        return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
    }

    /**
     * Check if string contains only valid hex characters.
     */
    public static boolean isValid(String hex) {
        if (hex == null) return false;
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativeHexIsValid(hex.getBytes(StandardCharsets.UTF_8));
        }
        for (int i = 0; i < hex.length(); i++) {
            if (!isValidChar(hex.charAt(i))) return false;
        }
        return true;
    }

    /**
     * Check if string is valid hex bytes (even length, valid chars).
     */
    public static boolean isValidBytes(String hex) {
        return hex != null && hex.length() % 2 == 0 && isValid(hex);
    }

    /**
     * Constant-time hex string comparison.
     * Use for comparing security-sensitive values like API keys, tokens, and hashes.
     */
    public static boolean constantTimeEquals(String a, String b) {
        if (a == null || b == null) return false;
        if (a.length() != b.length()) return false;
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativeHexConstantTimeEq(
                a.getBytes(StandardCharsets.UTF_8),
                b.getBytes(StandardCharsets.UTF_8));
        }
        return constantTimeEqualsPure(a, b);
    }

    /**
     * Constant-time byte array comparison.
     */
    public static boolean constantTimeEquals(byte[] a, byte[] b) {
        if (a == null || b == null) return false;
        if (a.length != b.length) return false;
        int result = 0;
        for (int i = 0; i < a.length; i++) {
            result |= a[i] ^ b[i];
        }
        return result == 0;
    }

    /**
     * Format hex with spaces between bytes.
     */
    public static String formatSpaced(String hex) {
        if (hex == null || hex.length() < 2) return hex;
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < hex.length(); i += 2) {
            if (i > 0) sb.append(' ');
            sb.append(hex, i, Math.min(i + 2, hex.length()));
        }
        return sb.toString();
    }

    /**
     * Format hex with colons between bytes (like MAC address).
     */
    public static String formatColons(String hex) {
        if (hex == null || hex.length() < 2) return hex;
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < hex.length(); i += 2) {
            if (i > 0) sb.append(':');
            sb.append(hex, i, Math.min(i + 2, hex.length()));
        }
        return sb.toString();
    }

    /**
     * Convert integer to hex string with optional minimum width.
     */
    public static String fromInt(long value, int minWidth, boolean uppercase) {
        String hex = Long.toHexString(value);
        if (uppercase) hex = hex.toUpperCase();
        if (hex.length() < minWidth) {
            hex = "0".repeat(minWidth - hex.length()) + hex;
        }
        return hex;
    }

    /**
     * Parse hex string to integer.
     */
    public static ProvenResult<Long> toInt(String hex) {
        if (hex == null || hex.isEmpty()) {
            return ProvenResult.err(ProvenStatus.ERR_EMPTY_INPUT, "Empty hex string");
        }

        String clean = hex;
        if (clean.startsWith("0x") || clean.startsWith("0X")) {
            clean = clean.substring(2);
        }

        if (!isValid(clean)) {
            return ProvenResult.err(ProvenStatus.ERR_INVALID_ARGUMENT, "Invalid hex characters");
        }

        try {
            return ProvenResult.ok(Long.parseUnsignedLong(clean, 16));
        } catch (NumberFormatException e) {
            return ProvenResult.err(ProvenStatus.ERR_OVERFLOW, "Hex value too large");
        }
    }

    /**
     * Strip 0x prefix if present.
     */
    public static String stripPrefix(String hex) {
        if (hex == null) return null;
        if (hex.startsWith("0x") || hex.startsWith("0X")) {
            return hex.substring(2);
        }
        return hex;
    }

    /**
     * Calculate required buffer size for encoding.
     */
    public static int encodeSize(int bytesLen) {
        return bytesLen * 2;
    }

    /**
     * Calculate required buffer size for decoding.
     */
    public static int decodeSize(int hexLen) {
        return hexLen / 2;
    }

    // Pure Java implementations

    private static String encodePure(byte[] bytes, boolean uppercase) {
        char[] table = uppercase ? HEX_UPPER : HEX_LOWER;
        char[] result = new char[bytes.length * 2];
        for (int i = 0; i < bytes.length; i++) {
            int b = bytes[i] & 0xFF;
            result[i * 2] = table[b >>> 4];
            result[i * 2 + 1] = table[b & 0x0F];
        }
        return new String(result);
    }

    private static ProvenResult<byte[]> decodePure(String hex) {
        byte[] result = new byte[hex.length() / 2];
        for (int i = 0; i < result.length; i++) {
            int hi = Character.digit(hex.charAt(i * 2), 16);
            int lo = Character.digit(hex.charAt(i * 2 + 1), 16);
            if (hi < 0 || lo < 0) {
                return ProvenResult.err(ProvenStatus.ERR_INVALID_ARGUMENT,
                    "Invalid hex character at position " + (i * 2));
            }
            result[i] = (byte) ((hi << 4) | lo);
        }
        return ProvenResult.ok(result);
    }

    private static boolean constantTimeEqualsPure(String a, String b) {
        int result = 0;
        for (int i = 0; i < a.length(); i++) {
            char ca = Character.toLowerCase(a.charAt(i));
            char cb = Character.toLowerCase(b.charAt(i));
            result |= ca ^ cb;
        }
        return result == 0;
    }
}
