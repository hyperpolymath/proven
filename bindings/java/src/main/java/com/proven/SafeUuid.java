// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.proven;

import java.nio.charset.StandardCharsets;
import java.security.SecureRandom;
import java.util.UUID;
import java.util.regex.Pattern;

/**
 * Safe UUID generation and parsing following RFC 4122.
 * Provides UUID v4 generation, parsing, and formatting.
 * Calls native verified code via JNI when available.
 */
public final class SafeUuid {
    private SafeUuid() {}

    /** UUID bytes length. */
    public static final int UUID_BYTES_LEN = 16;

    /** Canonical UUID string length. */
    public static final int UUID_STRING_LEN = 36;

    /** UUID validation pattern. */
    private static final Pattern UUID_PATTERN = Pattern.compile(
        "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"
    );

    private static final SecureRandom SECURE_RANDOM = new SecureRandom();

    /** The nil UUID (all zeros). */
    public static final byte[] NIL = new byte[UUID_BYTES_LEN];

    /** DNS namespace UUID for name-based generation. */
    public static final byte[] NAMESPACE_DNS = parseUuidBytes("6ba7b810-9dad-11d1-80b4-00c04fd430c8");

    /** URL namespace UUID for name-based generation. */
    public static final byte[] NAMESPACE_URL = parseUuidBytes("6ba7b811-9dad-11d1-80b4-00c04fd430c8");

    /**
     * UUID version types per RFC 4122.
     */
    public enum Version {
        NIL(0),
        TIME_BASED(1),
        DCE_SECURITY(2),
        NAME_BASED_MD5(3),
        RANDOM(4),
        NAME_BASED_SHA1(5);

        private final int value;

        Version(int value) {
            this.value = value;
        }

        public int getValue() {
            return value;
        }

        public static Version fromValue(int value) {
            for (Version v : values()) {
                if (v.value == value) return v;
            }
            return NIL;
        }
    }

    /**
     * Generate a v4 (random) UUID.
     */
    public static ProvenResult<byte[]> v4Generate() {
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeUuidV4Generate(status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() && result != null ? ProvenResult.ok(result) : ProvenResult.err(s);
        }
        return v4GeneratePure();
    }

    /**
     * Generate a v4 UUID and return as string.
     */
    public static ProvenResult<String> v4GenerateString() {
        return v4Generate().map(SafeUuid::toString);
    }

    /**
     * Parse UUID from canonical string format.
     */
    public static ProvenResult<byte[]> parse(String uuidStr) {
        if (uuidStr == null || uuidStr.isEmpty()) {
            return ProvenResult.err(ProvenStatus.ERR_EMPTY_INPUT, "Empty UUID string");
        }
        if (!isValid(uuidStr)) {
            return ProvenResult.err(ProvenStatus.ERR_PARSE_FAILURE, "Invalid UUID format");
        }

        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeUuidParse(
                uuidStr.getBytes(StandardCharsets.UTF_8), status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() && result != null ? ProvenResult.ok(result) : ProvenResult.err(s);
        }

        return parsePure(uuidStr);
    }

    /**
     * Check if string is valid UUID format.
     */
    public static boolean isValid(String uuidStr) {
        if (uuidStr == null || uuidStr.length() != UUID_STRING_LEN) return false;
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativeUuidIsValid(uuidStr.getBytes(StandardCharsets.UTF_8));
        }
        return UUID_PATTERN.matcher(uuidStr).matches();
    }

    /**
     * Format UUID bytes as canonical string.
     */
    public static String toString(byte[] uuid) {
        if (uuid == null || uuid.length != UUID_BYTES_LEN) {
            return "";
        }
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeUuidToString(uuid, status);
            if (ProvenStatus.fromCode(status[0]).isOk() && result != null) {
                return new String(result, StandardCharsets.UTF_8);
            }
        }
        return toStringPure(uuid);
    }

    /**
     * Format UUID bytes as uppercase string.
     */
    public static String toStringUpper(byte[] uuid) {
        return toString(uuid).toUpperCase();
    }

    /**
     * Format UUID as URN.
     */
    public static String toUrn(byte[] uuid) {
        return "urn:uuid:" + toString(uuid);
    }

    /**
     * Get UUID version.
     */
    public static Version getVersion(byte[] uuid) {
        if (uuid == null || uuid.length != UUID_BYTES_LEN) {
            return Version.NIL;
        }
        if (ProvenNative.isNativeLoaded()) {
            int version = ProvenNative.nativeUuidGetVersion(uuid);
            return Version.fromValue(version);
        }
        int version = (uuid[6] & 0xF0) >> 4;
        return Version.fromValue(version);
    }

    /**
     * Check if UUID is nil (all zeros).
     */
    public static boolean isNil(byte[] uuid) {
        if (uuid == null || uuid.length != UUID_BYTES_LEN) return true;
        for (byte b : uuid) {
            if (b != 0) return false;
        }
        return true;
    }

    /**
     * Compare two UUIDs.
     */
    public static int compare(byte[] a, byte[] b) {
        if (a == null && b == null) return 0;
        if (a == null) return -1;
        if (b == null) return 1;
        if (a.length != UUID_BYTES_LEN || b.length != UUID_BYTES_LEN) {
            return Integer.compare(a.length, b.length);
        }
        for (int i = 0; i < UUID_BYTES_LEN; i++) {
            int cmp = Integer.compare(a[i] & 0xFF, b[i] & 0xFF);
            if (cmp != 0) return cmp;
        }
        return 0;
    }

    /**
     * Check if two UUIDs are equal.
     */
    public static boolean equals(byte[] a, byte[] b) {
        return compare(a, b) == 0;
    }

    /**
     * Convert Java UUID to bytes.
     */
    public static byte[] fromJavaUuid(UUID uuid) {
        if (uuid == null) return NIL.clone();
        byte[] bytes = new byte[UUID_BYTES_LEN];
        long msb = uuid.getMostSignificantBits();
        long lsb = uuid.getLeastSignificantBits();
        for (int i = 0; i < 8; i++) {
            bytes[i] = (byte) (msb >> (56 - i * 8));
            bytes[i + 8] = (byte) (lsb >> (56 - i * 8));
        }
        return bytes;
    }

    /**
     * Convert bytes to Java UUID.
     */
    public static UUID toJavaUuid(byte[] bytes) {
        if (bytes == null || bytes.length != UUID_BYTES_LEN) {
            return new UUID(0, 0);
        }
        long msb = 0;
        long lsb = 0;
        for (int i = 0; i < 8; i++) {
            msb = (msb << 8) | (bytes[i] & 0xFF);
            lsb = (lsb << 8) | (bytes[i + 8] & 0xFF);
        }
        return new UUID(msb, lsb);
    }

    // Pure Java fallback implementations

    private static ProvenResult<byte[]> v4GeneratePure() {
        byte[] bytes = new byte[UUID_BYTES_LEN];
        SECURE_RANDOM.nextBytes(bytes);

        // Set version to 4
        bytes[6] = (byte) ((bytes[6] & 0x0F) | 0x40);
        // Set variant to RFC 4122
        bytes[8] = (byte) ((bytes[8] & 0x3F) | 0x80);

        return ProvenResult.ok(bytes);
    }

    private static ProvenResult<byte[]> parsePure(String uuidStr) {
        String hex = uuidStr.replace("-", "");
        byte[] bytes = new byte[UUID_BYTES_LEN];
        for (int i = 0; i < UUID_BYTES_LEN; i++) {
            int hi = Character.digit(hex.charAt(i * 2), 16);
            int lo = Character.digit(hex.charAt(i * 2 + 1), 16);
            if (hi < 0 || lo < 0) {
                return ProvenResult.err(ProvenStatus.ERR_PARSE_FAILURE, "Invalid hex in UUID");
            }
            bytes[i] = (byte) ((hi << 4) | lo);
        }
        return ProvenResult.ok(bytes);
    }

    private static String toStringPure(byte[] uuid) {
        StringBuilder sb = new StringBuilder(UUID_STRING_LEN);
        for (int i = 0; i < UUID_BYTES_LEN; i++) {
            if (i == 4 || i == 6 || i == 8 || i == 10) {
                sb.append('-');
            }
            sb.append(String.format("%02x", uuid[i] & 0xFF));
        }
        return sb.toString();
    }

    private static byte[] parseUuidBytes(String uuidStr) {
        try {
            return parsePure(uuidStr).unwrap();
        } catch (Exception e) {
            return NIL.clone();
        }
    }
}
