// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.proven;

import java.nio.charset.StandardCharsets;

/**
 * Safe string operations with injection prevention.
 * Provides UTF-8 validation and context-aware escaping.
 * Calls native verified code via JNI when available.
 */
public final class SafeString {
    private SafeString() {}

    /**
     * Validate that a byte array contains valid UTF-8.
     */
    public static boolean isValidUtf8(byte[] data) {
        if (data == null) return false;
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativeStringIsValidUtf8(data);
        }
        return isValidUtf8Pure(data);
    }

    /**
     * Escape string for safe HTML insertion.
     */
    public static String escapeHtml(String input) {
        if (input == null) return "";
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeStringEscapeHtml(
                input.getBytes(StandardCharsets.UTF_8), status);
            if (ProvenStatus.fromCode(status[0]).isOk() && result != null) {
                return new String(result, StandardCharsets.UTF_8);
            }
        }
        return escapeHtmlPure(input);
    }

    /**
     * Escape string for safe SQL insertion (parameterized queries preferred).
     */
    public static String escapeSql(String input) {
        if (input == null) return "";
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeStringEscapeSql(
                input.getBytes(StandardCharsets.UTF_8), status);
            if (ProvenStatus.fromCode(status[0]).isOk() && result != null) {
                return new String(result, StandardCharsets.UTF_8);
            }
        }
        return escapeSqlPure(input);
    }

    /**
     * Escape string for safe URL insertion.
     */
    public static String escapeUrl(String input) {
        if (input == null) return "";
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeStringEscapeUrl(
                input.getBytes(StandardCharsets.UTF_8), status);
            if (ProvenStatus.fromCode(status[0]).isOk() && result != null) {
                return new String(result, StandardCharsets.UTF_8);
            }
        }
        return escapeUrlPure(input);
    }

    /**
     * Escape for JavaScript string context.
     */
    public static String escapeJs(String input) {
        if (input == null) return "";
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeStringEscapeJs(
                input.getBytes(StandardCharsets.UTF_8), status);
            if (ProvenStatus.fromCode(status[0]).isOk() && result != null) {
                return new String(result, StandardCharsets.UTF_8);
            }
        }
        return escapeJsPure(input);
    }

    // Pure Java fallback implementations

    private static boolean isValidUtf8Pure(byte[] data) {
        int i = 0;
        while (i < data.length) {
            int b = data[i] & 0xFF;
            int len;
            if ((b & 0x80) == 0) len = 1;
            else if ((b & 0xE0) == 0xC0) len = 2;
            else if ((b & 0xF0) == 0xE0) len = 3;
            else if ((b & 0xF8) == 0xF0) len = 4;
            else return false;

            if (i + len > data.length) return false;
            for (int j = 1; j < len; j++) {
                if ((data[i + j] & 0xC0) != 0x80) return false;
            }
            i += len;
        }
        return true;
    }

    private static String escapeHtmlPure(String input) {
        StringBuilder sb = new StringBuilder(input.length() * 2);
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            switch (c) {
                case '<' -> sb.append("&lt;");
                case '>' -> sb.append("&gt;");
                case '&' -> sb.append("&amp;");
                case '"' -> sb.append("&quot;");
                case '\'' -> sb.append("&#x27;");
                default -> sb.append(c);
            }
        }
        return sb.toString();
    }

    private static String escapeSqlPure(String input) {
        return input.replace("'", "''").replace("\\", "\\\\");
    }

    private static String escapeUrlPure(String input) {
        StringBuilder sb = new StringBuilder();
        for (byte b : input.getBytes(StandardCharsets.UTF_8)) {
            int i = b & 0xFF;
            if ((i >= 'A' && i <= 'Z') || (i >= 'a' && i <= 'z') ||
                (i >= '0' && i <= '9') || i == '-' || i == '_' || i == '.' || i == '~') {
                sb.append((char) i);
            } else {
                sb.append(String.format("%%%02X", i));
            }
        }
        return sb.toString();
    }

    private static String escapeJsPure(String input) {
        StringBuilder sb = new StringBuilder(input.length() * 2);
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            switch (c) {
                case '\\' -> sb.append("\\\\");
                case '"' -> sb.append("\\\"");
                case '\'' -> sb.append("\\'");
                case '\n' -> sb.append("\\n");
                case '\r' -> sb.append("\\r");
                case '\t' -> sb.append("\\t");
                case '<' -> sb.append("\\u003c");
                case '>' -> sb.append("\\u003e");
                case '&' -> sb.append("\\u0026");
                default -> sb.append(c);
            }
        }
        return sb.toString();
    }
}
