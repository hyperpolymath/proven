// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

import com.sun.jna.Memory;

import java.nio.charset.StandardCharsets;
import java.util.Optional;

/**
 * Safe string operations via libproven FFI.
 *
 * Provides UTF-8 validation and context-aware escaping (HTML, SQL, JS).
 * All logic delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No escaping logic is reimplemented in Java.
 */
public final class SafeString {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private SafeString() {}

    /**
     * Validate that a byte array contains valid UTF-8.
     *
     * @param data the bytes to validate
     * @return true if the bytes are valid UTF-8, empty if null pointer error
     */
    public static Optional<Boolean> isValidUtf8(byte[] data) {
        Memory mem = NativeUtil.toNativeBytes(data);
        if (mem == null) {
            return Optional.empty();
        }
        return NativeUtil.extractBool(
            LIB.proven_string_is_valid_utf8(mem, data.length));
    }

    /**
     * Escape a string for safe HTML insertion.
     *
     * @param input the string to escape
     * @return the HTML-escaped string, or empty on error
     */
    public static Optional<String> escapeHtml(String input) {
        Memory mem = NativeUtil.toNativeString(input);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = input.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractString(
            LIB.proven_string_escape_html(mem, bytes.length));
    }

    /**
     * Escape a string for safe SQL insertion.
     * Note: parameterized queries are always preferred over escaping.
     *
     * @param input the string to escape
     * @return the SQL-escaped string, or empty on error
     */
    public static Optional<String> escapeSql(String input) {
        Memory mem = NativeUtil.toNativeString(input);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = input.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractString(
            LIB.proven_string_escape_sql(mem, bytes.length));
    }

    /**
     * Escape a string for safe JavaScript string context.
     *
     * @param input the string to escape
     * @return the JS-escaped string, or empty on error
     */
    public static Optional<String> escapeJs(String input) {
        Memory mem = NativeUtil.toNativeString(input);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = input.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractString(
            LIB.proven_string_escape_js(mem, bytes.length));
    }
}
