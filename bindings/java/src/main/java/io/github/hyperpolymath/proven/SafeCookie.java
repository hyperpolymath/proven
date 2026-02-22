// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

import com.sun.jna.Memory;

import java.nio.charset.StandardCharsets;
import java.util.Optional;

/**
 * Safe HTTP cookie operations via libproven FFI.
 *
 * Provides cookie injection detection, name/value validation, and prefix detection.
 * All logic delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No cookie logic is reimplemented in Java.
 */
public final class SafeCookie {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private SafeCookie() {}

    /**
     * Check if a cookie value contains injection sequences.
     *
     * @param value the cookie value to check
     * @return true if injection detected, empty on null pointer error
     */
    public static Optional<Boolean> hasInjection(String value) {
        Memory mem = NativeUtil.toNativeString(value);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = value.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractBool(
            LIB.proven_cookie_has_injection(mem, bytes.length));
    }

    /**
     * Validate a cookie name.
     *
     * @param name the cookie name to validate
     * @return true if valid, empty on null pointer error
     */
    public static Optional<Boolean> validateName(String name) {
        Memory mem = NativeUtil.toNativeString(name);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = name.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractBool(
            LIB.proven_cookie_validate_name(mem, bytes.length));
    }

    /**
     * Validate a cookie value.
     *
     * @param value the cookie value to validate
     * @return true if valid, empty on null pointer error
     */
    public static Optional<Boolean> validateValue(String value) {
        Memory mem = NativeUtil.toNativeString(value);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = value.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractBool(
            LIB.proven_cookie_validate_value(mem, bytes.length));
    }

    /**
     * Detect cookie name prefix (__Secure- or __Host-).
     *
     * @param name the cookie name
     * @return the prefix type as integer code, or empty on error
     */
    public static Optional<Long> getPrefix(String name) {
        Memory mem = NativeUtil.toNativeString(name);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = name.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractLong(
            LIB.proven_cookie_get_prefix(mem, bytes.length));
    }
}
