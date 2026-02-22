// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

import com.sun.jna.Memory;

import java.nio.charset.StandardCharsets;
import java.util.Optional;

/**
 * Safe HTTP header operations via libproven FFI.
 *
 * Provides CRLF injection detection, header name validation,
 * dangerous header detection, header rendering, and HSTS building.
 * All logic delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No header logic is reimplemented in Java.
 */
public final class SafeHeader {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private SafeHeader() {}

    /**
     * Check if a header value contains CRLF injection sequences.
     *
     * @param value the header value to check
     * @return true if CRLF injection detected, empty on null pointer error
     */
    public static Optional<Boolean> hasCrlf(String value) {
        Memory mem = NativeUtil.toNativeString(value);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = value.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractBool(
            LIB.proven_header_has_crlf(mem, bytes.length));
    }

    /**
     * Check if a header name is valid per HTTP spec.
     *
     * @param name the header name to validate
     * @return true if valid, empty on null pointer error
     */
    public static Optional<Boolean> isValidName(String name) {
        Memory mem = NativeUtil.toNativeString(name);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = name.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractBool(
            LIB.proven_header_is_valid_name(mem, bytes.length));
    }

    /**
     * Check if a header name is security-dangerous (e.g. X-Forwarded-For
     * from untrusted sources).
     *
     * @param name the header name to check
     * @return true if dangerous, empty on null pointer error
     */
    public static Optional<Boolean> isDangerous(String name) {
        Memory mem = NativeUtil.toNativeString(name);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = name.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractBool(
            LIB.proven_header_is_dangerous(mem, bytes.length));
    }

    /**
     * Render a header name-value pair as a proper HTTP header line.
     *
     * @param name  the header name
     * @param value the header value
     * @return the rendered header line, or empty on error
     */
    public static Optional<String> render(String name, String value) {
        if (name == null || value == null) {
            return Optional.empty();
        }
        byte[] nameBytes = name.getBytes(StandardCharsets.UTF_8);
        byte[] valueBytes = value.getBytes(StandardCharsets.UTF_8);
        Memory nameMem = NativeUtil.toNativeBytes(nameBytes);
        Memory valueMem = NativeUtil.toNativeBytes(valueBytes);
        if (nameMem == null || valueMem == null) {
            return Optional.empty();
        }
        return NativeUtil.extractString(
            LIB.proven_header_render(nameMem, nameBytes.length, valueMem, valueBytes.length));
    }

    /**
     * Build an HSTS (HTTP Strict Transport Security) header value.
     *
     * @param maxAge            max-age in seconds
     * @param includeSubdomains whether to include subdomains
     * @param preload           whether to include preload directive
     * @return the HSTS header value, or empty on error
     */
    public static Optional<String> buildHsts(long maxAge, boolean includeSubdomains, boolean preload) {
        return NativeUtil.extractString(
            LIB.proven_header_build_hsts(maxAge, includeSubdomains, preload));
    }
}
