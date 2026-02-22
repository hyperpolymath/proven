// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

import com.sun.jna.Memory;

import java.nio.charset.StandardCharsets;
import java.util.Optional;

/**
 * Safe URL encoding/decoding via libproven FFI.
 *
 * All logic delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No URL encoding logic is reimplemented in Java.
 */
public final class SafeUrl {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private SafeUrl() {}

    /**
     * URL-encode a string (percent-encoding).
     *
     * @param input the string to encode
     * @return the URL-encoded string, or empty on error
     */
    public static Optional<String> encode(String input) {
        Memory mem = NativeUtil.toNativeString(input);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = input.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractString(
            LIB.proven_http_url_encode(mem, bytes.length));
    }

    /**
     * URL-decode a string (percent-decoding).
     *
     * @param input the URL-encoded string to decode
     * @return the decoded string, or empty on error
     */
    public static Optional<String> decode(String input) {
        Memory mem = NativeUtil.toNativeString(input);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = input.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractString(
            LIB.proven_http_url_decode(mem, bytes.length));
    }
}
