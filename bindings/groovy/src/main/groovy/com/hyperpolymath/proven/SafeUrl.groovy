// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeUrl - JNA wrapper for proven_http_url_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import com.sun.jna.Memory
import groovy.transform.CompileStatic

import java.nio.charset.StandardCharsets
import java.util.Optional

/**
 * Safe URL encoding/decoding via libproven JNA FFI.
 *
 * Every method delegates to the Idris 2 verified implementation.
 * No URL logic is reimplemented in Groovy.
 */
@CompileStatic
class SafeUrl {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /**
     * URL-encode a string (percent-encoding).
     */
    static Optional<String> urlEncode(String input) {
        Memory mem = NativeUtil.toNativeString(input)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = input.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractString(
            LIB.proven_http_url_encode(mem, bytes.length))
    }

    /**
     * URL-decode a percent-encoded string.
     */
    static Optional<String> urlDecode(String input) {
        Memory mem = NativeUtil.toNativeString(input)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = input.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractString(
            LIB.proven_http_url_decode(mem, bytes.length))
    }
}
